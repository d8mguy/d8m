// Compilation and matching of patterns and interpn of xprftns.

package main

import (
	"fmt"
	"math"
	"strings"
)

// The following definitions create a facility for addressing subterms, including access and update.
// The elements of a Codept are a mix of TermAddrLabels and indices.
// A Codept represents one or more steps from a specific term, specific because each step tells what
// kind of concrete term it is (such as *TermTL) and then where to go (such as to the args[3] element).
// Codepts are used in compiledPatterns and likely, in other ways in future code.
type Codept []uint8

type TermAddrLabel uint8
type TAL = TermAddrLabel // nicer for cnvns

const (
	NoTAL TermAddrLabel = iota
	// first set of codes identify non-leaf concrete term types
	talT
	talTT0
	talTT1
	// talL and talTLa codes are followed by an index between 0 and 254; 255 acts as overflow code, followed by 4 bytes
	talL
	talTLt
	talTLa
	talSym
	talFuni
)

// tal encodes what kind of term trm is and what to do with it
// Any type assertion failure is a compiler error
func (tal TermAddrLabel) moveTo(trm Term, ofs int) (ret Term) {
	switch tal {
	case talT:
		ret = trm.(*TermT).arg0
	case talTT0:
		ret = trm.(*TermTT).arg0
	case talTT1:
		ret = trm.(*TermTT).arg1
	case talL:
		ret = trm.(*TermL).args[ofs]
	case talTLt:
		ret = trm.(*TermTL).trm
	case talTLa:
		ret = trm.(*TermTL).args[ofs]
	}
	return
}

// little functions to build Codept structures
// Note: many of these aren't used and may need to be deleted
func (cdpt *Codept) addT() {
	*cdpt = append(*cdpt, uint8(talT))
}

func (cdpt *Codept) addTT0() {
	*cdpt = append(*cdpt, uint8(talTT0))
}

func (cdpt *Codept) addTT1() {
	*cdpt = append(*cdpt, uint8(talTT1))
}

func (cdpt *Codept) addIndex(inx int) {
	if inx < 255 {
		*cdpt = append(*cdpt, uint8(inx))
	} else {
		*cdpt = append(*cdpt, uint8(255), uint8(inx&0xff), uint8((inx>>8)&0xff), uint8((inx>>16)&0xff), uint8((inx>>24)&0xff))
	}
}

func (cdpt *Codept) addL(inx int) {
	*cdpt = append(*cdpt, uint8(talL))
	cdpt.addIndex(inx)
}

func (cdpt *Codept) addTL0() {
	*cdpt = append(*cdpt, uint8(talTLt))
}

func (cdpt *Codept) addTLs(inx int) {
	*cdpt = append(*cdpt, uint8(talTLa))
	cdpt.addIndex(inx)
}

func (cdpt Codept) getCodeptIndex(i int) (newinx int, val int) {
	val = int(cdpt[i])
	i++
	newinx = i
	if val == 255 {
		newinx += 4
		val = int(cdpt[newinx-3]) | (int(cdpt[newinx-2]) << 8) | (int(cdpt[newinx-1]) << 16) | (int(cdpt[newinx]) << 24)
	}
	return
}

// A function to "walk" a term directed by cdpt and return the subterm it finds.
// Will panic if trm isn't compatible with cdpt, or if cdpt is constructed wrongly.
func walkCodept(trm Term, cdpt Codept) Term {
	curtrm := trm
	inx := 0
	incr := 0
	for i := 0; i < len(cdpt); {
		cp := TAL(cdpt[i])
		i++
		if cp == talL || cp == talTLa {
			incr, inx = cdpt.getCodeptIndex(i)
			i += incr
		}
		curtrm = TAL(cp).moveTo(curtrm, inx)
	}
	return curtrm
}

// Tentative defn of a new approach to compiled patterns
type Pattern struct {
	tag   Termtag
	parts []*Pattern
	// the following slots are used selectively based on the value of tag (above)
	flexcount int // <0 means disjunctive; Symboltag && != 0 means TEM; else Termtag in [Stmts, Litform0, Funcall] => #flex elts
	aTerm     Term
	other     interface{}
}

func makePattern(tg Termtag, parts []*Pattern, fc int, trm Term, other interface{}) *Pattern {
	return &Pattern{tg, parts, fc, trm, other}
}

// This is the type of elements of the slice the compiler passes for lists with flex matchables in them
type flexPtnElt struct {
	mbl *Symbol // set if flex matchable
	tag Termtag // set if fixed ptn (else will be NoTerm)
}

// In stmt patterns, xprstmts elts are flexible; similarly xprterms in expr lists. For such patterns, the compiler creates
// a []flexPtnElt list that intersperses xprstmts matchables (say) with specific tags. On any given term (eg stmtlist) this
// creates a set of possible matches; calculating this set is an interesting problem with combinatorial aspects. For example,
// given 3 tags and assuming a full set of interspersed elts
type bktkInfo struct {
	taglist          []Termtag // tags of non-flex terms, derived from compiler's flexinfo
	possibles        [][]int   // len(possibles) == len(taglist); each tells what indices terms match corresp tag
	possiblesIndices []int     // enum state of possibles
	indices          []int     // current indices into actual term; updated by nextAssignment
	activeIndex      int
	fpi              []flexPtnElt // records how pattern looked before compilation
	// next 3 record the state when this bktkInfo is created, for restoral
	ptn       *Pattern // the pattern
	trm       Term     // the ground term
	bindIndex int      // index in PtnRuntime's bindings list
}

// Called when we have a pattern with flex elements. Such a pattern always applies to a list, either a stmt list or
// expr list. Here, we create a bktkInfo structure that figures out at what indices in the actual list (trmargs) different
// fixed tags in the pattern could be. If there are no possible solutions in trmargs, it'll figure that out.
func makeBktkInfo(trmargs []Term, flexinfo []flexPtnElt) *bktkInfo {
	// First, collect the fixed tags in flexinfo
	tags := make([]Termtag, 0, len(flexinfo)/2)
	for _, fpi := range flexinfo {
		if fpi.tag != NoTerm {
			tags = append(tags, fpi.tag)
		}
	}
	ntags := len(tags)
	indices := make([]int, ntags)
	poss := make([][]int, ntags)
	possi := make([]int, ntags)
	nargs := len(trmargs)
	findingfirst := 0
	// A single pass through the terms fills out all the poss slices, and the use of findingfirst enforces the cntt
	// that L[k+1].head > L[k].head.
	for trminx := 0; trminx < nargs; trminx++ {
		trmtg := trmargs[trminx].Tag()
		for inx := 0; inx <= findingfirst; inx++ {
			if tags[inx] == trmtg {
				poss[inx] = append(poss[inx], trminx)
				if inx == findingfirst && findingfirst < ntags-1 {
					findingfirst++
					inx++
				}
			}
		}
	}
	// At this point, if findingfirst < ntags-1, there are no solns
	if findingfirst < ntags-1 {
		return nil
	}
	// Well, maybe there are solns; next, enforce the cntt that L[k].last < L[k+1].last by selectively deleting indices
	curlastinx := ntags - 1
	lastlastslice := poss[curlastinx]
	lastlast := lastlastslice[len(lastlastslice)-1]
	for curlastinx--; curlastinx >= 0; curlastinx-- {
		curlastslice := poss[curlastinx]
		curlast := curlastslice[len(curlastslice)-1]
		for curlast >= lastlast {
			curlastslice = curlastslice[:len(curlastslice)-1]
			if len(curlastslice) == 0 {
				return nil
			}
			curlast = curlastslice[len(curlastslice)-1]
		}
	}
	// Ok, at this point we have at least one path, so set it up and create the bktkInfo item.
	for i := 0; i < ntags; i++ {
		indices[i] = poss[i][0]
		possi[i] = 0
	}
	//fmt.Println("mkbktkinfo starts with ", indices)
	return &bktkInfo{tags, poss, possi, indices, 0, flexinfo, nil, nil, 0}
}

// From the current state of bti, look for another set of indices that satisfies taglist. Do this
// highest index first. So first, we look for higher indices with just the last elt. If that fails, we look
// on successively lower indices of taglist and if found, completing the set from 1+ the index found for the
// lower. If nothing pans out, return false, else true with the state changed. It's ok to mod in place since
// if it doesn't work out, this entity won't be used anymore.

func (bti *bktkInfo) nextAssignment(ptnrt *PtnRuntime) bool {
	//fmt.Println("nextAssignment, indices before ", bti.indices)
	ntags := len(bti.taglist)
	// increment possi repeatedly until we run out
	for {
		found := false
		for inx := ntags - 1; inx >= 0; inx-- {
			bti.possiblesIndices[inx]++
			if bti.possiblesIndices[inx] >= len(bti.possibles[inx]) {
				bti.possiblesIndices[inx] = 0 // this is likely wrong but the monotone checking below will fix it
			} else {
				found = true
				break
			}
		}
		if !found {
			return false // no more possibilities
		}
		last := -1
		monotone := true
		for inx := 0; inx < ntags; inx++ {
			next := bti.possibles[inx][bti.possiblesIndices[inx]]
			if next > last {
				last = next
			} else {
				monotone = false
				break
			}
			bti.indices[inx] = next
		}
		if monotone {
			//fmt.Println("nextAssignment trying ", bti.indices)
			bti.activeIndex = 0
			if ptnrt.assignFlex(bti.ptn, bti) {
				//fmt.Println("nextAssignment satisifed with ", bti.indices)
				return true
			}
		}
	}
	//fmt.Println("nextAssignment returning false")
	return false
}

// State info for pattern matching, both the one term variety and the "search for subterms" one.
// The matchinfos and stopOnFound slots are only used for searching.
type PtnRuntime struct {
	bktks    []*bktkInfo // used for backtracking
	bindings []*Symbol   // TEMs and PEMs bound along the way
	// next slots only matter in psearch, not pmatch
	matchinfos  [][]Term // only in matches, matchcount
	stopOnFound bool     // only in contains
	counter     int      // set to 0 to count else -1, don't care when stopOnFound is true
}

func makePtnRT(ctr int) *PtnRuntime {
	return &PtnRuntime{nil, nil, nil, false, ctr}
}

// Called to bind a matchable. Type compat was already checked.
func (ptnrt *PtnRuntime) bindSymbol(sym *Symbol, val Term) {
	//fmt.Println("binding", sym.ident, " to ", val.String())
	sym.binding = val
	for _, b := range ptnrt.bindings {
		if b == sym {
			return
		}
	}
	ptnrt.bindings = append(ptnrt.bindings, sym)
}

// When a PEM succeeds, we need to merge its bindings into the "parent" ptnrt
func (ptnrt *PtnRuntime) mergeBindings(optnrt *PtnRuntime) {
	ptnrt.bindings = append(ptnrt.bindings, optnrt.bindings...)
}

func (ptnrt *PtnRuntime) clearMatchablesTo(inx int) {
	//fmt.Println("clrM to ", inx, "; nbdgs=", len(ptnrt.bindings))
	for i := inx; i < len(ptnrt.bindings); i++ {
		sym := ptnrt.bindings[i]
		seen := make([]*Type, 0, 2)
		sym.dtype.clearGTS(&seen)
		sym.binding = nil
	}
	ptnrt.bindings = ptnrt.bindings[:inx]
}

// Given a plausible assignment of indices in the list component of trm, return whether all the concrete parts match.
func (ptnrt *PtnRuntime) assignFlex(ptn *Pattern, bti *bktkInfo) bool {
	lim := len(bti.taglist)
	var args []Term
	if bti.trm.Tag() == Funcall { // flex exprlist
		args = bti.trm.(*TermTL).args
	} else {
		args = bti.trm.(*TermL).args // flex stmtlist
	}
	//fmt.Println("assignFlex: ", bti.indices, "bdgs b4:", len(ptnrt.bindings))
	bb4 := len(ptnrt.bindings)
	for ; bti.activeIndex < lim; bti.activeIndex++ {
		if !ptnrt.pmatch(args[bti.indices[bti.activeIndex]], ptn.parts[bti.activeIndex]) {
			//fmt.Println("bailing from assignFlex with bdgs=", len(ptnrt.bindings))
			ptnrt.clearMatchablesTo(bb4)
			return false
		}
	}
	// we've found a match; bind the flex matchables which are at all the flexElts in the pattern and are
	// known not to occur in pairs. We don't actually know at this point how many flex matchables there are
	// but it can't be more than 1 more than the number of fixed patterns. We have indices for each of the
	// fixed patterns, so if there's a gap in the pattern, there's a flex there.
	finx := 0
	if bti.fpi[0].mbl != nil {
		// here, initial ptn elt is a flex matchable
		fmterm := makeScopedTermL(Stmts, makeScope(), args[:bti.indices[0]], TypeNothing, Pos(-1), Pos(-1))
		bti.fpi[0].mbl.binding = fmterm
		finx = 1
	}
	// at this point, we can assume that any flex matchable comes after a fixed elt that's at or later than the index into indices
	for i := 0; i < len(bti.indices); {
		if finx == len(bti.fpi) {
			break
		}
		ptnelt := bti.fpi[finx]
		if ptnelt.mbl == nil {
			finx++
			continue
		}
		// here, bti.fpi[finx-1] is fixed and bti.fpi[finx] is flex
		inx0 := bti.indices[i]
		i++
		inxn := len(args)
		if i < len(bti.indices) {
			inxn = bti.indices[i]
		}
		fmterm := makeScopedTermL(Stmts, makeScope(), args[inx0+1:inxn], TypeNothing, Pos(-1), Pos(-1))
		bti.fpi[finx].mbl.binding = fmterm
		finx++
	}
	return true
}

// Should be called only if ptnrt.bktks is non-empty. First, clear the matchable bindings back to the point given by
// ptnrt.bktks.last, then try to find another match. If so, return ptn and trm, else pop bktks and try again.
// If no luck, return nils.
func (ptnrt *PtnRuntime) resume() (ptn *Pattern, trm Term) {
	//fmt.Println("resume: ")
	// each pass through the outer loop strips one level of backtracking; usually, there's at most one
	for len(ptnrt.bktks) > 0 {
		bti := ptnrt.bktks[len(ptnrt.bktks)-1]
		ptnrt.clearMatchablesTo(bti.bindIndex)
		if bti.nextAssignment(ptnrt) || len(ptnrt.bktks) == 0 {
			ptn = bti.ptn
			trm = bti.trm
			break
		}
		ptnrt.bktks = ptnrt.bktks[:len(ptnrt.bktks)-1]
	}
	return
}

// Like resume, called only if there's a backtrack item. Whenever it returns true, a pattern has been matched.
func (ptnrt *PtnRuntime) resumeLoop() bool {
	//fmt.Println("resumeLoop:")
	bti := ptnrt.bktks[len(ptnrt.bktks)-1]
	//defer fmt.Println("leaving resumeLoop")
	for {
		ptn, _ := ptnrt.resume()
		if len(ptnrt.bktks) == 0 {
			return false
		}
		if ptnrt.assignFlex(ptn, bti) {
			return true
		}
	}
}

// This is the searching version of pmatch.
func (ptnrt *PtnRuntime) psearch(trm Term, ptn *Pattern) (matched Term) {
	var recordMatch = func() {
		if ptnrt.counter < 0 {
			nbound := len(ptnrt.bindings)
			minfo := make([]Term, nbound+1)
			for i, s := range ptnrt.bindings {
				minfo[i] = s.binding
				s.binding = nil
			}
			minfo[nbound] = makeSymbol("term", Gzterm, trm)
			ptnrt.matchinfos = append(ptnrt.matchinfos, minfo)
		} else {
			ptnrt.counter++
		}
	}
	// 1. is there a match here?
	found := ptnrt.pmatch(trm, ptn)
	if found {
		if ptnrt.stopOnFound {
			return trm
		}
		recordMatch()
		// todo: work out how to clear relevant bindings
	}
	// 2. if backtracking is active, look for more cases on this term
	for len(ptnrt.bktks) > 0 {
		if ptnrt.pmatch(trm, ptn) {
			recordMatch()
		}
	}
	// 3. recurse into subterms.
	switch trm0 := trm.(type) {
	case *Funinst:
		if trm0.body != nil {
			mt := ptnrt.psearch(trm0.body, ptn)
			if mt != nil && ptnrt.stopOnFound {
				return mt
			}
		}
	case *TermT:
		if trm0.arg0 != nil {
			mt := ptnrt.psearch(trm0.arg0, ptn)
			if mt != nil && ptnrt.stopOnFound {
				return mt
			}
		}
	case *TermTT:
		mt := ptnrt.psearch(trm0.arg0, ptn)
		if mt != nil && ptnrt.stopOnFound {
			return mt
		}
		mt = ptnrt.psearch(trm0.arg1, ptn)
		if mt != nil && ptnrt.stopOnFound {
			return mt
		}
	case *TermL:
		for _, a := range trm0.args {
			mt := ptnrt.psearch(a, ptn)
			if mt != nil && ptnrt.stopOnFound {
				return mt
			}
		}
	case *TermTL:
		mt := ptnrt.psearch(trm0.trm, ptn)
		if mt != nil && ptnrt.stopOnFound {
			return mt
		}
		for _, a := range trm0.args {
			mt = ptnrt.psearch(a, ptn)
			if mt != nil && ptnrt.stopOnFound {
				return mt
			}
		}
	}
	return nil
}

func printMatchables(msg string, symx SymXDict) {
	bdgs := []string{}
	for _, sx := range symx {
		mtchble := itemExists(sx.sym, "TEM") || itemExists(sx.sym, "PEM")
		if mtchble {
			bdgstrg := sx.sym.ident + " is <nil>"
			if sx.sym.binding != nil {
				rhs := sx.sym.binding.String()
				if sx.sym.binding.Tag() == Stmts && strings.Count(rhs, "\n") > 6 {
					stmtargs := sx.sym.binding.(*TermL).args
					rhs = fmt.Sprintf("...%d stmts/%d lines...first:%s ***last: %s ***done", len(stmtargs), strings.Count(rhs, "\n"),
						stmtargs[0].String(), stmtargs[len(stmtargs)-1].String())
				}
				bdgstrg = sx.sym.ident + " = " + rhs
			}
			bdgs = append(bdgs, bdgstrg)
		}
	}
	logger.Println(msg, strings.Join(bdgs, "; "))
}

// Attempt to apply a specific rwrule to a term. Make a runtime DS for
// matching (ptnrt) and call its pmatch method. If that succeeds, apply the rwrule, which can
// succeed or fail. If it succeeds, return the modified term else check whether there are more
// possibilities for matching.
func ptnMatch(trm Term, ruleinfo *rwruleInfo, smi *STMapInfo) (Term, bool) {
	ptnrt := makePtnRT(0)
	var rulename = ""
	if prRewrite {
		rulename = ruleinfo.rule.plist.Find("xprname").(string)
		if rulename == "rwSum2MR" {
			fmt.Print()
		}
	}
	matched := ptnrt.pmatch(trm, ruleinfo.mqp)
	if matched {
		if prRewrite && rulename == "rwEachPll" {
			logger.Println("matched MQ on ", rulename)
			printMatchables("matchables: ", ruleinfo.rule.body.scope.entries)
			if rulename == "rwSum2MR" {
				fmt.Print()
			}
		}
		xpi := makeXPRInfo(smi)
		for {
			tmp0, changed := xpi.interpXprftn(trm, ruleinfo.rule)
			if prRewrite {
				rslt := "fails"
				if changed && tmp0.Tag() != ErrorTag {
					rslt = "succeeds"
				}
				logger.Println(rulename, rslt)
			}
			if tmp0.Tag() == ErrorTag {
				fmt.Println("error in rewrite rule", rulename, ":", tmp0.String())
				// for now, print an error but don't propagate it
				changed = false
			}
			if prRewrite && changed {
				logger.Println("result: ", tmp0.String())
				logger.Println("********")
			}
			if changed {
				return Simplify(tmp0), true
			}
			if len(ptnrt.bktks) > 0 {
				if !ptnrt.resumeLoop() {
					break
				}
				/*if prRewrite {
					printMatchables("after backtrack:", ruleinfo.rule.body.scope.entries)
				}*/
			} else {
				break
			}
		}
	}
	// clear any generics in the given scope, and all matchables
	ruleinfo.clear()
	return trm, false
}

// Recursive parts explore trm and ptn in pll.
func (ptnrt *PtnRuntime) pmatch(trm Term, ptn *Pattern) (retval bool) {
	// This loop runs until backtracking options of the pattern are exhausted
	// or a match is found. If no backtracking, it'll run once.
	for {
		if ptn.tag == SymbolTag { // can be TEM, PEM, or const
			mbl := ptn.aTerm.(*Symbol)
			if mbl.ident == "_" {
				return true
			}
			seen := []*Type{}
			if ptn.flexcount != 0 { // codes a TEM
				if mbl.binding != nil && !trm.Equal(mbl.binding) {
					return false
				}
				if !((trm.Tag() == TypeTag && mbl.dtype == Gztype) || (trm.Tag() == Stmts && mbl.dtype == Gzstmts) || trm.Dtype().compat(mbl.dtype, &seen)) {
					return false
				}
				ptnrt.bindSymbol(mbl, trm)
				return true
			} else if len(ptn.parts) > 0 { // codes a PEM
				success := false
				if mbl.dtype == Gzterm || mbl.dtype == Gzstmts || trm.Dtype().compat(mbl.dtype, &seen) {
					matchcall := mbl.plist.Find("PEM")
					if matchcall == nil {
						panic("can't happen")
					}
					searching := matchcall.(*TermTL).args[1]
					binding := matchcall.(*TermTL).args[2]
					if searching == FalseLiteral {
						success = ptnrt.pmatch(trm, ptn.parts[0])
					} else {
						nptnrt := makePtnRT(0)
						nptnrt.stopOnFound = true
						found := nptnrt.psearch(trm, ptn.parts[0])
						success = found != nil
						if success {
							ptnrt.mergeBindings(nptnrt)
						}
					}
					if success {
						ptnrt.bindSymbol(mbl, trm)
					}
					if binding == FalseLiteral || !success {
						ptnrt.clearMatchablesTo(0)
					}
				}
				return success
			} else {
				if trm.Tag() == SymbolTag { // don't worry about the binding
					trm0 := trm.(*Symbol)
					if trm0.ident == mbl.ident {
						if trm0.dtype.family == TFFtn && mbl.dtype.family == TFFtn && mbl.dtype.isGeneric() {
							// special attn to grounding gnc ftns in patterns
							seen := []*Type{}
							retval = trm0.dtype.compat(mbl.dtype, &seen)
							seen = seen[:0]
							mbl.dtype.clearGTS(&seen)
						} else {
							retval = trm0.dtype.compat(mbl.dtype, &seen)
						}
					}
				}
			}
		} else if ptn.tag == SymbolPattern && trm.Tag() == SymbolTag {
			trm0 := trm.(*Symbol)
			// proceed carefully since the ident of a Symbol is not a Term
			idptn := ptn.parts[0]
			identlbl := makeTermB(Labellit, trm0.ident, Pos(-1))
			if idptn.tag == SymbolTag {
				ptnsym := idptn.aTerm.(*Symbol)
				seen := []*Type{}
				if ptnsym.binding == nil && identlbl.Dtype().compat(ptnsym.dtype, &seen) {
					ptnrt.bindSymbol(ptnsym, identlbl)
				} else {
					return false
				}
			} else if !ptnrt.pmatch(identlbl, idptn) {
				return false
			}
			typtn := ptn.parts[1]
			// special check for compat of pattern and term wrt type vs entity binding
			if trm0.dtype == nil && typtn.tag == TypeTag {
				// todo: add check for :type in pattern and entity in term
				return false
			}
			if !ptnrt.pmatch(trm0.dtype, typtn) {
				return false
			}
			// I'm not really sure that mksym with binding omitted is a legit option for this kind of pattern but
			// I'm coding for it as of now.
			if len(ptn.parts) == 3 && !ptnrt.pmatch(trm0.binding, ptn.parts[2]) {
				return false
			}
			retval = true
		} else if ptn.tag == Litform0 && ptn.flexcount < 0 {
			// disjunctive pattern, check each disjunct here
			for _, disjunct := range ptn.parts {
				if ptnrt.pmatch(trm, disjunct) {
					retval = true
					break
				}
			}
		} else if ptn.tag != trm.Tag() {
			retval = false
		} else {
			// handle base literals
			trm0, isLit := trm.(*TermB)
			if isLit {
				return trm0.Equal(ptn.aTerm)
			}
			// handle types
			typ0, isType := trm.(*Type)
			if isType {
				return typ0.Equal(ptn.aTerm)
			}
			// now everything else
			switch ptn.tag { // start with tags that get special treatment; generics after
			case Stmts, Litform0, Funcall:
				var trmargs []Term // stmtlist or exprlist used to establish bktrk info struct
				var ptnargs []*Pattern
				if ptn.tag == Funcall {
					trm1 := trm.(*TermTL)
					trmargs = trm1.args
					ptnargs = ptn.parts[1:]
					if !ptnrt.pmatch(trm1.trm, ptn.parts[0]) {
						return
					}
				} else {
					trmargs = trm.(*TermL).args
					ptnargs = ptn.parts
				}
				if ptn.flexcount == 0 { // no flex elts so match parts in pll
					for i, s := range trmargs {
						if i == len(ptnargs) || !ptnrt.pmatch(s, ptnargs[i]) {
							return false
						}
					}
					retval = true
				} else {
					// the compiler provides a []flexPtnElt to create the backtrack info element
					flexinfo := ptn.other.([]flexPtnElt)
					bti := makeBktkInfo(trmargs, flexinfo)
					if bti == nil {
						return false
					}
					bti.ptn = ptn
					bti.trm = trm
					bti.bindIndex = len(ptnrt.bindings)
					// At this point we've found one assignment of tags; push it onto backtrack stack and call
					// assignFlex to see if they really match. If not, call resume until we find something or
					// it goes away.
					ptnrt.bktks = append(ptnrt.bktks, bti)
					//fmt.Println("entering bktrk loop with ", bti)
					for {
						if ptnrt.assignFlex(ptn, bti) {
							return true
						}
						ptn, trm = ptnrt.resume()
						if len(ptnrt.bktks) == 0 {
							return false
						}
					}
				}
			case IfStmt, Ifcase, EachStmt, AndandExpr, OrorExpr, Symchain: // these are all the remaining TermL tags
				fail := false
				for i, x := range trm.(*TermL).args {
					// last (index decl) arg of Each is optional; fail if in term but not in pattern
					// This causes index declg each to not rewrite but avoids the need for optional matchables in patterns
					if ptn.tag == EachStmt && i == 3 && len(ptn.parts) < 4 {
						fail = true
						break
					}
					if !ptnrt.pmatch(x, ptn.parts[i]) {
						fail = true
						break
					}
				}
				if !fail {
					return true
				}
			case Valdecl, Vardecl, Lognot, Negate, ReturnStmt, BreakStmt, ContinueStmt, LoopStmt, Gdref, Typeterm:
				// these are all the TermT tags; optional or omitted args have nothing in parts
				if len(ptn.parts) == 0 {
					return true
				}
				retval = ptnrt.pmatch(trm.(*TermT).arg0, ptn.parts[0])
				if retval && ptn.tag == Typeterm {
					seen := []*Type{}
					retval = trm.Dtype().compat(ptn.parts[1].aTerm.(*Type), &seen)
				}
			case EqExpr, EqeqExpr, AsgnStmt: // these are all the TermTT tags
				trm0 := trm.(*TermTT)
				retval = ptnrt.pmatch(trm0.arg0, ptn.parts[0]) && ptnrt.pmatch(trm0.arg1, ptn.parts[1])
			case FuninstTag:
				trm0 := trm.(*Funinst)
				retval = ptnrt.pmatch(trm0.body, ptn.parts[0])
			default:
				panic("not reached")
			}
		}
		// Note: the previous code, preserved in the next line, is definitely buggy; I'm not 100% sure the new line is the right fix
		//if retval || len(ptnrt.bktks) == 0 {
		if retval || ptn.flexcount == 0 {
			break
		}
		// We get here only if ptn and term involve lists with flex elts
		ptn, trm = ptnrt.resume()
		if ptn == nil {
			break
		}
		// resume succeeded so go around for another pass
	}
	return
}

type ptnCompInfo struct {
	xprsyms    SymXDict // all the symbols decld in the xprftn: matchables and locals
	errors     []Term
	matchables []*Symbol
	mqseen     bool // locals are treated differently before and after the matchquery
}

func (state *ptnCompInfo) addMatchable(m *Symbol) {
	for _, m0 := range state.matchables {
		if m == m0 {
			return
		}
	}
	state.matchables = append(state.matchables, m)
}

func (state *ptnCompInfo) addError(msg string, trm Term) {
	state.errors = append(state.errors, genError(msg, trm))
}

// true if trm is a matchable of type xprstmts or xprterms
func flexElt(trm Term) bool {
	if trm.Tag() == SymbolTag {
		sym := trm.(*Symbol)
		if sym.binding == nil && (sym.dtype == Gzstmts || sym.dtype == Gztermlist) {
			return true
		}
	}
	return false
}

// This is the entry point for pattern compilation. Given the pattern Term and a (possibly identical)
// Term that the compiled pattern is to be attached to, create a ptnCompInfo entity (using the syms
// argument) and call the real pattern compiler on ptn. If it returns an error, return that else
// attach the compiled pattern to the attach Term (using key "CP" on plist) and return it.
// The syms give the local scope and matchables; mqseen tells whether to compile this pattern for before
// the matchquery or after. (It has to do with whether the binding of locals is treated as runtime info or not.)
func compilePattern(ptn, attach Term, syms SymXDict, synthMatchinfo bool, mqseen bool) Term {
	pcstate := &ptnCompInfo{syms, nil, nil, mqseen}
	xpi := makeXPRInfo(nil)
	xpi.setup(syms)
	xpi.precompile = true
	ptn0 := interpXprExpr(ptn, xpi)
	// Detect disjunctive patterns. It's not any list-valued pattern term because maybe we're not
	// intending to interpret that term. This could be a design problem, where I need some kind of
	// quotation to make it unambiguous. For now, I'm treating any list-valued litform as a disjunctive
	// pattern and anything else as not disjunctive.
	var cmpdptn *Pattern
	if ptn0.Tag() == Litform0 {
		ptn1 := ptn0.(*TermL)
		parts := make([]*Pattern, len(ptn1.args))
		for i, pp := range ptn1.args {
			parts[i] = ptnCompiler(pp, pcstate)
			if parts[i] == nil {
				return pcstate.errors[0]
			}
		}
		// todo: check restrns on matchables in disjunctive patterns
		cmpdptn = makePattern(Litform0, parts, -1, nil, nil)
	}
	if cmpdptn == nil {
		cmpdptn = ptnCompiler(ptn0, pcstate)
	}
	if cmpdptn == nil {
		return pcstate.errors[0]
	}
	attach.Plist().Add("CP", cmpdptn)
	if synthMatchinfo {
		// synth the matchinfo type a/w this pattern
		nmtch := len(pcstate.matchables)
		miAttribs := make([]*Symbol, nmtch+1)
		copy(miAttribs, pcstate.matchables)
		miAttribs[nmtch] = makeSymbol("self", Gzterm, nil)
		// cheat: in this case, attach is always a funcall
		attach.(*TermTL).dtype = makeListType(makeType(TFTuple, miAttribs, false))
	}
	return attach
}

// This is the recursion point for pattern compilation.
// On error, add error to state and return nil.
func ptnCompiler(trm Term, state *ptnCompInfo) *Pattern {
	if trm == nil {
		return nil
	}
	switch trm0 := trm.(type) {
	case *Symbol:
		sym := state.xprsyms.Find(trm0.ident)
		if sym == nil || trm0.ident == "_" {
			// definitely a const ("_" is specially treated in pmatch)
			return makePattern(SymbolTag, nil, 0, trm0, nil)
		}
		// otherwise, sym is a local. It could be a constructed symbol, TEM, or PEM
		if sym.binding == nil {
			// definitely a TEM
			state.addMatchable(trm0)
			trm0.plist.Add("TEM", true)
			return makePattern(SymbolTag, nil, 1, trm0, nil)
		}
		if isPEM(sym) {
			ptncall := sym.binding.(*TermTL)
			subptn0 := ptncall.plist.Find("CP")
			if subptn0 == nil {
				return nil
			}
			// set trm0 up as a matchable post-TC: tagged as PEM with binding cleared
			trm0.plist.Add("PEM", ptncall)
			trm0.binding = nil
			subptn := subptn0.(*Pattern)
			return makePattern(SymbolTag, []*Pattern{subptn}, 0, trm0, nil)
		}
		// we have a local non-matchable; compile the binding if a pre-MQ local, else compile as a "runtime matchable"
		if state.mqseen {
			return makePattern(SymbolTag, nil, 1, trm0, nil)
		} else {
			return ptnCompiler(sym.binding, state)
		}
	case *TermB:
		return makePattern(trm0.kind, nil, 0, trm0, nil)
	case *Type:
		return makePattern(TypeTag, nil, 0, trm0, nil)
	case *Funinst:
		var parts []*Pattern
		if trm0.body != nil {
			// The typechecker creaes Funinsts that wrap exprs for chainops; in pattern cxt these can obscure
			// matchables meant to match the Funinst. I'm not sure this is 100% correct but I am making the simplification
			// of lifting all such Funinsts that occur in pattern cxt.
			if len(trm0.body.args) == 1 && trm0.body.args[0].Tag() == SymbolTag && isTEM(trm0.body.args[0].(*Symbol), state.xprsyms) {
				return ptnCompiler(trm0.body.args[0], state)
			}
			parts = []*Pattern{ptnCompiler(trm0.body, state)}
		}
		return makePattern(FuninstTag, parts, 0, nil, nil)
	case *TermT:
		if trm0.arg0 == nil {
			return makePattern(trm0.kind, nil, 0, nil, nil)
		}
		argptn := ptnCompiler(trm0.arg0, state)
		if argptn == nil {
			return nil
		}
		if trm0.kind == Typeterm {
			ttptn := ptnCompiler(trm0.dtype, state)
			if ttptn == nil {
				return nil
			}
			return makePattern(trm0.kind, []*Pattern{argptn, ttptn}, 0, nil, nil)
		}
		return makePattern(trm0.kind, []*Pattern{argptn}, 0, nil, nil)
	case *TermTT:
		p0 := ptnCompiler(trm0.arg0, state)
		p1 := ptnCompiler(trm0.arg1, state)
		if p0 == nil || p1 == nil {
			return nil
		}
		parts := []*Pattern{p0, p1}
		return makePattern(trm0.kind, parts, 0, nil, nil)
	case *TermL:
		if trm0.kind == Stmts || trm0.kind == Litform0 {
			if len(trm0.args) == 1 && trm0.args[0].Tag() == SymbolTag {
				// a stmtlist or xprlist matched by a single TEM or PEM needs to be lifted out of the list cxt
				sym := trm0.args[0].(*Symbol)
				if isTEM(sym, state.xprsyms) || isPEM(sym) {
					return ptnCompiler(sym, state)
				}
			}
			// check for flex elts
			nflex := 0
			for _, t := range trm0.args {
				if flexElt(t) {
					nflex++
				}
			}
			if nflex > 0 {
				nfix := len(trm0.args) - nflex
				flexinfo := make([]flexPtnElt, len(trm0.args))
				parts := make([]*Pattern, nfix)
				pinx := 0
				for i, t := range trm0.args {
					if flexElt(t) {
						tsym := t.(*Symbol)
						tsym.plist.Add("TEM", true)
						flexinfo[i].mbl = tsym
					} else {
						flexinfo[i].tag = t.Tag()
						parts[pinx] = ptnCompiler(t, state)
						if flexinfo[i].tag == SymbolTag && parts[pinx].flexcount == 0 && len(parts[pinx].parts) > 0 {
							// This is a PEM so reach through and use its anchor
							flexinfo[i].tag = parts[pinx].parts[0].tag
						}
						if parts[pinx] == nil {
							return nil
						}
						pinx++
					}
				}
				return makePattern(trm0.kind, parts, nflex, nil, flexinfo)
			}
		}
		subptns := make([]*Pattern, len(trm0.args))
		for i, t := range trm0.args {
			subptns[i] = ptnCompiler(t, state)
			if subptns[i] == nil {
				return nil
			}
		}
		return makePattern(trm0.kind, subptns, 0, nil, nil)
	case *TermTL:
		if trm0.kind == Funcall {
			if callingNamed(trm0, "mksym") {
				// if ident or dtype args are matchables, compile a SymbolPattern
				if isMatchable(trm0.args[0]) || isMatchable(trm0.args[1]) {
					subptns := make([]*Pattern, len(trm0.args))
					subptns[0] = ptnCompiler(trm0.args[0], state)
					subptns[1] = ptnCompiler(trm0.args[1], state)
					if len(trm0.args) == 3 {
						subptns[2] = ptnCompiler(trm0.args[2], state)
					}
					return makePattern(SymbolPattern, subptns, 0, nil, nil)
				}
				// else fall through
			} else if callingNamed(trm0, "parseRW") {
				return ptnCompiler(trm0.args[0], state)
			} else if callingNamed(trm0, "mktype") {
				panic("unwritten")
			}
			// check for flex elts
			// Note: I should get a bit more clever and merge the code for flex Funcall with that for Stmts
			ftnptn := ptnCompiler(trm0.trm, state)
			nflex := 0
			for _, t := range trm0.args {
				if flexElt(t) {
					nflex++
				}
			}
			if nflex > 0 {
				nfix := len(trm0.args) - nflex
				flexinfo := make([]flexPtnElt, len(trm0.args))
				parts := make([]*Pattern, nfix+1)
				parts[0] = ftnptn
				pinx := 1
				for i, t := range trm0.args {
					if flexElt(t) {
						flexinfo[i].mbl = t.(*Symbol)
					} else {
						flexinfo[i].tag = t.Tag()
						parts[pinx] = ptnCompiler(t, state)
						pinx++
					}
				}
				return makePattern(trm0.kind, parts, nflex, nil, flexinfo)
			}
			subptns := make([]*Pattern, len(trm0.args)+1)
			subptns[0] = ftnptn
			for i, t := range trm0.args {
				subptns[i+1] = ptnCompiler(t, state)
				if subptns[i+1] == nil {
					return nil
				}
			}
			return makePattern(trm0.kind, subptns, 0, nil, nil)
		}
	}
	panic("not reached")
}

// A XPRInfo holds state info when interpreting xprftns, mainly the matchables.
type XPRInfo struct {
	xprsyms       SymXDict  // this is the local scope: matchables (incl PEMs) and locals
	locals        []*Symbol // interpXprftn records locals calc'd during interpn
	funargs       []*Symbol // these will be bound to consts
	priorBindings []Term    // and records their prior bindings in this pll array for restoral
	// separately, we need to record and restore values of locals created in each loops
	eachSymbols  [][]*Symbol
	eachBindings [][]Term
	precompile   bool // flag concerning whether unbound matchables are ok
	smi          *STMapInfo
}

func makeXPRInfo(smi *STMapInfo) *XPRInfo {
	return &XPRInfo{nil, nil, nil, nil, nil, nil, false, smi}
}

func (xpi *XPRInfo) setup(mbls SymXDict) {
	xpi.xprsyms = mbls
	xpi.locals = make([]*Symbol, 0, len(mbls)/2)
	xpi.priorBindings = make([]Term, 0, len(mbls)/2)
}

// Call this after every exit from interpXprftn, success or failure. Otherwise, the rule will not be good to run again.
func (xpi *XPRInfo) cleanup() {
	for _, sx := range xpi.xprsyms {
		if sx.sym.plist.Find("TEM") != nil {
			sx.sym.binding = nil
		}
	}
	for i, sym := range xpi.locals {
		sym.binding = xpi.priorBindings[i]
	}
}

func (xpi *XPRInfo) enterEach() {
	xpi.eachSymbols = append(xpi.eachSymbols, make([]*Symbol, 0, 2))
	xpi.eachBindings = append(xpi.eachBindings, make([]Term, 0, 2))
}

func (xpi *XPRInfo) exitEach() {
	xpi.eachSymbols = xpi.eachSymbols[:len(xpi.eachSymbols)-1]
	xpi.eachBindings = xpi.eachBindings[:len(xpi.eachBindings)-1]
}

// implement the "tag" function for rewrite engine
var ttstrgTable = map[Termtag]string{
	SymbolTag: "symbol",
	Symchain:  "symchain",
	Funcall:   "funcall",
	EqExpr:    "eq",
	EqeqExpr:  "eqeq",
	Lognot:    "lognot",
	IfStmt:    "if",
	LoopStmt:  "loop",
	Range:     "range",
	Intlit:    "baseliteral",
	Floatlit:  "baseliteral",
	Stringlit: "baseliteral",
	Bytelit:   "baseliteral",
	Labellit:  "baseliteral",
	Boollit:   "baseliteral",
}

func termtagString(trm Term) string {
	strg := ttstrgTable[trm.Tag()]
	if strg == "" {
		panic("tag not in table")
	}
	return strg
}

var gensymCtr = 0

// var ctrmatch = 11 // making this a vbl allows to change in dbgr without recompiling
func gensym() string {
	gensymCtr++
	//if gensymCtr == ctrmatch {
	//	fmt.Print()
	//}
	return fmt.Sprintf("__v%d", gensymCtr)
}
func gensymPfx(pfx string) string {
	gensymCtr++
	//if gensymCtr == ctrmatch {
	//	fmt.Print()
	//}
	return fmt.Sprintf("%s%d", pfx, gensymCtr)
}

// Deep copy trm, leaving symbols as is. This probably deserves to be more prominent.
func termCopy(trm Term) Term {
	switch trm0 := trm.(type) {
	case *Symbol, *TermB:
		return trm0
	case *Funinst:
		var nbody *TermL
		if trm0.body != nil {
			nbody = termCopy(trm0.body).(*TermL)
		}
		return makeFuninst(trm0.scope, trm0.funargs, trm0.dtype, nbody)
	case *TermT:
		var a0 Term
		if trm0.arg0 != nil {
			a0 = termCopy(trm0.arg0)
		}
		return makeTermT(trm0.kind, a0, trm0.dtype, trm0.first, trm0.final)
	case *TermTT:
		a0 := termCopy(trm0.arg0)
		a1 := termCopy(trm0.arg1)
		return makeTermTT(trm0.kind, a0, a1, trm0.dtype, trm0.first, trm0.final)
	case *TermL:
		argsx := make([]Term, len(trm0.args))
		for i, a := range trm0.args {
			argsx[i] = termCopy(a)
		}
		return makeScopedTermL(trm0.kind, trm0.scope, argsx, trm0.dtype, trm0.first, trm0.final)
	case *TermTL:
		var tcpy Term
		if trm0.trm != nil {
			tcpy = termCopy(trm0.trm)
		}
		argsx := make([]Term, len(trm0.args))
		for i, a := range trm0.args {
			argsx[i] = termCopy(a)
		}
		return makeTermTL(trm0.kind, tcpy, argsx, trm0.dtype, trm0.first, trm0.final)
	}
	panic("not reached")
}

// Helper for getting the effective type of bound GTS types
func derefGTS(typ *Type) (rettype *Type) {
	rettype = typ
	if typ.family == TFGTS && typ.v.(*GTS).binding != nil {
		rettype = typ.v.(*GTS).binding
	}
	return
}

// We use a separate helper ftn for list-valued ftns. Wrap lists in a Litform0 term.
func interpXprListfunc(expr *TermTL, xpi *XPRInfo) Term {
	fnsym := expr.trm.(*Symbol)
	switch fnsym.ident {
	case "maplst":
		lst := interpXprExpr(expr.args[0], xpi) // get the list explicitly
		if lst.Tag() == ErrorTag {
			return lst
		}
		if lst.Tag() != Litform0 {
			return genError("non-manifest arg", expr)
		}
		lst0 := lst.(*TermL)
		funi := expr.args[1].(*Funinst)
		// put this and index symbols into xpi.xprsyms
		xpi.xprsyms.Add(funi.funargs[0], 0)
		xpi.xprsyms.Add(funi.funargs[1], 0)
		// manipulate values of these symbols while calling body code; accum result
		if len(funi.body.args) > 1 {
			return genError("locals not allowed in xprftn maplst calls", expr)
		}
		bodystmt := funi.body.args[0]
		rslt := make([]Term, len(lst0.args))
		for i, a := range lst0.args {
			funi.funargs[0].binding = a
			funi.funargs[1].binding = makeIntTerm2(i) // this is likely wasted effort
			tmp := interpXprExpr(bodystmt, xpi)
			if tmp.Tag() == ErrorTag {
				return tmp
			}
			rslt[i] = tmp
		}
		xpi.xprsyms.Remove(funi.funargs[1])
		xpi.xprsyms.Remove(funi.funargs[0])
		return makeTermL(Litform0, rslt, makeListType(funi.dtype.v.(*Ftntype).rettype), expr.first, expr.final)
	case "pushb":
		arg0 := expr.args[0]
		if arg0.Tag() != SymbolTag {
			return genError("pushb expects symbol", expr)
		}
		sym := arg0.(*Symbol)
		if sym.binding.Tag() != Litform0 {
			return genError("non-manifest", sym.binding)
		}
		lst0 := sym.binding.(*TermL)
		elt := interpXprExpr(expr.args[1], xpi)
		lst0.args = append(lst0.args, elt)
		return lst0
	case "filt":
		panic("unwritten")
	case "uniq":
		panic("unwritten")
	case "+":
		lst1 := interpXprExpr(expr.args[0], xpi)
		if lst1.Tag() == ErrorTag {
			return lst1
		}
		if lst1.Tag() != Litform0 {
			return genError("non-manifest arg", expr)
		}
		lst2 := interpXprExpr(expr.args[1], xpi)
		if lst2.Tag() == ErrorTag {
			return lst2
		}
		if lst2.Tag() != Litform0 {
			return genError("non-manifest arg", expr)
		}
		lst1args := lst1.(*TermL).args
		copy1 := make([]Term, len(lst1args))
		copy(copy1, lst1args)
		return makeTermL(Litform0, append(copy1, lst2.(*TermL).args...), TypeNothing, expr.first, expr.final)
	case "uses", "mods":
		trm := interpXprExpr(expr.args[0], xpi)
		if trm.Tag() == ErrorTag {
			return trm
		}
		rslt := usemod(trm, fnsym.ident == "mods")
		return makeTermL(Litform0, rslt, Gztermlist, Pos(-1), Pos(-1))
	case "defs":
		trm := interpXprExpr(expr.args[0], xpi)
		return makeTermL(Litform0, defslist(trm), Gztermlist, Pos(-1), Pos(-1))
	case "args":
		tmp := interpXprExpr(expr.args[0], xpi)
		if tmp.Tag() == SymbolTag && tmp.Plist().Find("var") == nil {
			tmp = tmp.(*Symbol).binding
		}
		switch funarg := tmp.(type) {
		case *TermL:
			if funarg.kind == EachStmt {
				return genError("unexpected type to args", expr)
			} else if funarg.kind == Litform0 {
				return funarg
			} else {
				return makeTermL(Litform0, funarg.args, Gztermlist, Pos(-1), Pos(-1))
			}
		case *TermTL:
			return makeTermL(Litform0, funarg.args, Gztermlist, Pos(-1), Pos(-1))
		default:
			return genError("unexpected argument to args", expr)
		}
	case "stmts":
		stmts := interpXprExpr(expr.args[0], xpi)
		if stmts.Tag() == ErrorTag {
			return stmts
		}
		if stmts.Tag() != Stmts {
			return genError("non-manifest arg", expr)
		}
		return makeTermL(Litform0, stmts.(*TermL).args, Gztermlist, Pos(-1), Pos(-1))
	case "attributes":
		arg := interpXprType(expr.args[0], xpi)
		if arg.Tag() == ErrorTag {
			return arg
		}
		typ := derefGTS(arg.(*Type))
		if typ.family != TFTuple {
			return genError("attributes requires a tuple type", expr)
		}
		ttyp := typ.v.(*Tupletype)
		lst := make([]Term, len(ttyp.attribs))
		for i, a := range ttyp.attribs {
			lst[i] = a
		}
		return makeTermL(Litform0, lst, Gzsymlist, expr.first, expr.final)
	case "variants":
		arg := interpXprType(expr.args[0], xpi)
		if arg.Tag() == ErrorTag {
			return arg
		}
		typ := derefGTS(arg.(*Type))
		if typ.family != TFOrtype {
			return genError("variants requires an ortype", expr)
		}
		ttyp := typ.v.(*Ortype)
		lst := make([]Term, len(ttyp.variants))
		for i, a := range ttyp.variants {
			lst[i] = a
		}
		return makeTermL(Litform0, lst, Gzsymlist, expr.first, expr.final)
	case "fmlargs":
		arg := interpXprExpr(expr.args[0], xpi)
		if arg.Tag() == ErrorTag {
			return arg
		}
		if arg.Tag() != FuninstTag {
			return genError("non-function arg", expr)
		}
		fmlargs := arg.(*Funinst).funargs // this is []*Symbol, so we need to copy for go
		args := make([]Term, len(fmlargs))
		for i, a := range fmlargs {
			args[i] = a
		}
		return makeTermL(Litform0, args, Gzsymlist, expr.first, expr.final)
	case "methods":
		arg := interpXprType(expr.args[0], xpi)
		if arg.Tag() == ErrorTag {
			return arg
		}
		typ := derefGTS(arg.(*Type))
		lst := make([]Term, len(typ.methods))
		for i, m := range typ.methods {
			lst[i] = m.sym
		}
		return makeTermL(Litform0, lst, Gzsymlist, expr.first, expr.final)
	case "matches":
		cp := expr.plist.Find("CP")
		if cp == nil {
			panic("not supposed to happen")
		}
		cp0 := cp.(*Pattern)
		infolst := []Term{}
		// run pattern matcher with full search, binding; return a list(matchinfo)
		nptnrt := makePtnRT(-1)
		var tuptype *Type
		for trminx := 1; trminx < len(expr.args); trminx++ {
			trm := interpXprExpr(expr.args[trminx], xpi)
			nptnrt.psearch(trm, cp0)
			if tuptype == nil && len(nptnrt.matchinfos) > 0 {
				atrbs := make([]*Symbol, len(nptnrt.matchinfos[0]))
				for i, trm := range nptnrt.matchinfos[0] {
					if i < len(nptnrt.bindings) {
						atrbs[i] = makeSymbol(nptnrt.bindings[i].ident, trm.Dtype(), nil)
					} else {
						atrbs[i] = makeSymbol("term", trm.Dtype(), nil)
					}
				}
				tuptype = makeType(TFTuple, atrbs, false)
			}
			for _, mi := range nptnrt.matchinfos {
				tupelts := make([]Term, len(mi))
				for i, trm := range mi {
					tupelts[i] = trm
				}
				tuplit := makeTermL(Litform0, tupelts, tuptype, Pos(-1), Pos(-1))
				infolst = append(infolst, tuplit)
			}

		}
		return makeTermL(Litform0, infolst, makeListType(tuptype), expr.first, expr.final)
	}
	panic("not reached")
}

// We use a separate helper ftn for type-valued ftns.
func interpXprType(expr Term, xpi *XPRInfo) (retval Term) {
	tg := expr.Tag()
	if tg == TypeTag {
		return derefGTS(expr.(*Type))
	}
	// set default; this mods no visible state
	if tg == SymbolTag {
		sym := expr.(*Symbol)
		if sym.dtype == nil || sym.dtype == Gztype {
			return sym.binding
		}
	}
	if tg == Funcall && expr.(*TermTL).trm.Tag() == SymbolTag {
		expr0 := expr.(*TermTL)
		arg0 := expr0.args[0]
		switch expr0.trm.(*Symbol).ident {
		default:
			panic("unwritten")
		case "mktype":
			// first arg is a label lit that determines the rest. Return a Type
			// Note that fam can be a family name or a typegen name but not everything is coded yet.
			fam := expr0.args[0].(*TermB).value
			switch fam {
			case "list", "space":
				eltyp := interpXprType(expr0.args[1], xpi)
				if eltyp.Tag() == ErrorTag {
					return eltyp
				}
				if fam == "space" {
					dimx := expr0.args[2]
					if dimx.Tag() != Litform0 || len(dimx.(*TermTL).args) != 1 {
						return genError("space dimension should present as a litform with a single element", expr0.args[2])
					}
					dim0 := interpXprExpr(dimx.(*TermTL).args[0], xpi)
					if dim0.Tag() == ErrorTag {
						return dim0
					}
					return makeType(TFSpace, eltyp, dim0)
				} else {
					return makeType(TFList, eltyp)
				}
			case "tuple":
				atrbs := interpXprExpr(expr0.args[1], xpi)
				if atrbs.Tag() != Litform0 {
					panic("???")
				}
				attribs := make([]*Symbol, len(atrbs.(*TermL).args))
				for i, a := range atrbs.(*TermL).args {
					tmp := interpXprExpr(a, xpi)
					if tmp.Tag() == ErrorTag {
						return tmp
					}
					attribs[i] = tmp.(*Symbol)
				}
				return makeType(TFTuple, attribs, false)
			case "ortype":
				if expr0.args[1].Tag() != Litform0 {
					panic("???")
				}
				vnts := make([]*Symbol, len(expr0.args[1].(*TermL).args))
				for i, a := range expr0.args[1].(*TermL).args {
					tmp := interpXprExpr(a, xpi)
					if tmp.Tag() == ErrorTag {
						return tmp
					}
					vnts[i] = tmp.(*Symbol)
				}
				return makeType(TFOrtype, vnts)
			case "function":
				args := expr0.args[1]
				if args.Tag() != Litform0 {
					panic("unwritten")
				}
				argsx := make([]*Type, len(args.(*TermL).args))
				for i, a := range args.(*TermL).args {
					tmp := interpXprType(a, xpi)
					if tmp.Tag() == ErrorTag {
						return tmp
					}
					argsx[i] = tmp.(*Type)
				}
				retyp := interpXprType(expr0.args[2], xpi)
				if retyp.Tag() == ErrorTag {
					return retyp
				}
				ntyp := makeType(TFFtn, argsx, retyp, expr0.args[3].(*TermB).value == "pure")
				return ntyp
			case "integer":
				return TypeInt
			case "float":
				return TypeFloat
			case "string":
				return TypeString
			case "Gomap":
				keytyp := interpXprType(expr0.args[1], xpi)
				if keytyp.Tag() == ErrorTag {
					return keytyp
				}
				valtyp := interpXprType(expr0.args[1], xpi)
				if valtyp.Tag() == ErrorTag {
					return valtyp
				}
				ntyp := makeType(TFGomap, keytyp, valtyp)
				return ntyp
			case "Gochannel":
				chntyp := interpXprType(expr0.args[1], xpi)
				if chntyp.Tag() == ErrorTag {
					return chntyp
				}
				dir := interpXprExpr(expr0.args[2], xpi)
				if dir.Tag() != Labellit {
					return genError("require manifest label for channel direction", expr)
				}
				ntyp := makeType(TFGochan, chntyp, dir.(*TermB).value)
				return ntyp
			default:
				return genError("no code yet to create type based on "+fam, expr)
			}
			retval = interpXprFuncall(expr0, xpi)
		case "mktypex":
			panic("unwritten")
		case "lookupType":
			ident := interpXprExpr(arg0, xpi)
			if ident.Tag() == ErrorTag {
				return ident
			}
			var ident0 string
			if ident.Tag() == Stringlit || ident.Tag() == Labellit {
				ident0 = ident.(*TermB).value
			} else {
				return genError("identifier expected", ident)
			}
			typsym := gblScope.lookupType(ident0)
			if typsym == nil {
				return genError("no such type", ident)
			}
			retval := derefGTS(typsym.binding.(*Type))
			return retval
		case "type":
			arg := interpXprExpr(arg0, xpi)
			if arg.Tag() == ErrorTag {
				return arg
			}
			retval = arg.Dtype()
			if retval.Tag() == TypeTag && retval.(*Type).family == TFGTS && retval.(*Type).v.(*GTS).binding != nil {
				retval = retval.(*Type).v.(*GTS).binding
			}
		case "elttype":
			arg := interpXprType(arg0, xpi)
			if arg.Tag() == ErrorTag {
				return arg
			}
			retval = derefGTS(arg.(*Type)).Elttype()
		case "rettype":
			arg := interpXprType(arg0, xpi)
			if arg.Tag() == TypeTag && arg.(*Type).family == TFFtn {
				retval = arg.(*Type).v.(*Ftntype).rettype
			}
		case "family":
			panic("unwritten")
		case "typesubst":
			// Use ConczInfo.handleType. Its constructor inconveniently requires a []*Symbol for the generics,
			// since that allows to distinguish GTS and GES. But if we construct with nil args, we can then smash in
			// what we want.
			subject := interpXprType(expr0.args[0], xpi)
			if subject.Tag() == ErrorTag {
				return subject
			}
			b4 := interpXprType(expr0.args[1], xpi)
			if b4.Tag() == ErrorTag {
				return b4
			}
			b4t := derefGTS(b4.(*Type))
			aftr := interpXprType(expr0.args[2], xpi)
			if aftr.Tag() == ErrorTag {
				return aftr
			}
			aftrt := derefGTS(aftr.(*Type))
			czi := makeConczInfo(nil, nil)
			czi.gnctypes = []*Type{b4t}
			czi.conctypes = []*Type{aftrt}
			retval = czi.handleType(subject.(*Type))
		}
	}
	if retval == nil {
		retval = genError("cannot interpret expr as a type", expr)
	}
	return
}

// mkterm is complicated enough to deserve its own helper function.
// first arg is a label lit that determines the rest. Return the constructed Term, which can be an Error term.
func interpMkterm(expr *TermTL, xpi *XPRInfo) (retval Term) {
	tagarg := expr.args[0].(*TermB).value
	args := expr.args
	nargs := len(args)
	// I have looked into using the same table that drives TC but there are too many diffs in what's needed here.
	switch tagarg {
	case "symchain":
		root := interpXprExpr(expr.args[1], xpi)
		if root.Tag() == ErrorTag {
			return root
		}
		chainargs := make([]Term, len(expr.args)-1)
		chainargs[0] = root
		var typ *Type
		for i, sx := range expr.args[2:] {
			tmp := interpXprExpr(sx, xpi)
			if tmp.Tag() == ErrorTag {
				return tmp
			}
			if tmp.Tag() == Labellit && root.Dtype().family == TFTuple {
				for _, a := range root.Dtype().v.(*Tupletype).attribs {
					if a.ident == tmp.(*TermB).value {
						tmp = a
						break
					}
				}
			}
			if tmp.Tag() != SymbolTag {
				return genError("symbol expected", expr)
			}
			chainargs[i+1] = tmp
			typ = tmp.Dtype()
		}
		return makeTermL(Symchain, chainargs, typ, expr.first, expr.final)
	case "gdref":
		arg := interpXprExpr(expr.args[1], xpi)
		if arg.Tag() == ErrorTag {
			return arg
		}
		typ := interpXprType(expr.args[2], xpi)
		if typ.Tag() == ErrorTag {
			return typ
		}
		return makeTermT(Gdref, arg, typ.(*Type), expr.first, expr.final)
	case "litform":
		typ := interpXprType(expr.args[1], xpi)
		if typ.Tag() == ErrorTag {
			return typ
		}
		litvals := expr.args[2]
		tg2 := litvals.Tag()
		if tg2 == Litform0 {
			lf0 := expr.args[2].(*TermL)
			lfargs := make([]Term, len(lf0.args))
			for i, x := range lf0.args {
				lfargs[i] = interpXprExpr(x, xpi)
				if lfargs[i].Tag() == ErrorTag {
					return lfargs[i]
				}
			}
			return makeTermL(Litform0, lfargs, typ.(*Type), lf0.first, lf0.final)
		} else if tg2 == SymbolTag { // already interpd
			litvals = litvals.(*Symbol).binding
			if litvals.Tag() != Litform0 {
				return genError(fmt.Sprintf("can't interpret %s as a list", expr.args[2]), expr)
			}
			return makeTermL(Litform0, litvals.(*TermL).args, typ.(*Type), litvals.First(), litvals.Final())
		} else if tg2 != Funcall {
			return genError(fmt.Sprintf("can't interpret %s as a list", expr.args[2]), expr)
		}
		lst := interpXprListfunc(expr.args[2].(*TermTL), xpi)
		if lst.Tag() == ErrorTag {
			return lst
		}
		lst0 := lst.(*TermL)
		lst0.dtype = typ.(*Type)
		lst0.first = expr.first
		lst0.final = expr.final
		retval = lst
	case "val", "var":
		tg := Valdecl
		if tagarg == "var" {
			tg = Vardecl
		}
		sym := interpXprExpr(args[1], xpi)
		if sym.Tag() != SymbolTag {
			return genError(fmt.Sprintf("argument to %s does not generate a symbol", tagarg), expr)
		}
		if tg == Vardecl {
			sym.Plist().Add("var", true)
		}
		return makeTermT(tg, sym, TypeNothing, expr.first, expr.final)
	case "funcall":
		ftn := interpXprExpr(args[1], xpi) // normally a symbol but could be a funinst
		if ftn.Tag() == ErrorTag {
			return ftn
		}
		// assume arglist is litform, not calculated. Not sure this is really ok
		if args[2].Tag() != Litform0 {
			return genError("mkterm funcall does not accept calculated arg lists", expr)
		}
		arglist := args[2].(*TermL)
		argsx := make([]Term, len(arglist.args))
		for i, a := range arglist.args {
			a0 := interpXprExpr(a, xpi)
			if a0.Tag() == ErrorTag {
				return a0
			}
			argsx[i] = a0
		}
		ftyp := ftn.Dtype()
		var rettyp *Type
		if ftyp.family == TFFtn {
			rettyp = ftyp.v.(*Ftntype).rettype
		} else if ftyp == Gzterm || ftyp == Gzsym {
			rettyp = ftyp
		} else {
			return genError("function arg generates non-function", expr)
		}
		return makeTermTL(Funcall, ftn, argsx, rettyp, expr.first, expr.final)
	case "if":
		cond := interpXprExpr(args[1], xpi)
		if cond.Tag() == ErrorTag {
			return cond
		}
		tpart := interpXprExpr(args[2], xpi)
		if tpart.Tag() == ErrorTag {
			return tpart
		}
		if nargs > 3 {
			epart := interpXprExpr(args[3], xpi)
			if epart.Tag() == ErrorTag {
				return epart
			}
			retval = makeTermL(IfStmt, []Term{cond, tpart, epart}, tpart.Dtype(), expr.first, expr.final)
		} else {
			retval = makeTermL(IfStmt, []Term{cond, tpart}, TypeNothing, expr.first, expr.final)
		}
	case "ifcase":
		clauses := interpXprExpr(args[1], xpi)
		if clauses.Tag() == ErrorTag {
			return clauses
		}
		if clauses.Tag() != Litform0 {
			return genError("non-manifest clauses to ifcase", expr)
		}
		clz0 := clauses.(*TermL)
		retval = makeTermL(Ifcase, clz0.args, clz0.args[0].Dtype(), expr.first, expr.final)
	case "oncondition":
		cond := interpXprExpr(args[1], xpi)
		if cond.Tag() == ErrorTag {
			return cond
		}
		body := interpXprExpr(args[2], xpi)
		if body.Tag() == ErrorTag {
			return body
		}
		retval = makeTermTT(OncondStmt, cond, body, TypeNothing, expr.first, expr.final)
	case "arrow":
		lhs := interpXprExpr(args[1], xpi)
		if lhs.Tag() == ErrorTag {
			return lhs
		}
		rhs := interpXprExpr(args[2], xpi)
		if rhs.Tag() == ErrorTag {
			return rhs
		}
		retval = makeTermTT(Arrow, lhs, rhs, rhs.Dtype(), expr.first, expr.final)
	case "eq", "eqeq":
		tg := EqExpr
		if tagarg == "eqeq" {
			tg = EqeqExpr
		}
		lhs := interpXprExpr(args[1], xpi)
		if lhs.Tag() == ErrorTag {
			return lhs
		}
		rhs := interpXprExpr(args[2], xpi)
		if rhs.Tag() == ErrorTag {
			return rhs
		}
		retval = makeTermTT(tg, lhs, rhs, TypeBool, expr.first, expr.final)
	case "not":
		cond := interpXprExpr(args[1], xpi)
		if cond.Tag() == ErrorTag {
			return cond
		}
		retval = Simplify(makeTermT(Lognot, cond, TypeBool, expr.first, expr.final))
	case "irange", "frange":
		r1 := interpXprExpr(args[1], xpi)
		if r1.Tag() == ErrorTag {
			return r1
		}
		r2 := interpXprExpr(args[2], xpi)
		if r2.Tag() == ErrorTag {
			return r2
		}
		r3 := interpXprExpr(args[3], xpi)
		if r3.Tag() == ErrorTag {
			return r3
		}
		r4 := interpXprExpr(args[4], xpi)
		if r4.Tag() == ErrorTag {
			return r4
		}
		rangenm := "rangeI"
		if tagarg == "frange" {
			rangenm = "rangeF"
		}
		rsyms := biScope[rangenm]
		var rngtyp *Type
		for _, s := range rsyms {
			if s.dtype == nil {
				rngtyp = s.binding.(*Type)
				break
			}
		}
		if rngtyp == nil {
			panic("impossible")
		}
		lftrm := makeTermL(Litform0, []Term{r1, r2, r3, r4}, rngtyp, expr.first, expr.final)
		return makeTermT(Range, lftrm, makeListType(r1.Dtype()), Pos(-1), Pos(-1))
	case "asgn":
		lhs := interpXprExpr(args[1], xpi)
		if !(lhs.Tag() == SymbolTag || lhs.Tag() == Symchain) {
			return genError("illegal assignment lval", expr)
		}
		rhs := interpXprExpr(args[2], xpi)
		if rhs.Tag() == ErrorTag {
			return rhs
		}
		return makeTermTT(AsgnStmt, lhs, rhs, TypeNothing, expr.first, expr.final)
	case "each":
		itervar := interpXprExpr(args[1], xpi)
		if itervar.Tag() != SymbolTag {
			return genError("each requires a symbol", expr)
		}
		enumeree := interpXprExpr(args[2], xpi)
		if enumeree.Tag() == ErrorTag {
			return enumeree
		}
		eachscope := makeScope()
		eachscope.Add(itervar.(*Symbol), 0)
		body := interpXprExpr(args[3], xpi)
		if body.Tag() == ErrorTag {
			return body
		}
		if body.Tag() != Stmts {
			return genError("statement list expected", expr)
		}
		eachargs := []Term{itervar, enumeree, body}
		if len(args) == 5 {
			inxvar := interpXprExpr(args[4], xpi)
			if inxvar.Tag() == ErrorTag {
				return inxvar
			}
			eachscope.Add(inxvar.(*Symbol), 0)
			eachargs = append(eachargs, inxvar)
		}
		body.(*TermL).scope.parent = eachscope
		return makeScopedTermL(EachStmt, eachscope, eachargs, TypeNothing, expr.first, expr.final)
	case "break", "continue":
		tg := BreakStmt
		if tagarg != "break" {
			tg = ContinueStmt
		}
		retval = makeTermT(tg, nil, TypeNothing, expr.first, expr.final)
	case "return":
		var retval0 Term
		var rettyp *Type
		if nargs > 1 {
			retval0 = interpXprExpr(args[1], xpi)
			if retval0.Tag() == ErrorTag {
				return retval0
			}
			rettyp = retval0.Dtype()
		}
		retval = makeTermT(ReturnStmt, retval0, rettyp, expr.first, expr.final)
	case "typeterm":
		typ := interpXprType(expr.args[1], xpi)
		if typ.Tag() == ErrorTag {
			return typ
		}
		lbl := interpXprExpr(args[2], xpi)
		if lbl.Tag() == ErrorTag {
			return lbl
		}
		retval = makeTermT(Typeterm, lbl, typ.(*Type), expr.first, expr.final)
	}
	if retval == nil {
		panic("unwritten")
	}
	return
}

// Helper for interpXprExpr, handles all funcalls, which is most of the cases.
// Many of these are detected here for disposition in various helper ftns: interpXprListfunc, interpMkterm
func interpXprFuncall(expr *TermTL, xpi *XPRInfo) Term {
	fnsym := expr.trm.(*Symbol)
	fntyp := fnsym.dtype.v.(*Ftntype)
	if fntyp.rettype == Gztype {
		return interpXprType(expr, xpi)
	}
	if xpi.precompile {
		// not sure yet whether to eval args on uneval'd ftns...
		switch fnsym.ident {
		case "match", "contains", "matchcount", "matches":
			cp := compilePattern(expr.args[0], expr, xpi.xprsyms, false, true)
			if cp.Tag() != ErrorTag && len(expr.args) > 1 {
				// note that cp aliases expr here
				for i, a := range expr.args[1:] {
					a0 := interpXprExpr(a, xpi)
					if a0.Tag() == ErrorTag {
						return a0
					}
					expr.args[i+1] = a0
				}
			}
			return cp
		case "mksym":
			ident0 := interpXprExpr(expr.args[0], xpi)
			identb, isTB := ident0.(*TermB)
			var typ *Type
			if expr.args[1].Tag() == TypeTag {
				typ = expr.args[1].(*Type)
			} else if callingNamed(expr.args[1], "mktype") {
				typ0 := interpXprExpr(expr.args[1], xpi)
				if typ0.Tag() == TypeTag {
					typ = typ0.(*Type)
				}
			}
			var bdg Term
			if len(expr.args) == 3 {
				bdg = interpXprExpr(expr.args[2], xpi)
				if bdg.Tag() == ErrorTag {
					bdg = expr.args[2]
				}
			}
			if typ == nil {
				if len(expr.args) == 3 {
					expr.args[2] = bdg
				}
				return expr
			}
			if isTB {
				return makeSymbol(identb.value, typ, bdg)
			}
			expr.args[1] = typ
			return expr
		case "mkterm":
			return interpMkterm(expr, xpi)
		}
		return expr
	}
	switch fnsym.ident {
	case "*", "+", "-", "/", "<", "<=", ">", ">=":
		// all binops already handled by simplify
		tmp0 := interpXprExpr(expr.args[0], xpi)
		if tmp0.Tag() == ErrorTag {
			return tmp0
		}
		_, trmb := tmp0.(*TermB)
		if !trmb {
			return genError("non-manifest arg", expr)
		}
		tmp1 := interpXprExpr(expr.args[1], xpi)
		if tmp1.Tag() == ErrorTag {
			return tmp1
		}
		_, trmb = tmp1.(*TermB)
		if !trmb {
			return genError("non-manifest arg", expr)
		}
		return Simplify(makeTermTL(Funcall, expr.trm, []Term{tmp0, tmp1}, expr.dtype, expr.first, expr.final))
	case "match", "contains", "matchcount":
		cp := expr.plist.Find("CP")
		if cp == nil {
			panic("not supposed to happen")
		}
		cp0 := cp.(*Pattern)
		rslt := FalseLiteral
		if fnsym.ident == "match" {
			// run pattern matcher without search, with binding; return a boolean
			matchrslt := makePtnRT(-1).pmatch(interpXprExpr(expr.args[1], xpi), cp0)
			if matchrslt {
				rslt = TrueLiteral
			}
		} else {
			// run pattern matcher with search, binding, early stop; return a boolean
			ctr := -1
			if fnsym.ident == "matchcount" {
				ctr = 0
			}
			nptnrt := makePtnRT(ctr)
			nptnrt.stopOnFound = fnsym.ident == "contains"
			for trminx := 1; trminx < len(expr.args); trminx++ {
				trm := interpXprExpr(expr.args[trminx], xpi)
				matchrslt := nptnrt.psearch(trm, cp0)
				if matchrslt != nil && nptnrt.stopOnFound {
					rslt = TrueLiteral
					break
				}
			}
			if fnsym.ident == "matchcount" {
				rslt = makeIntTerm2(nptnrt.counter)
			}
		}
		return rslt
	case "count":
		lst := interpXprExpr(expr.args[0], xpi)
		if lst.Tag() == SymbolTag {
			s := lst.(*Symbol)
			if s.binding != nil {
				lst = s.binding
			}
		}
		if lst.Tag() == Litform0 {
			return makeIntlit(len(lst.(*TermL).args))
		} else if lst.Tag() == Range {
			rcnt := tryRangeCount(lst)
			if rcnt.Tag() == Intlit {
				return rcnt
			}
		}
		return genError("non-manifest list to count", expr.args[0])
	case "mkterm":
		return interpMkterm(expr, xpi)
	case "mksym":
		// 2 or 3 args, first typed as label. Return a Symbol
		ident0 := interpXprExpr(expr.args[0], xpi)
		if ident0.Tag() == SymbolTag {
			ident0 = interpXprExpr(ident0.(*Symbol).binding, xpi)
		}
		if ident0.Tag() == ErrorTag {
			return ident0
		}
		_, isTB := ident0.(*TermB)
		if !isTB {
			ident0 = genError("ident argument must evaluate to string or label", expr.args[0])
		}
		if ident0.Tag() == ErrorTag {
			return ident0
		}
		typ := interpXprType(expr.args[1], xpi)
		if typ.Tag() == ErrorTag {
			return typ
		}
		var bdg Term
		if len(expr.args) == 3 {
			bdg = interpXprExpr(expr.args[2], xpi)
		}
		return makeSymbol(ident0.(*TermB).value, typ.(*Type), bdg)
	case "mkftn":
		typ := interpXprType(expr.args[0], xpi)
		if typ.Tag() == ErrorTag {
			return typ
		}
		if typ.Tag() != TypeTag || typ.(*Type).family != TFFtn {
			return genError("not a function type", expr)
		}
		funargs := interpXprExpr(expr.args[1], xpi)
		if funargs.Tag() == ErrorTag {
			return funargs
		}
		if funargs.Tag() != Litform0 {
			return genError("funargs are not manifest", expr)
		}
		scp := makeScope()
		funargs0 := make([]*Symbol, len(funargs.(*TermL).args))
		for i, a := range funargs.(*TermL).args {
			s := a.(*Symbol)
			scp.Add(s, 0)
			funargs0[i] = s
		}
		body := interpXprExpr(expr.args[2], xpi)
		if body.Tag() == ErrorTag {
			return body
		}
		return makeFuninst(scp, funargs0, typ.(*Type), body.(*TermL))
	case "mkstmts":
		// 1 arg typed as list(xprterm). Eval this and wrap in Stmts term.
		// Attempt to infer the correct dtype for the new stmtlist.
		arg := expr.args[0]
		evalArg := true
		if arg.Tag() == SymbolTag {
			arg = arg.(*Symbol).binding
			evalArg = false
		}
		if arg.Tag() != Litform0 && arg.Tag() != Funcall {
			return genError("cannot interpret mkstmts arg as a list", arg)
		}
		var rslt []Term
		var rettype *Type
		if arg.Tag() == Litform0 {
			expr0 := arg.(*TermL)
			if evalArg {
				rslt = make([]Term, len(expr0.args))
				for i, a := range expr0.args {
					rslt[i] = interpXprExpr(a, xpi)
					if rslt[i].Tag() == ErrorTag {
						return rslt[i]
					}
					rettype = rslt[i].Dtype()
				}
			} else {
				rslt = expr0.args
			}
		} else {
			trm := interpXprListfunc(arg.(*TermTL), xpi)
			if trm.Tag() == ErrorTag {
				return trm
			}
			if trm.Tag() != Litform0 {
				return genError("non-manifest list to count", arg)
			}
			rslt = trm.(*TermL).args
			rettype = Gzstmts
			if len(rslt) > 0 {
				rettype = rslt[len(rslt)-1].Dtype()
			}
		}
		stmts := makeScopedTermL(Stmts, makeScope(), rslt, rettype, expr.first, expr.final)
		flattenStmts1(stmts)
		stmts.scope.parent = xpi.smi.scopetree
		return stmts
	case "lookup":
		ident := interpXprExpr(expr.args[0], xpi)
		if ident.Tag() == ErrorTag {
			return ident
		}
		var ident0 string
		if ident.Tag() == Stringlit || ident.Tag() == Labellit {
			ident0 = ident.(*TermB).value
		} else {
			return genError("identifier expected", ident)
		}
		sym, _ := gblScope.Lookup(ident0)
		if sym == nil {
			return genError("symbol not found", expr)
		}
		return sym
	case "methods", "asserts", "handlers", "argtypes", "family", "purity", "attributes", "dimension", "methodNamed":
		arg := interpXprType(expr.args[0], xpi)
		if arg.Tag() == ErrorTag {
			return arg
		}
		typ := derefGTS(arg.(*Type))
		switch fnsym.ident {
		case "argtypes", "purity":
			if typ.family != TFFtn {
				return genError("argtypes requires function type", expr)
			}
			ftyp := typ.v.(*Ftntype)
			if fnsym.ident == "argtypes" {
				// go doesn't infer []*Type to []Term even though *Type is a Term. I actually agree
				// with this design choice even if it's a bit inconvenient at the moment.
				argtypes := make([]Term, len(ftyp.fmlargs))
				for i, t := range ftyp.fmlargs {
					argtypes[i] = t
				}
				return makeTermL(Litform0, argtypes, Gztypelist, expr.first, expr.final)
			} else {
				strg := "pure"
				if !ftyp.pure {
					strg = "mod"
				}
				return makeTermB(Labellit, strg, Pos(-1))
			}
		case "dimension":
			if typ.family != TFSpace {
				return genError("dimension requires a space type", expr)
			}
			spctyp := typ.v.(*Spacetype)
			return spctyp.dimensions
		case "asserts":
			// note: little used and a bit tricky since assertions are pre-processed internally
			panic("unwritten")
		case "family":
			return makeTermB(Labellit, typeFamilyStrings[typ.family], Pos(-1))
		case "handlers":
			panic("unwritten")
		case "methodNamed":
			lbllit := interpXprExpr(expr.args[1], xpi)
			if lbllit.Tag() == ErrorTag {
				return lbllit
			}
			if lbllit.Tag() != Labellit {
				return genError("non-manifest name to methodNamed", expr)
			}
			nm := lbllit.(*TermB).value
			mthd := typ.methodLookup(nm)
			if mthd == nil {
				return genError(fmt.Sprintf("method %s not found for %s", nm, typ.String()), expr)
			}
			if xpi.smi != nil {
				mthd = xpi.smi.visit(mthd).(*Symbol)
			}
			return mthd
		}
	case "pkgSymbol":
		// if pkg isn't imported, do a kind of "underground" import (activate but don't make symtbl generally available)
		path := interpXprExpr(expr.args[0], xpi)
		if path.Tag() == ErrorTag {
			return path
		}
		if path.Tag() != Stringlit {
			return genError("non-manifest path to pkgSymbol", expr)
		}
		pathstrg := path.(*TermB).value
		mop := QIPathMap[pathstrg]
		if mop == nil || mop.pkg == nil {
			inx := binsearchStrings(pkgdbNames, pathstrg)
			if inx < 0 || inx >= len(pkgdbNames) {
				return genError("unknown package path", expr)
			}
			mname := pathstrg
			pslen := len(pathstrg) - 1
			for mninx := 0; mninx < len(pathstrg); mninx++ {
				if pathstrg[pslen-mninx] == '/' {
					mname = pathstrg[pslen+1-mninx:]
					break
				}
			}
			pkgdb[inx].AddPkg(pathstrg, mname)
			mop = QIPathMap[pathstrg]
		}
		scope := mop.pkg.scope
		identx := interpXprExpr(expr.args[1], xpi)
		if identx.Tag() == ErrorTag {
			return identx
		}
		if identx.Tag() != Stringlit {
			return genError("non-manifest symbol identifier to pkgSymbol", expr)
		}
		sym := scope.entries.Find(identx.(*TermB).value)
		if sym != nil {
			return sym
		}
		return genError("cannot find symbol in package", expr)
	case "arg0":
		tmp := interpXprExpr(expr.args[0], xpi)
		switch funarg := tmp.(type) {
		case *TermL:
			return makeTermL(Litform0, funarg.args, Gztermlist, expr.first, expr.final)
		case *TermTL:
			return funarg.trm
		case *TermT:
			return funarg.arg0
		case *TermTT:
			return funarg.arg0
		default:
			return genError("unexpected argument to arg0", expr)
		}
	case "arg1":
		tmp := interpXprExpr(expr.args[0], xpi)
		switch funarg := tmp.(type) {
		case *TermL:
			if funarg.kind == IfStmt {
				return funarg.args[1]
			} else {
				return genError("unexpected type to arg1", expr)
			}
		case *TermTT:
			return funarg.arg1
		default:
			return genError("unexpected argument to arg1", expr)
		}
	case "defn":
		tmp := interpXprExpr(expr.args[0], xpi)
		if tmp.Tag() != FuninstTag {
			return genError("non-function arg", expr)
		}
		funi := tmp.(*Funinst)
		if funi.body == nil {
			return genError("cannot take defn of builtin function", expr)
		}
		return funi.body
	case "ident":
		tmp := interpXprExpr(expr.args[0], xpi)
		if tmp.Tag() != SymbolTag {
			return genError("expected a symbol", expr)
		}
		sym := tmp.(*Symbol)
		return makeLabelTerm(sym.ident, Pos(-1))
	case "termtag":
		tmp := interpXprExpr(expr.args[0], xpi)
		tagstrg := termtagString(tmp)
		return makeLabelTerm(tagstrg, Pos(-1))
	case "gensym":
		return makeTermB(Stringlit, gensym(), Pos(-1))
	case "typedAs":
		ent := interpXprExpr(expr.args[0], xpi)
		typ := interpXprType(expr.args[1], xpi)
		if typ.Tag() != TypeTag {
			return genError("type expected", expr.args[1])
		}
		if ent.Tag() == SymbolTag {
			return makeTermT(Gdref, ent, typ.(*Type), Pos(-1), Pos(-1))
		} else {
			smashDtype(ent, typ.(*Type))
			return ent
		}
	case "renameLocals":
		stmts1 := interpXprExpr(expr.args[0], xpi)
		stmts2 := interpXprExpr(expr.args[1], xpi)
		if stmts1.Tag() != Stmts || stmts2.Tag() != Stmts {
			return genError("non-stmts to renameLocals", expr)
		}
		s1locals := make([]*Symbol, 0, 2)
		s1 := stmts1.(*TermL)
		for _, s := range s1.args {
			tg := s.Tag()
			if tg == Valdecl || tg == Vardecl {
				s1locals = append(s1locals, s.(*TermT).arg0.(*Symbol))
			}
		}
		if len(s1locals) > 0 {
			s2locals := make([]*Symbol, 0, 2)
			for _, s := range stmts2.(*TermL).args {
				tg := s.Tag()
				if tg == Valdecl || tg == Vardecl {
					s2locals = append(s2locals, s.(*TermT).arg0.(*Symbol))
				}
			}
			// I'm going to smash the symbols; I think it's safe but I am slightly queasy about it.
			for _, s := range s2locals {
				xid := identifierOutOfScope(s1.scope, []*Scope{}, s.ident, s)
				s.ident = xid
			}
		}
		return stmts2
	case "ceiling", "round", "floor":
		// build in the functions to cvt float to int
		trm := interpXprExpr(expr.args[0], xpi) // trm will be float
		if trm.Tag() == ErrorTag {
			return trm
		}
		if trm.Tag() != Floatlit {
			return genError("manifest float expected", expr)
		}
		fltval := trm.Plist().Find("floatval").(float64)
		intval := 0
		if fnsym.ident == "round" {
			intval = int(math.Round(fltval))
		} else if fnsym.ident == "ceiling" {
			intval = int(math.Ceil(fltval))
		} else {
			intval = int(math.Floor(fltval))
		}
		return makeIntlit(intval)
	case "autocvt":
		// handle some simple builtin autocvts
		trm := interpXprExpr(expr.args[0], xpi)
		if trm.Tag() == ErrorTag {
			return trm
		}
		rettyp := fnsym.dtype.v.(*Ftntype).rettype
		if trm.Tag() == Intlit && rettyp == TypeFloat {
			return makeFloatTerm(trm.(*TermB).value, Pos(-1))
		} else {
			_, isb := trm.(*TermB)
			errmsg := ""
			if !isb {
				errmsg = "non-manifest value to autocvt"
			} else {
				errmsg = "unimplemented flavor of autocvt"
			}
			return genError(errmsg, expr)
		}
	case "rvalindex", "filt1", "filt1x", "index":
		lst := interpXprExpr(expr.args[0], xpi)
		if lst.Tag() != Litform0 {
			if lst.Tag() == SymbolTag && lst.(*Symbol).binding != nil && lst.(*Symbol).binding.Tag() == Litform0 {
				lst = lst.(*Symbol).binding
			} else {
				return genError("non-manifest arg", expr)
			}
		}
		lst0 := lst.(*TermL)
		if fnsym.ident == "rvalindex" {
			inxlit := interpXprExpr(expr.args[1], xpi)
			if inxlit.Tag() != Intlit {
				return genError("non-manifest arg", expr)
			}
			inx := inxlit.Plist().Find("intval").(int)
			if inx < 0 || inx >= len(lst0.args) {
				return genError(fmt.Sprintf("index (%d) out of bounds in rvalindex", inx), expr)
			}
			return lst0.args[inx]
		} else if fnsym.ident == "index" {
			what := interpXprExpr(expr.args[1], xpi)
			for i, x := range lst0.args {
				if x.Equal(what) {
					return makeIntlit(i)
				}
			}
			return nilEntity
		} else {
			if fnsym.ident == "filt1x" {
				xfm := expr.args[2].(*Funinst)
				if !(len(xfm.body.args) == 1 && xfm.body.args[0] == xfm.funargs[1]) {
					return genError("currently only lst[pred => index] is supported in rewrites", expr)
				}
			}
			pred := expr.args[1].(*Funinst)
			// put this and index symbols into xpi.xprsyms
			xpi.xprsyms.Add(pred.funargs[0], 0)
			xpi.xprsyms.Add(pred.funargs[1], 0)
			// manipulate values of these symbols while calling body code; accum result
			if len(pred.body.args) > 1 {
				return genError("locals not allowed in xprftn maplst calls", expr)
			}
			bodystmt := pred.body.args[0]
			for i, a := range lst0.args {
				pred.funargs[0].binding = a
				pred.funargs[1].binding = makeIntTerm2(i) // this is likely wasted effort
				tmp := interpXprExpr(bodystmt, xpi)
				if tmp.Tag() == ErrorTag {
					return tmp
				}
				if tmp == TrueLiteral {
					xpi.xprsyms.Remove(pred.funargs[1])
					xpi.xprsyms.Remove(pred.funargs[0])
					if fnsym.ident == "filt1x" {
						return pred.funargs[1].binding
					} else {
						return a
					}
				}
			}
			xpi.xprsyms.Remove(pred.funargs[1])
			xpi.xprsyms.Remove(pred.funargs[0])
			return nilEntity
		}
	case "parseRW":
		// parseRW(X) is interpreted at pattern compile time if before MQ else at runtime. The runtime
		// case is when the arg is a stringlit, otherwise it's some other term.
		parsearg := expr.args[0]
		if parsearg.Tag() == Stringlit {
			strg := parsearg.(*TermB).value
			parsetrm := parseString(strg)
			// todo: stop ignoring stx errors
			tci := makeTCInfo(gblScope)
			if len(xpi.funargs) > 0 {
				argscope := makeScope()
				for _, a := range xpi.funargs {
					argscope.Add(a, 0)
				}
				tci.PushScope(argscope)
			}
			tci.inXprftn = true
			xprscope := makeScope()
			// note: must copy in case TC mods the scope
			copysyms := make(SymXDict, len(xpi.xprsyms))
			copy(copysyms, xpi.xprsyms)
			xprscope.entries = copysyms
			tci.PushScope(xprscope)
			tci.cxt.Push(SymbolPattern)
			parsed := parsetrm.Typecheck(nil, tci)
			tci.cxt.Pop()
			tci.PopScope()
			if parsed.Tag() == ErrorTag {
				return parsed
			}
			parsearg = parsed
		} else {
			parsearg = termCopy(parsearg)
		}
		// here for the pre-MQ case: subst in the bindings of matchables
		b4 := make([]Term, len(xpi.xprsyms))
		aftr := make([]Term, len(xpi.xprsyms))
		for i, sx := range xpi.xprsyms {
			b4[i] = sx.sym
			aftr[i] = sx.sym.binding
		}
		substd := runSubstn(b4, aftr, parsearg, false)
		if substd.Tag() == Stmts {
			flattenStmts1(substd)
			substd.(*TermL).scope.parent = xpi.smi.scopetree
		}
		// Copying might be needed but I can't convince myself that it is and doing it with visit in this way causes
		// a subtle problem, see notes at 09/21/21.
		//substd = xpi.smi.visit(substd)
		return substd
	case "subst":
		trm := interpXprExpr(expr.args[0], xpi)
		lst0 := interpXprExpr(expr.args[1], xpi)
		lst1 := interpXprExpr(expr.args[2], xpi)
		if lst0.Tag() != Litform0 || lst1.Tag() != Litform0 {
			return genError("non-manifest arg", expr)
		}
		//fmt.Println("runSubstn on b4=", lst0.String(), "aftr=", lst1.String())
		return runSubstn(termCopy(lst0).(*TermL).args, termCopy(lst1).(*TermL).args, trm, true)
	case "typesubst":
		subj := interpXprType(expr.args[0], xpi)
		b4 := interpXprType(expr.args[1], xpi)
		aftr := interpXprType(expr.args[2], xpi)
		if subj.Tag() != TypeTag || b4.Tag() != TypeTag || aftr.Tag() != TypeTag {
			return genError("non-type arg", expr)
		}
		return (&typesubstInfo{b4.(*Type), aftr.(*Type), make([]*Type, 0, 2)}).typeSubstn(subj.(*Type))
	case "pattern":
		return expr // pattern calls can occur since we cull var/val decls without CDFA but patterns are always compiled at TC time.
	case "disjoint":
		// disjoint is a binop on lists of xprterms, true if they're disjoint as sets.
		// I don't have a convenient way to order terms so for now, I'll use the naive algo.
		lst1 := interpXprExpr(expr.args[0], xpi)
		lst2 := interpXprExpr(expr.args[1], xpi)
		if lst1.Tag() == Litform0 && lst2.Tag() == Litform0 {
			ret := true
			for _, x := range lst1.(*TermL).args {
				for _, y := range lst2.(*TermL).args {
					if x.Equal(y) {
						ret = false
						break
					}
				}
			}
			if ret {
				return TrueLiteral
			} else {
				return FalseLiteral
			}
		} else {
			return genError("disjoint gets non-manifest arguments", expr)
		}
	case "usescount":
		trm := interpXprExpr(expr.args[0], xpi)
		if trm.Tag() == ErrorTag {
			return trm
		}
		inst := interpXprExpr(expr.args[1], xpi)
		if inst.Tag() == ErrorTag {
			return inst
		}
		return usescount(trm, inst)
	case "in":
		lst := interpXprExpr(expr.args[0], xpi)
		elt := interpXprExpr(expr.args[1], xpi)
		if lst.Tag() == Litform0 && elt.Tag() != ErrorTag {
			for _, x := range lst.(*TermL).args {
				if x.Equal(elt) {
					return TrueLiteral
				}
			}
		}
		return FalseLiteral
	case "stringify":
		// implement a few stringify options
		trm := interpXprExpr(expr.args[0], xpi)
		if trm.Tag() == ErrorTag {
			return trm
		}
		simplified := Simplify(trm)
		if !Manifest(simplified) {
			return genError("non-manifest: "+simplified.String(), simplified)
		}
		// definitely some holes in this...
		s0 := simplified.(*TermB)
		stringVersion := makeStringTerm(s0.value, s0.first, false)
		return stringVersion
	case "manifest":
		trm := interpXprExpr(expr.args[0], xpi)
		if trm.Tag() == ErrorTag {
			return trm
		}
		if trm.Tag() == TypeTag {
			if trm.(*Type).family == TFTypred {
				return FalseLiteral
			} else {
				return TrueLiteral
			}
		} else {
			simplified := Simplify(trm)
			if simplified.Tag() == SymbolTag && simplified.Plist().Find("var") == nil && simplified.(*Symbol).binding != nil {
				simplified = simplified.(*Symbol).binding
			}
			retval := FalseLiteral
			if simplified.Tag() == Litform0 || Manifest(simplified) {
				retval = TrueLiteral
			}
			return retval
		}
	case "exit": // do a cntld exit as called for in the rwrule
		errmsg := interpXprExpr(expr.args[0], xpi)
		panic(errmsg)
	case "rewriteStoreGet":
		trm := interpXprExpr(expr.args[0], xpi)
		if prRewrite {
			logger.Println("rwsGet:", expr.args[0].String(), ":", trm.String())
		}
		return trm
	case "rewriteStoreSet":
		if len(expr.args) == 1 {
			// mod funcall
			interpXprExpr(expr.args[0], xpi)
		} else {
			// asgnmt
			rhs := interpXprExpr(expr.args[1], xpi)
			if rhs.Tag() == ErrorTag {
				return rhs
			}
			lhs := expr.args[0]
			if lhs.Tag() == SymbolTag {
				sym0 := lhs.(*Symbol)
				symrws, _ := xpi.smi.rewriteStore.Lookup(sym0.ident)
				if symrws != nil {
					symrws.binding = rhs
				} else {
					return genError(sym0.ident+" not found in rewriteStore", expr)
				}
			} else if callingNamed(lhs, "rvalindex") {
				funargs := lhs.(*TermTL).args
				lstarg := interpXprExpr(funargs[0], xpi)
				if lstarg.Tag() != SymbolTag {
					return genError("first arg to rvalindex must be a symbol", expr)
				}
				symrws, _ := xpi.smi.rewriteStore.Lookup(lstarg.(*Symbol).ident)
				if symrws == nil {
					return genError(lstarg.(*Symbol).ident+" not found in rewriteStore", expr)
				}
				inx := interpXprExpr(funargs[1], xpi)
				if inx.Tag() != Intlit {
					return genError("rewriteStoreSet: index not manifest", expr)
				}
				inxval := inx.Plist().Find("intval").(int)
				curbdg := lstarg.(*Symbol).binding
				if curbdg.Tag() != Litform0 {
					return genError("cannot assign non-manifest list", expr)
				}
				curbdg0 := curbdg.(*TermL)
				if inxval < 0 || inxval >= len(curbdg0.args) {
					return genError("index out of range", expr)
				}
				curbdg0.args[inxval] = rhs
			} else {
				return genError("bad arg to rewriteStoreSet", expr)
			}
		}
		return nilEntity // ??
	case "defineGlobal":
		tg := expr.args[0].Tag()
		var arg Term
		if tg == SymbolTag {
			arg = expr.args[0].(*Symbol).binding
		} else {
			arg = interpXprExpr(expr.args[0], xpi)
			if arg.Tag() == ErrorTag {
				return arg
			}
		}
		if arg.Tag() != SymbolTag {
			return genError("symbol expected", arg)
		}
		sym0 := arg.(*Symbol)
		if prRewrite {
			logger.Println("defineGlobal:", sym0.ident, "=", sym0.binding.String())
		}
		gblScope.Add(sym0, 0)
		return nilEntity
	case "anyConnected":
		// todo: code connectedness for real
		return FalseLiteral
	}
	panic("impossible")
}

// interpret a single expr in an xprftn. These are all based on funcalls, so the arg is typed as such.
// Also, we assume the ftns are Symbols.
func interpXprExpr(expr Term, xpi *XPRInfo) Term {
	// local ftn to run stmtlists in if stmts
	var runpart = func(stmts []Term) Term {
		lastinx := len(stmts) - 1
		var rslt Term
		for i, s := range stmts {
			stg := s.Tag()
			// fmt.Println("running nested stmt:", s.String())
			if stg == RwsDeclStmt {
				return genError("statement "+s.String()+" may not occur nested in rewrite rules", s)
			} else if callingNamed(s, "skipIf") {
				skipIfResult := xpi.evalSkipIf(s)
				if skipIfResult.Tag() == ErrorTag {
					return skipIfResult
				}
				if skipIfResult == TrueLiteral { // this is an otherwise impossible Term recognized by caller
					return makeTermB(Boollit, "skip", Pos(-1))
				}
			} else if stg == Valdecl || stg == Vardecl {
				sym := s.(*TermT).arg0.(*Symbol)
				xpi.locals = append(xpi.locals, sym)
				xpi.priorBindings = append(xpi.priorBindings, sym.binding)
				tmp := interpXprExpr(s, xpi)
				if tmp.Tag() == ErrorTag {
					return tmp
				}
			} else {
				tmp := interpXprExpr(s, xpi)
				if tmp.Tag() == ErrorTag {
					return tmp
				}
				if lastinx == i {
					rslt = tmp
				}
			}
		}
		return rslt
	}
	tg := expr.Tag()
	switch expr0 := expr.(type) {
	case *Type:
		if expr0.family == TFGTS {
			expr0 = expr0.v.(*GTS).binding
		}
		return expr0
	case *Symbol:
		// if a matchable or local, eval to the binding (checking it isn't nil) else eval to the symbol itself.
		// If local, also eval the binding.
		sym0 := xpi.xprsyms.Find(expr0.ident)
		if !xpi.precompile {
			symrws, _ := xpi.smi.rewriteStore.Lookup(expr0.ident)
			if symrws != nil {
				return symrws
			}
			// deref funargs
			for _, s := range xpi.funargs {
				if expr0.ident == s.ident {
					return s.binding
				}
			}
		}
		var sym1 *Symbol
		for _, s := range xpi.locals {
			if s.ident == expr0.ident {
				sym1 = s
				break
			}
		}
		if sym0 == expr0 || sym1 == expr0 {
			if sym1 == expr0 {
				sym0 = sym1
			}
			if xpi.precompile {
				if sym0.binding != nil {
					tmp := interpXprExpr(sym0.binding, xpi)
					if tmp.Tag() != ErrorTag {
						sym0.binding = tmp
					} else {
						expr = tmp
					}
				}
				return expr
			}
			if sym0.binding == nil {
				return genError(fmt.Sprintf("matchable %s is not bound when referenced", sym0.ident), expr)
			} else {
				ret := sym0.binding
				// I'm not sure if this is always uninterp'd; I suspect so.
				// At any rate, xprstmts as flex matchables need special handling here
				if ret == nil {
					return genError(fmt.Sprintf("unbound matchable %s", sym0.ident), sym0)
				}
				return ret
			}
		} else {
			// not matchable or local, treat as a constant
			return expr0
		}
	case *TermTL:
		if tg == Funcall {
			fnsym := interpXprExpr(expr0.trm, xpi) // guaranteed to be a symbol, or error
			if fnsym.Tag() == ErrorTag {
				return fnsym
			}
			if fnsym.Tag() != SymbolTag || fnsym.(*Symbol).ident == "_" {
				for i, a := range expr0.args {
					tmp := interpXprExpr(a, xpi)
					if tmp.Tag() == ErrorTag {
						return tmp
					}
					expr0.args[i] = tmp
				}
				return expr0
			}
			ftyp := fnsym.Dtype().v.(*Ftntype)
			if ftyp.rettype.family == TFList && !xpi.precompile {
				return interpXprListfunc(expr0, xpi)
			} else {
				return interpXprFuncall(expr0, xpi)
			}
		} else {
			// must be Litform
			return interpXprListfunc(expr0, xpi)
		}
	case *TermB:
		// baselits
		return expr
	case *TermT:
		lhs := expr0.arg0
		switch tg {
		case Valdecl, Vardecl:
			if xpi.precompile {
				return genError("cannot occur in xprftn", expr0)
			} else {
				sym := lhs.(*Symbol)
				// before executing the binding step, check and save if not already saved.
				lastinx := len(xpi.eachSymbols)
				if lastinx > 0 {
					syms := xpi.eachSymbols[lastinx-1]
					bdgs := xpi.eachBindings[lastinx-1]
					syminx := -1
					for i, s := range syms {
						if s == sym {
							syminx = i
							break
						}
					}
					if syminx < 0 {
						syms = append(syms, sym)
						xpi.eachSymbols[lastinx-1] = syms
						bdgs = append(bdgs, sym.binding)
						xpi.eachBindings[lastinx-1] = bdgs
					}
				}
				sym.binding = interpXprExpr(sym.binding, xpi)
				if sym.binding.Tag() == ErrorTag {
					return sym.binding
				}
				sym.dtype = sym.binding.Dtype() // In this case, it can be e.g. xprterm before and a "real" type after
			}
		case Lognot:
			lhs2 := interpXprExpr(lhs, xpi)
			if lhs2.Tag() == ErrorTag {
				return lhs2
			}
			if lhs2.Tag() == Boollit {
				if lhs2 == TrueLiteral {
					return FalseLiteral
				} else {
					return TrueLiteral
				}
			}
			expr0.arg0 = lhs2
		case Gdref:
			return interpXprExpr(expr0.arg0, xpi)
		case Negate:
			lhs := interpXprExpr(lhs, xpi)
			if lhs.Tag() == ErrorTag {
				return lhs
			}
			if lhs.Tag() == Intlit || lhs.Tag() == Floatlit {
				return Simplify(makeTermT(tg, lhs, expr0.dtype, expr0.first, expr0.final))
			}
			expr0.arg0 = lhs
		}
		return expr
	case *TermTT:
		// eq, etc.
		switch tg {
		case EqExpr, EqeqExpr:
			lhs := interpXprExpr(expr0.arg0, xpi)
			if lhs.Tag() == ErrorTag {
				return lhs
			}
			rhs := interpXprExpr(expr0.arg1, xpi)
			if rhs.Tag() == ErrorTag {
				return rhs
			}
			_, isTB := lhs.(*TermB)
			if lhs == rhs || (isTB && lhs.Equal(rhs)) {
				return TrueLiteral
			} else {
				return FalseLiteral
			}
		case AsgnStmt:
			if expr0.arg0.Tag() != SymbolTag {
				return genError("assignment expects symbol", expr0)
			}
			sym := expr0.arg0.(*Symbol)
			tmp := interpXprExpr(expr0.arg1, xpi)
			if !xpi.precompile {
				sym.binding = tmp
			}
			return expr
		}
	case *TermL:
		if xpi.precompile {
			for i, x := range expr0.args {
				x0 := interpXprExpr(x, xpi)
				if x0.Tag() == ErrorTag {
					return x0
				}
				expr0.args[i] = x0
			}
		} else if tg == IfStmt {
			cond := interpXprExpr(expr0.args[0], xpi)
			if cond.Tag() == ErrorTag {
				return cond
			}
			if cond.Tag() != Boollit {
				return genError("non-manifest if condition in xprftn", expr0)
			}
			if cond == TrueLiteral {
				return runpart(expr0.args[1].(*TermL).args)
			} else if len(expr0.args) == 3 {
				return runpart(expr0.args[2].(*TermL).args)
			}
		} else if tg == OrorExpr || tg == AndandExpr {
			rslt := TrueLiteral
			if tg == OrorExpr {
				rslt = FalseLiteral
			}
			for _, x := range expr0.args {
				x0 := interpXprExpr(x, xpi)
				if x0.Tag() == ErrorTag {
					return x0
				}
				if x0.Tag() != Boollit {
					return genError("non-manifest arg", expr0)
				}
				if tg == OrorExpr && x0 == TrueLiteral {
					return TrueLiteral
				}
				if tg == AndandExpr && x0 == FalseLiteral {
					return FalseLiteral
				}
			}
			return rslt
		} else if tg == Litform0 {
			argsx := make([]Term, len(expr0.args))
			for i, x := range expr0.args {
				tmp := interpXprExpr(x, xpi)
				if tmp.Tag() == ErrorTag {
					return tmp
				}
				argsx[i] = tmp
			}
			expr = makeTermL(tg, argsx, expr0.dtype, expr0.first, expr0.final)
		} else if tg == EachStmt {
			// here, run the each stmt
			// the other vbl binding ops (maplst, etc) use xprsyms, I'm not sure why but I'm imitating
			itervar := expr0.args[0].(*Symbol)
			xpi.xprsyms.Add(itervar, 0)
			var inxsym *Symbol
			if len(expr0.args) == 4 {
				inxsym = expr0.args[3].(*Symbol)
				xpi.xprsyms.Add(inxsym, 0)
			}
			lst := interpXprExpr(expr0.args[1], xpi)
			ltg := lst.Tag()
			if ltg == ErrorTag {
				return lst
			}
			if ltg == Range && lst.(*TermT).arg0.Tag() == Litform0 {
				// check if manifest and if so, generate the litform
				// for now, limit to integer ranges
				rngvals := lst.(*TermT).arg0.(*TermL).args
				r0 := interpXprExpr(rngvals[0], xpi)
				r1 := interpXprExpr(rngvals[1], xpi)
				r2 := interpXprExpr(rngvals[2], xpi)
				r3 := interpXprExpr(rngvals[3], xpi)
				// Since requiring integer range, use Intlit checks instead of Manifest
				allManif := r0.Tag() == Intlit && r1.Tag() == Intlit && r2.Tag() == Intlit && Manifest(r3)
				if allManif {
					rnglist := make([]Term, 0, 4)
					top := r1.Plist().Find("intval").(int)
					incr := r2.Plist().Find("intval").(int)
					if r3.(*TermB).value == "true" {
						top += 1
					}
					for i := r0.Plist().Find("intval").(int); i < top; i += incr {
						rnglist = append(rnglist, makeIntlit(i))
					}
					ltg = Litform0
					lst = makeTermL(Litform0, rnglist, makeListType(TypeInt), Pos(-1), Pos(-1))
				}
			}
			if ltg != Litform0 {
				return genError("non-manifest list to each", expr0.args[1])
			}
			lst0 := lst.(*TermL)
			body := expr0.args[2].(*TermL).args
			xpi.enterEach()
			for inxval := 0; inxval < len(lst0.args); inxval++ {
				if inxsym != nil {
					inxsym.binding = makeIntlit(inxval)
				}
				itervar.binding = lst0.args[inxval]
				for _, s := range body {
					stg := s.Tag()
					if stg == Valdecl || stg == Vardecl {
						sym := s.(*TermT).arg0.(*Symbol)
						xpi.locals = append(xpi.locals, sym)
						xpi.priorBindings = append(xpi.priorBindings, sym.binding)
					}
					interpXprExpr(s, xpi)
				}
				// restore bindings of locals
				savedinx := len(xpi.eachSymbols) - 1
				svbdgs := xpi.eachBindings[savedinx]
				for i, s := range xpi.eachSymbols[savedinx] {
					s.binding = svbdgs[i]
				}
			}
			xpi.exitEach()
			xpi.xprsyms.Remove(itervar)
			if inxsym != nil {
				xpi.xprsyms.Remove(inxsym)
			}
			return nilEntity
		} else if tg == Symchain {
			// each elt but the last should eval to a litform with a tuptype; eval in turn
			// Helper called in 2 spots, return value of appropriate attrib in litform lfm
			var deref = func(lfm *TermL, attrib *Symbol) Term {
				atrbs := lfm.dtype.v.(*Tupletype)
				for i, a := range atrbs.attribs {
					if a.ident == attrib.ident {
						return lfm.args[i]
					}
				}
				return genError("attribute"+attrib.ident+"not found", lfm)
			}
			var lfm *TermL
			rootsym := expr0.args[0].(*Symbol)
			if rootsym.binding.Tag() == Litform0 {
				lfm = rootsym.binding.(*TermL)
			}
			lastattrib := expr0.args[len(expr0.args)-1]
			for i := 1; i < len(expr0.args)-1; i++ {
				if lfm == nil {
					return genError("non-manifest value to symchain", expr0.args[i-1])
				}
				asym := expr0.args[i].(*Symbol)
				atrb := deref(lfm, asym)
				if atrb.Tag() == Litform0 {
					lfm = atrb.(*TermL)
				} else {
					if atrb.Tag() != ErrorTag {
						atrb = genError("non-manifest attribute value on", expr0)
					}
					return atrb
				}
			}
			return deref(lfm, lastattrib.(*Symbol))
		} else {
			panic("not reached")
		}
		return expr
	}
	panic("not reached")
}

// Given a term already determined to be a skipIf call, eval and return error, TrueLiteral, or nilEntity
func (xpi *XPRInfo) evalSkipIf(trm Term) Term {
	fcall := trm.(*TermTL)
	cond := Simplify(interpXprExpr(fcall.args[0], xpi))
	if cond.Tag() == ErrorTag {
		return cond
	}
	if cond.Tag() != Boollit {
		return genError("non-manifest if condition in skipIf", trm)
	}
	if cond == TrueLiteral {
		return cond
	}
	return nilEntity
}

// interpret an xprftn; will be called when it matches.
// This is where we interpret declaration of locals, skipIf, if exprs, final output terms.
func (xpi *XPRInfo) interpXprftn(trm Term, xprftn *Funinst) (rslt Term, changed bool) {
	// shallow copy body scope because interpr can change xpi.xprsyms
	bodysyms := xprftn.body.scope.entries
	xprsyms0 := make(SymXDict, len(bodysyms))
	copy(xprsyms0, bodysyms)
	xpi.setup(xprsyms0)
	xpi.funargs = xprftn.funargs
	lastinx := len(xprftn.body.args) - 1
	rslt = trm
	changed = true
	for i, stmt := range xprftn.body.args {
		// fmt.Println("running stmt:", stmt.String())
		if callingNamed(stmt, "skipIf") {
			skipIfResult := xpi.evalSkipIf(stmt)
			if skipIfResult == TrueLiteral {
				if prRewrite {
					logger.Println("failing skipIf on stmt", i, "of", len(xprftn.body.args))
					ebm := xpi.xprsyms.Find("EBM")
					if ebm != nil {
						s2 := xpi.xprsyms.Find("S2")
						s3 := xpi.xprsyms.Find("S3")
						logger.Println("EBM:", ebm.binding.String(), "; S2:", s2.binding.String(), "; S3:", s3.binding.String())
					}
				}
				changed = false
				break
			}
			if skipIfResult.Tag() == ErrorTag {
				rslt = skipIfResult
				break
			}
		} else if stmt.Tag() == Valdecl || stmt.Tag() == Vardecl {
			sym := stmt.(*TermT).arg0.(*Symbol)
			xpi.locals = append(xpi.locals, sym)
			xpi.priorBindings = append(xpi.priorBindings, sym.binding)
			tmp := interpXprExpr(stmt, xpi)
			if tmp.Tag() == ErrorTag {
				rslt = tmp
				return
			}
		} else if stmt.Tag() == RwsDeclStmt {
			sym := stmt.(*TermT).arg0.(*Symbol)
			xstg, _ := xpi.smi.rewriteStore.Lookup(sym.ident)
			if xstg == nil {
				xpi.smi.rewriteStore.Add(sym, 0)
			}
		} else {
			tmp := interpXprExpr(stmt, xpi)
			if tmp.Tag() == ErrorTag {
				rslt = tmp
				return
			}
			// for nested skipIf that's true, interpXprExpr returns a fake Boollit with value == "skip"
			if tmp.Tag() == Boollit && tmp.(*TermB).value == "skip" {
				changed = false
				break
			}
			if i == lastinx {
				rslt = tmp
			}
		}
	}
	xpi.cleanup()
	if changed {
		flattenStmts1(rslt)
	}
	return
}
