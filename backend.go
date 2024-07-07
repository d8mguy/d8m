// basic definitions for backend of compiler

package main

import (
	"fmt"
	"log"
	"strings"
	"time"
)

// Start with a set of ftns to support dataflow analysis, both in rewrite rules and hard coded backend.
// Use TraversePre to generate a map of Symbol and Symchain uses. Possibly more later.
type umapT = map[*Symbol]bool
type umapWFInfo struct {
	coll umapT
	seen []*Symbol // infloop preventer
	mods bool
}

// Add sym to coll if not already present
func (uwf *umapWFInfo) AddIfSym(trm Term) {
	var sym *Symbol
	if trm.Tag() == SymbolTag {
		sym = trm.(*Symbol)
	} else if trm.Tag() == Symchain {
		ltrm := trm.(*TermL).args[0]
		if ltrm.Tag() == SymbolTag {
			sym = ltrm.(*Symbol)
		}
	}
	if sym != nil {
		uwf.coll[sym] = true
	}
}

func (uwfi *umapWFInfo) seensym(sym *Symbol) bool {
	for _, s := range uwfi.seen {
		if s == sym {
			return true
		}
	}
	return false
}

// After a usesmap is generated, the ftns in the xprftn interpreter need it as a list of Terms
func (uwfi *umapWFInfo) unwrap() []Term {
	retval := make([]Term, 0, len(uwfi.coll))
	for v, _ := range uwfi.coll {
		retval = append(retval, v)
	}
	return retval
}

// The TraversePre-compatible work function for usesdict.
// Traverse bindings of locally defined symbols, not others. Do this by direct call on bindings.
func umapWF(trm Term, state interface{}) (retval int) {
	st0 := state.(*umapWFInfo)
	tg := trm.Tag()
	if tg == SymbolTag {
		sym := trm.(*Symbol)
		if sym.dtype != nil && sym.dtype.family == TFFtn && itemExists(sym, "recursive") {
			if st0.seensym(sym) {
				return 1
			}
			st0.seen = append(st0.seen, sym)
		}
	}
	if tg == Valdecl || tg == Vardecl {
		// neither uses nor mods but we must traverse the binding of the local, if any.
		sym := trm.(*TermT).arg0.(*Symbol)
		if sym.binding != nil && !st0.seensym(sym) {
			st0.seen = append(st0.seen, sym)
			TraversePre(sym.binding, umapWF, state)
		}
		retval = 1
	} else if st0.mods {
		if tg == AsgnStmt {
			st0.AddIfSym(trm.(*TermTT).arg0)
			retval = 1
		} else if tg == Funcall {
			trm0 := trm.(*TermTL)
			ftyp := trm0.trm.Dtype()
			if !ftyp.v.(*Ftntype).pure {
				st0.AddIfSym(trm0.args[0])
				// traverse other args by hand
				for _, a := range trm0.args[1:] {
					TraversePre(a, umapWF, state)
				}
				retval = 1
			}
		}
	} else {
		st0.AddIfSym(trm)
		if tg == SymbolTag {
			// This isn't the declaration of the Symbol, so don't traverse the binding.
			// For larger files, omitting this policy can cause the compiler to slow down drastically, 200x or more.
			retval = 1
		}
	}
	return
}

func usemod(trm Term, trackMods bool) []Term {
	state := &umapWFInfo{make(umapT, 20), nil, trackMods}
	TraversePre(trm, umapWF, state)
	return state.unwrap()
}

type ucountInfo struct {
	val       int
	sought    Term
	tagSought Termtag
}

// The TraversePre-compatible work function for usescount. Check each subterm for equality with the
// sought term and investigate everywhere except the bindings of non-local symbols.
func ucountWF(trm Term, state interface{}) int {
	st0 := state.(*ucountInfo)
	retval := 0
	tg := trm.Tag()
	if tg == st0.tagSought {
		if trm.Equal(st0.sought) {
			st0.val++
			return 3 // ensure that this term is pursued no further
		}
	}
	if tg == SymbolTag {
		retval = 1 // don't check binding
	} else if tg == Valdecl || tg == Vardecl {
		sym := trm.(*TermT).arg0.(*Symbol)
		if sym.binding != nil {
			TraversePre(sym.binding, ucountWF, st0)
		}
		retval = 1
	}
	return retval
}

// Return an Intlit with the count of inst in trm.
func usescount(trm, inst Term) *TermB {
	st0 := ucountInfo{0, inst, inst.Tag()}
	TraversePre(trm, ucountWF, &st0)
	return makeIntTerm2(st0.val)
}

// Return a list of symbols for local definitions. trm should be a Stmts.
// Defs is defined in the spec as not going into inner scopes, so no need for a tree style traversal here.
func defslist(trm Term) []Term {
	rslt := make([]Term, 0, 2)
	if trm.Tag() == Stmts {
		for _, s := range trm.(*TermL).args {
			if s.Tag() == Valdecl || s.Tag() == Vardecl {
				rslt = append(rslt, s.(*TermT).arg0)
			}
		}
	}
	return rslt
}

// Mostly a place to hold substn term lists so we can conveniently pass them around. See comments
// below for checkAsgn; it's used in one case of inlining, not in normal substn. The looseFtnCheck slot
// enables a second hack, which lets constructed generic ftn names match in the subst ftn of interpXprftn.
// This is arguably cheating, its long term future is TBD.
type substInfo struct {
	b4, aftr      []Term
	counts        []int
	checkAsgn     bool
	looseFtnCheck bool
}

func runSubstn(b, a []Term, trm Term, lfc bool) Term {
	si0 := &substInfo{b, a, nil, false, lfc}
	return si0.substn(trm)
}

// LooseEquals is for when looseFtnCheck is set in the substn state var.
func looseEquals(trm, other Term) bool {
	ttg := trm.Tag()
	otg := other.Tag()
	if ttg != otg {
		return false
	}
	switch trm0 := trm.(type) {
	case *TermB:
		return trm0.Equal(other)
	case *Symbol:
		osym := other.(*Symbol)
		seen := []*Type{}
		if !(trm0.ident == osym.ident && trm0.dtype.equal(osym.dtype, &seen)) {
			return false
		}
		return (trm0.binding == nil && osym.binding == nil) || (trm0.binding != nil && looseEquals(trm0.binding, osym.binding))
	case *Funinst:
		ofn := other.(*Funinst)
		if trm0.body == nil && ofn.body != nil || (trm0.body != nil && ofn.body == nil) {
			return false
		}
		if trm0.body != nil {
			return looseEquals(trm0.body, ofn.body)
		}
	case *TermT:
		ott := other.(*TermT)
		return (trm0.arg0 == nil && ott.arg0 == nil) || (trm0.arg0 != nil && looseEquals(trm0.arg0, ott.arg0))
	case *TermTT:
		ottt := other.(*TermTT)
		return looseEquals(trm0.arg0, ottt.arg0) && looseEquals(trm0.arg1, ottt.arg1)
	case *TermL:
		otl := other.(*TermL)
		if len(trm0.args) != len(otl.args) {
			return false
		}
		for i, a := range trm0.args {
			if !looseEquals(a, otl.args[i]) {
				return false
			}
		}
	case *TermTL:
		ottl := other.(*TermTL)
		if trm0.trm.Tag() == SymbolTag && ottl.trm.Tag() == SymbolTag {
			if !(trm0.trm.(*Symbol).ident == ottl.trm.(*Symbol).ident) {
				return false
			}
		} else if !looseEquals(trm0.trm, ottl.trm) {
			return false
		}
		if len(trm0.args) != len(ottl.args) {
			return false
		}
		for i, a := range trm0.args {
			if !looseEquals(a, ottl.args[i]) {
				return false
			}
		}
	default:
		panic("can't happen")
	}
	return true
}

// Substn needs to run the substitution checks "on the way down" and assemble the possibly changed term
// on the way back up. So I'm rolling a recursive traversal ftn to do that.
// Note that this substn will never go into bindings of symbols except when they're being defined.
// Also, substn always smashes (parts of) the trm given and returns it, unless trm equals one of the b4
// Terms in the substInfo entity. This means that in general, there's no way for the caller to tell from
// the return value if anything changed. For this and other reasons, if substInfo.counts is non-nil, it
// should be a []int of the same length as si.b4 initialized to zeros; the substn method will incr the
// corresponding one of these when a substn on that one is done. Thus, if counts is non-nil, the sum of its
// elements will be the # of changes after runSubstn.
func (si *substInfo) substn(trm Term) Term {
	for i, x := range si.b4 {
		// this is where looseFtnCheck happens. Run a more relaxed form of Equal on the funcalls
		if (si.looseFtnCheck && looseEquals(trm, x)) || trm.Equal(x) {
			if si.counts != nil {
				si.counts[i]++
			}
			return si.aftr[i]
		}
		// the single line above does most of the work, now handle a special case with Symchains, namely
		// to make a substn like a.b match terms like a.b.c.
		if trm.Tag() == Symchain && x.Tag() == Symchain {
			trm0 := trm.(*TermL)
			x0 := x.(*TermL)
			if len(trm0.args) > len(x0.args) {
				pmatch := true
				for j, y := range x0.args {
					if !trm0.args[j].Equal(y) {
						pmatch = false
						break
					}
				}
				if pmatch {
					if si.counts != nil {
						si.counts[i]++
					}
					rslt := si.aftr[i]
					if rslt.Tag() == Symchain {
						rslt0 := rslt.(*TermL)
						cnt0 := len(rslt0.args)
						nargs := make([]Term, cnt0+len(trm0.args)-len(x0.args))
						copy(nargs, rslt0.args)
						copy(nargs[cnt0:], trm0.args[len(x0.args):])
						return makeTermL(Symchain, nargs, trm0.dtype, Pos(-1), Pos(-1))
					} else {
						nargs := append([]Term{rslt}, trm0.args[len(x0.args):]...)
						return makeTermL(Symchain, nargs, trm0.dtype, Pos(-1), Pos(-1))
					}
				}
			}
		}
	}
	// types not mentioned below don't get traversed
	switch trm0 := trm.(type) {
	case *Funinst:
		if trm0.body != nil {
			trm0.body = si.substn(trm0.body).(*TermL) // possibly a little too trusting?
		}
	case *TermT:
		// traverse bindings of locals
		if trm0.kind == Valdecl || trm0.kind == Vardecl {
			sym := trm0.arg0.(*Symbol)
			if sym.binding != nil {
				sym.binding = si.substn(sym.binding)
			}
		} else if trm0.kind == Gdref {
			trm0.arg0 = si.substn(trm0.arg0)
			// substn can willy-nilly make the arg0 of a Gdref more specific than the Gdref; detect and undo this
			if trm0.dtype.family == TFTypred && trm0.arg0.Dtype().family != TFTypred {
				return trm0.arg0
			}
		} else if trm0.arg0 != nil {
			trm0.arg0 = si.substn(trm0.arg0)
		}
	case *TermTT:
		if si.checkAsgn && trm0.kind == AsgnStmt {
			// these conditions ensure that callingNamed(si.aftr[0], "rvalindex") is true.
			// If it so happens that trm0.arg0 == si.b4[0] then instead of normal processing, rewrite to lvalindex.
			// Note that == must work because si.b4[0] must be a fmlarg, ie a Symbol.
			if trm0.arg0 == si.b4[0] {
				rvicall := si.aftr[0].(*TermTL)
				if si.counts != nil {
					si.counts[0]++
				}
				rviRcvrType := rvicall.args[0].Dtype() // look up the lvalindex method here
				lvi := rviRcvrType.methodLookup("lvalindex")
				if lvi.Tag() != SymbolTag {
					panic("should not happen")
				}
				return makeTermTL(Funcall, lvi, []Term{rvicall.args[0], rvicall.args[1], si.substn(trm0.arg1)}, TypeNothing, rvicall.first, rvicall.final)
			}
		}
		trm0.arg0 = si.substn(trm0.arg0)
		trm0.arg1 = si.substn(trm0.arg1)
	case *TermL:
		for i, a := range trm0.args {
			trm0.args[i] = si.substn(a)
			if trm0.kind == Symchain {
				if trm0.args[i].Tag() == Symchain {
					// whoops, straighten this out by copying.
					chain0 := trm0.args
					rootchain := trm0.args[i].(*TermL).args
					xargs := make([]Term, len(rootchain)+len(chain0)-1)
					copy(xargs, rootchain)
					copy(xargs[len(rootchain):], chain0[1:])
					trm0.args = xargs
				}
				break // do not subst in the attributes, only the root
			}
		}
		// fix up nested symchains
		if trm0.kind == Symchain && trm0.args[0].Tag() == Symchain {
			attribs := trm0.args[1:]
			nested := trm0.args[0].(*TermL)
			nargs := append(nested.args, attribs...)
			trm0.args = nargs
			//fmt.Println("fixed up nested symchain:", trm0.String())
		}
	case *TermTL:
		trm0.trm = si.substn(trm0.trm)
		for i, a := range trm0.args {
			trm0.args[i] = si.substn(a)
		}
	}
	return trm
}

// Type substn is similar to Term substn. One diff is that we only need to substitute one at a time.
// This is the info struct, which handles the substn args plus infloop prevention.
type typesubstInfo struct {
	b4, aftr *Type
	seen     []*Type
}

// Traverse typ and substitute aftr for b4 in tsi. Return is == typ if no substns are done.
// The convention is that new structure is returned explicitly; the final return at the end is for default case.
func (tsi *typesubstInfo) typeSubstn(typ *Type) *Type {
	if typ == tsi.b4 {
		return tsi.aftr
	}
	for i := 0; i < len(tsi.seen); i += 2 {
		if typ == tsi.seen[i] {
			return tsi.seen[i+1]
		}
	}
	switch typ.family {
	case TFList:
		eltyp := typ.v.(*Type)
		t0 := tsi.typeSubstn(eltyp)
		if t0 != eltyp {
			ntyp := makeListType(t0)
			tsi.seen = append(tsi.seen, typ, ntyp)
			return ntyp
		}
	case TFSpace:
		spacet := typ.v.(*Spacetype)
		t0 := tsi.typeSubstn(spacet.elttype)
		if t0 != spacet.elttype {
			return makeSpaceType(t0, spacet.dimensions)
		}
	case TFTuple:
		ttype := typ.v.(*Tupletype)
		changed := false
		attribs := make([]*Symbol, len(ttype.attribs))
		ntyp := makeType0(TFTuple, &Tupletype{attribs, ttype.situated}, "")
		tsi.seen = append(tsi.seen, typ, ntyp)
		for i, s := range ttype.attribs {
			t0 := tsi.typeSubstn(s.dtype)
			if t0 != s.dtype {
				changed = true
				attribs[i] = makeSymbol(s.ident, t0, s.binding)
				attribs[i].plist = *s.plist.Copy()
			} else {
				attribs[i] = s
			}
		}
		if changed {
			// todo: fix the problem here that I'm not doing anything about methods
			ntyp.SetString()
			return ntyp
		}
	case TFFtn:
		ftype := typ.v.(*Ftntype)
		changed := false
		rett := tsi.typeSubstn(ftype.rettype)
		fmls := make([]*Type, len(ftype.fmlargs))
		for i, t := range ftype.fmlargs {
			t0 := tsi.typeSubstn(t)
			if t0 != t {
				changed = true
			}
			fmls[i] = t0
		}
		if rett != ftype.rettype || changed {
			return makeType(TFFtn, fmls, rett, ftype.pure)
		}
	}
	return typ
}

// The above substitution functions are for general use plus to be used in the rewrite engine. Next,
// we define specialized substitution functions separately, these are for use in the "cleanup" phase of
// the "unfold to fixpoint" body, which iterates rewrite rule application, inlining, and cleanup. Cleanup
// means to (1) delete locals in a Funinst that are unused; (2) delete (parts of) stmts that do nothing;
// (3) move locals that are used once and meet various other conditions. The "parts of" qualification on cases
// 1 and 2 is that if the binding of an unused decl or a stmt consisting of a pure funcall contains non-removeable
// parts then those parts are retained. A non-removeable part is a mod funcall or a call to a connected or
// purified ftn.
//
// The "other conditions" for case 3 are that between the decl of X and its (single) use there are no mod calls
// with X or a symchain rooted in X as rcvr, nor any assignments to X (or a symchain rooted in X), nor any mods
// to any symchain in the *binding* of X. These conditions are slightly more conservative than is actually
// required, for example, an assignment to the aaa attribute of X would be ok if the use of X is a different
// symchain (such as X.bbb) or some other use that ensures X.aaa is not referenced. However, the rule just given
// is sound, if slightly conservative.
//
// A further complication is that use counting needs special treatment in assignment. Specifically, when assigning
// to X (for X any symchain) uses of X on the rhs of the assignment are not counted. Thus, X = F(...) adds nothing to
// the use count of X but it does add to its mods count. This will disqualify X from being moved if there's a single
// later use of X but will still permit X to be deleted if it's a symbol and there are no later uses.

// A function is omittable if pure and not connected or purified
func (fn *Symbol) omittable() bool {
	return !(fn.dtype.isMod() || fn.plist.Find("connected") != nil || fn.plist.Find("purified") != nil)
}

// Traverse trm recursively, starting from a funcall and collect any args that must be kept, accum'g in rslt
func collectNonOmittables(trm Term, rslt *[]Term) {
	switch trm0 := trm.(type) {
	case *TermT:
		if trm0.arg0 != nil {
			collectNonOmittables(trm0.arg0, rslt)
		}
	case *TermTT:
		collectNonOmittables(trm0.arg0, rslt)
		collectNonOmittables(trm0.arg1, rslt)
	case *TermL:
		for _, a := range trm0.args {
			collectNonOmittables(a, rslt)
		}
	case *TermTL:
		if !trm0.trm.(*Symbol).omittable() {
			*rslt = append(*rslt, trm0)
		} else {
			for _, a := range trm0.args {
				collectNonOmittables(a, rslt)
			}
		}
	}
}

// A symCleanup is the value side of the symdict dictionary. Collects info gathered per Symbol in pass1 of cleanup.
type symCleanup struct {
	usecount  int
	modcount  int
	loopdepth int // loop depth at defn of symbol
	disabled  int // if > 0, symbol can't be moved or deleted because modded or something used in binding is modded
	// see cleanupInfo.disables.
	useIsDeeper bool    // true if *any* use is at a larger loopdepth than the defining one
	moved       bool    // used in pass2 to signal that binding was substd for symbol
	synonym     *Symbol // if non-nil, the binding of symbol in key
}

func makeSymCleanup(ld int) *symCleanup {
	return &symCleanup{0, 0, ld, -1, false, false, nil}
}

func (sc *symCleanup) String() string {
	ret := fmt.Sprintf("u:%d, m:%v, dis=%v, mv=%v, dpr=%v", sc.usecount, sc.modcount, sc.disabled, sc.moved, sc.useIsDeeper)
	if sc.synonym != nil {
		ret += fmt.Sprint(";synm=", sc.synonym.ident)
	}
	return ret
}

// This holds the state generated in the info gathering for cleanup (aka pass1) and used in pass2.
type cleanupInfo struct {
	exempt      *Symbol                 // if non-nil, we're in the rhs of an asgnmt to this
	symdict     map[*Symbol]*symCleanup // holds only symbols decld locally
	disables    map[*Symbol][]*Symbol   // tells which symbols to disable moving when a given symbol is modded
	prevstmts   [][]Term                // stack of slices from stmtlists we're calling from, used for realtimer scan
	loopdepth   int
	changecount int
	// rarely, need scopetree access for naming away
	stmtscopes []*Scope
}

func makeCleanupInfo() *cleanupInfo {
	sd := make(map[*Symbol]*symCleanup, 20)
	dbs := make(map[*Symbol][]*Symbol, 20)
	return &cleanupInfo{nil, sd, dbs, make([][]Term, 0, 3), 0, 0, make([]*Scope, 0, 5)}
}

func (cui *cleanupInfo) String() string {
	var bldr strings.Builder
	bldr.WriteString("cui symdict: \n")
	nms := make([]string, 0, len(cui.symdict))
	for k, v := range cui.symdict {
		bldr.WriteString(k.ident)
		bldr.WriteString(": ")
		bldr.WriteString(v.String())
		bldr.WriteString("\n")
		inx := binsearchStrings(nms, k.ident)
		if inx >= 0 && nms[inx] == k.ident {
			bldr.WriteString("duplicate identifier ")
			bldr.WriteString(k.ident)
			bldr.WriteByte('\n')
		} else {
			insertStrings(nms, inx, k.ident)
		}
	}
	return bldr.String()
}

// Recursively traverse trm adding items to cui.disables when (local) symbols are encountered.
// This is called when visiting the declaration of sym. The cui.disables part says that if sym
// is modded at any point, the symbols in it depend on sym and so, should not be moved. In addition,
// if sym.binding contains material that requires non-movement (such as calling a multiret ftn) then
// sym must not be moved; this is handled directly through the disabled slot of cui.symdict[sym].
// Initially, trm == sym.binding but in the recursive traversal, sym is something to refer back to.
func (cui *cleanupInfo) disablements(trm Term, sym *Symbol) {
	switch trm0 := trm.(type) {
	case *Symbol:
		if cui.symdict[trm0] != nil {
			cur := cui.disables[trm0]
			cui.disables[trm0] = append(cur, sym)
		}
	case *Funinst:
		if trm0.body != nil {
			cui.disablements(trm0.body, sym)
		}
	case *TermT:
		if trm0.arg0 != nil {
			cui.disablements(trm0.arg0, sym)
		}
	case *TermTT:
		cui.disablements(trm0.arg0, sym)
		cui.disablements(trm0.arg1, sym)
	case *TermL:
		for _, s := range trm0.args {
			cui.disablements(s, sym)
		}
	case *TermTL:
		cui.disablements(trm0.trm, sym)
		if itemExists(trm0.trm, "multiret") || itemExists(trm0.trm, "connected") {
			cui.symdict[sym].disabled = 1
		}
		for _, s := range trm0.args {
			cui.disablements(s, sym)
		}
	}
}

func (cui *cleanupInfo) addMod(sym *Symbol) {
	entry := cui.symdict[sym]
	if entry != nil { // should always be local but this isn't absolutely required
		entry.disabled = 1 // modded symbols can never be moved
		entry.modcount++
	}
}

// When a symbol is "returned" from ITE, give it an extra usecount since it's likely used by a caller.
func (cui *cleanupInfo) bumpSymbol(trm Term) {
	if trm.Tag() != Stmts {
		return
	}
	trm0 := trm.(*TermL)
	lastinx := len(trm0.args) - 1
	lastx := trm0.args[lastinx]
	if lastx.Tag() == SymbolTag {
		entry := cui.symdict[lastx.(*Symbol)]
		if entry != nil && lastx != cui.exempt {
			entry.usecount += 1
		}
	}
}

// The pass1 method handles the first pass which will build up the symdict. This is the recursion point; it is
// normally started with the body of a Funinst. In fact, it counts on being started with a Stmts so that the
// indices slot has something on it.
func (cui *cleanupInfo) pass1(trm Term) {
	tg := trm.Tag()
	switch trm0 := trm.(type) {
	case *Symbol: // this is not a decl
		entry := cui.symdict[trm0]
		if entry != nil && trm0 != cui.exempt {
			entry.usecount += 1
			if entry.disabled == 0 { // triggerable? then disable.
				entry.disabled++
			}
			if cui.loopdepth > entry.loopdepth {
				entry.useIsDeeper = true
			}
		}
	case *Funinst: // must be a thunk, since IAAF's have been inlined before this is called
		if trm0.body != nil {
			if trm0.dtype.isMod() {
				rcvr := trm0.funargs[0]
				scu := makeSymCleanup(0)
				cui.symdict[rcvr] = scu
			}
			cui.pass1(trm0.body)
		}
	case *TermT:
		if tg == Valdecl || tg == Vardecl {
			sym := trm0.arg0.(*Symbol)
			scu := makeSymCleanup(cui.loopdepth)
			cui.symdict[sym] = scu
			if sym.dtype != nil && sym.binding != nil {
				if sym.binding.Tag() == SymbolTag {
					scu.synonym = sym.binding.(*Symbol)
				}
				cui.pass1(sym.binding)
				cui.disablements(sym.binding, sym)
			}
		} else if trm0.arg0 != nil && tg != AssertStmt {
			if trm0.kind == LoopStmt {
				cui.loopdepth++
			}
			cui.pass1(trm0.arg0)
			if trm0.kind == LoopStmt {
				cui.loopdepth--
			}
		}
	case *TermTT:
		if tg == AsgnStmt {
			lhs := trm0.arg0
			if lhs.Tag() == Symchain {
				lhs = lhs.(*TermL).args[0]
				// symchain on lhs should count as both mod and use
				cui.pass1(lhs)
			} else if lhs.Tag() == Gdref {
				lhs = lhs.(*TermT).arg0 // refnc through gdrefs
			}
			if lhs.Tag() == SymbolTag {
				lhsym := lhs.(*Symbol)
				cui.addMod(lhsym)
				disables := cui.disables[lhsym]
				if disables != nil {
					for _, ss := range disables {
						ssinfo := cui.symdict[ss]
						if ssinfo == nil {
							// it seems like this shouldn't be possible but if ss was deleted on a prior cycle, it can
							// in which case, it's safe to ignore
							continue
						}
						if ssinfo.disabled < 0 { // set to "triggerable"
							ssinfo.disabled = 0
						}
					}
				}
				cui.exempt = lhsym
				cui.pass1(trm0.arg1)
				cui.exempt = nil
			} else {
				cui.pass1(trm0.arg1)
			}
		} else {
			cui.pass1(trm0.arg0)
			cui.pass1(trm0.arg1)
		}
	case *TermL:
		if tg == Stmts {
			// first, flatten nested stmtlists
			flattened := trm0.args
			for i := 0; i < len(flattened); i++ {
				stmt := flattened[i]
				if stmt.Tag() == Stmts {
					nestedStmts := stmt.(*TermL)
					nestcount := len(nestedStmts.args)
					if cap(flattened) < len(flattened)+nestcount {
						flattened0 := make([]Term, len(flattened)+nestcount-1)
						copy(flattened0, flattened[:i])
						copy(flattened0[i:i+nestcount], nestedStmts.args)
						copy(flattened0[i+nestcount:], flattened[i+1:])
						flattened = flattened0
					} else {
						flattened = flattened[:len(flattened)+nestcount-1]
						copy(flattened[i+nestcount:], flattened[i+1:])
						copy(flattened[i:i+nestcount], nestedStmts.args)
					}
				}
			}
			trm0.args = flattened
			// now do pass1 for real
			for i, s := range trm0.args {
				cui.prevstmts = append(cui.prevstmts, trm0.args[:i])
				cui.pass1(s)
				cui.prevstmts = cui.prevstmts[:len(cui.prevstmts)-1]
			}
		} else if tg == EachStmt {
			sym := trm0.args[0].(*Symbol)
			scu := makeSymCleanup(cui.loopdepth)
			cui.symdict[sym] = scu
			cui.pass1(trm0.args[1])
			cui.loopdepth++
			cui.pass1(trm0.args[2])
			cui.loopdepth--
		} else if tg == IfStmt {
			for _, x := range trm0.args {
				cui.pass1(x)
			}
			if trm0.dtype != TypeNothing { // implies if-then-else
				cui.bumpSymbol(trm0.args[1])
				cui.bumpSymbol(trm0.args[2])
			}
		} else if tg == Symchain {
			cui.pass1(trm0.args[0]) // only the root matters
		} else if tg == Multiret || tg == TypeAssertDecl {
			// decl the symbols & traverse the "binding"
			cui.symdict[trm0.args[0].(*Symbol)] = makeSymCleanup(cui.loopdepth)
			cui.symdict[trm0.args[1].(*Symbol)] = makeSymCleanup(cui.loopdepth)
			cui.pass1(trm0.args[2])
		} else {
			for _, x := range trm0.args {
				if x != nil {
					cui.pass1(x)
				}
			}
		}
	case *TermTL:
		skip1 := false
		if tg == Funcall && trm0.trm.Dtype().isMod() { // record rcvr
			rcvr := trm0.args[0]
			// do some careful checking about whether this disables substg the binding for the symbol. It doesn't
			// if the binding is a symbol or symchain, since d8m semantics says that mod calls preserve such entities.
			// However, that only applies if the mod fn returns the rcvr, which we approximate by checking if the types
			// are the same. This isn't completely correct.
			// todo: improve it
			if rcvr.Tag() == Symchain {
				rcvr = rcvr.(*TermL).args[0]
			}
			if rcvr.Tag() == SymbolTag {
				rcvrsym := rcvr.(*Symbol)
				rcvrsym0 := rcvrsym
				rsbdg := rcvrsym.binding
				preserve := rcvrsym.dtype == trm0.dtype
				for {
					if rsbdg == nil || rsbdg.Tag() != SymbolTag {
						break
					}
					rcvrsym = rsbdg.(*Symbol)
					rsbdg = rcvrsym.binding
				}
				var rstg Termtag
				if rsbdg != nil {
					rstg = rsbdg.Tag()
				}
				if rstg == Gdref {
					rstg = rsbdg.(*TermT).arg0.Tag()
				}
				if !preserve {
					preserve = rstg == SymbolTag || rstg == Symchain
				}
				if !preserve {
					cui.addMod(rcvrsym)
				}
				// funargs and nlng's won't have an entry but can't be deleted
				if cui.symdict[rcvrsym0] != nil {
					cui.symdict[rcvrsym0].usecount += 1 // asgnmts don't count use on lhs but mod rcvrs do
				}
				skip1 = true
				disables := cui.disables[rcvrsym0]
				if disables != nil {
					for _, ss := range disables {
						ssinfo := cui.symdict[ss]
						if ssinfo == nil { // ignore, see comment above
							continue
						}
						if ssinfo.disabled < 0 { // set to "triggerable"
							ssinfo.disabled = 0
						}
					}
				}
			} else if callingNamed(trm0, "Sscanf") {
				// Sscanf is currently the only (package) function that requires a pointer and mods the value. Ensure
				// that the mod is recorded. Note that args[2] is the one getting ptrzd.
				if trm0.args[2].Tag() == SymbolTag {
					cui.addMod(trm0.args[2].(*Symbol))
				}
			}
			// The following travesty is to fix a problem with swapping index elts of a list.
			// It shows a need to do more serious CDFA in order to do this correctly.
			if callingNamed(trm0, "lvalindex") && trm0.args[2].Tag() == SymbolTag {
				sym0 := trm0.args[2].(*Symbol)
				entry := cui.symdict[sym0]
				if entry != nil {
					entry.disabled = 1
				}
			}
		}
		// Check for cases where we shouldn't move defns across time revealers. This is hackish but not too bad IMO.
		// The idea is to disable mvmt of any unused local defined prior to the time revealing stmt. If it's already dead
		// (meaning born dead) this doesn't matter; else, it picks just the ones that could be moved across the time revealer.
		if tg == Funcall && trm0.trm.Plist().Find("realtimer") != nil {
			prevstmts := cui.prevstmts[len(cui.prevstmts)-1]
			for _, stmt := range prevstmts {
				stg := stmt.Tag()
				// disable mvmt of any unused local defined prior to the time revealing stmt.
				if stg == Valdecl || stg == Vardecl {
					declsym := stmt.(*TermT).arg0.(*Symbol)
					entry := cui.symdict[declsym] // cannot be nil
					usemods := entry.usecount + entry.modcount
					if entry.synonym != nil {
						entry1 := cui.symdict[entry.synonym]
						usemods += entry1.usecount + entry1.modcount
					}
					if usemods == 0 {
						entry.disabled = 1
						//fmt.Println("disabling", declsym.ident)
					}
				}
			}
		}
		cui.pass1(trm0.trm)
		for _, a := range trm0.args {
			if skip1 { // see above, maybe we already processed the rcvr
				skip1 = false
				continue
			}
			cui.pass1(a)
		}
	}
}

// Called from pass2 to handle Stmts terms. Thus, the arg type is *TermL rather than Term.
// Works by modding trm in place, typically by deleting stmts. Runs from last stmt backwards to the first.
func (cui *cleanupInfo) pass2stmts(trm *TermL) {
	args := trm.args
	nargs := len(args)
	if nargs == 0 {
		return
	}
	cui.stmtscopes = append(cui.stmtscopes, trm.scope)
	defer func() { cui.stmtscopes = cui.stmtscopes[:len(cui.stmtscopes)-1] }()
	//na0 := nargs
	i := nargs - 1
	if trm.dtype != TypeNothing {
		if prCleanup {
			logger.Println("...cleanup on retval:", args[i])
		}
		args[i] = cui.pass2(args[i])
		if prCleanup {
			logger.Println("...continuing with pass2stmts")
		}
		i--
	}
	for ; i >= 0; i-- {
		stmt := args[i]
		if prCleanup {
			logger.Print("pass2 (i=", i, "): ", stmt.String())
		}
		tg := stmt.Tag()
		var sym *Symbol
		var check4mods Term
		// del tells if we're deleting stmt
		// tricky bit: initz del to true if stmt is a base literal (unlikely but possible)
		_, del := stmt.(*TermB)
		if tg == AsgnStmt {
			stmt0 := stmt.(*TermTT)
			lhs := stmt0.arg0
			if lhs.Tag() == Symchain {
				lhs = lhs.(*TermL).args[0]
			}
			if lhs.Tag() == SymbolTag {
				sym = lhs.(*Symbol)
				// The following is very hackish and somewhat incorrect. The problem is explained in the termmod tests;
				// with ortypes the usual way to handle modding doesn't work because you can't take a pointer of an intfc
				// type. Here, we detect and "fix" the inlined version of pushb to a list in an ortype by substg the
				// ortype for the symbol bound to it (with a gdref). This is incorrect if there's another use of the
				// symbol, although there typically isn't. A better approach requires more robust CDFA than I've got right
				// now. So here goes.
				rhs := stmt0.arg1
				if callingNamed(rhs, "__append") && rhs.(*TermTL).args[0].Equal(lhs) && sym.binding != nil && sym.binding.Tag() == Gdref {
					bdgtyp := sym.binding.(*TermT).arg0.Dtype()
					if bdgtyp.family == TFOrtype && !bdgtyp.isNilposs() {
						stmt0.arg0 = sym.binding.(*TermT).arg0
						return
					}
				}
			}
		} else if tg == SymbolTag || tg == Symchain {
			del = true
			if tg == SymbolTag {
				entry := cui.symdict[stmt.(*Symbol)]
				if entry != nil {
					entry.usecount -= 1
				}
			} else {
				scroot := stmt.(*TermL).args[0]
				var sym *Symbol
				if scroot.Tag() == SymbolTag {
					sym = scroot.(*Symbol)
				} else if scroot.Tag() == Gdref {
					sym = scroot.(*TermT).arg0.(*Symbol)
				}
				entry := cui.symdict[sym]
				if entry != nil {
					entry.usecount -= 1
				}
			}
		} else if callingNamed(stmt, "rvalindex") && stmt.Dtype() == TypeNothing {
			del = true
		} else if tg == Valdecl || tg == Vardecl {
			sym = stmt.(*TermT).arg0.(*Symbol)
		}
		if sym != nil {
			entry := cui.symdict[sym]
			if entry != nil {
				adjustedUsecount := entry.usecount
				if entry.synonym != nil {
					synentry := cui.symdict[entry.synonym]
					if synentry != nil && tg != Valdecl && tg != Vardecl {
						adjustedUsecount += synentry.usecount - 1 // the - 1 adjusts for the binding in sym's defn
					}
				}
				if adjustedUsecount == 0 || entry.moved {
					del = true
				}
			}
		}
		if del {
			if prCleanup {
				logger.Println("...deleting")
			}
			// the following has no real effect, just cleans up symdict a bit
			if tg == Valdecl || tg == Vardecl {
				delete(cui.symdict, sym)
			}
			// here's the actual deletion
			copy(args[i:nargs-1], args[i+1:nargs])
			nargs -= 1
			args = args[:nargs]
			trm.args = args // copy back now so things work as we continue
			cui.changecount++
		} else {
			if prCleanup {
				logger.Println("...exploring")
			}
			args[i] = cui.pass2(stmt)
		}
		if check4mods != nil {
			// we have a funcall as stmt, on an omittable ftn, so we've deleted it. However, add back any
			// args that shouldn't be omitted
			colln := make([]Term, 0, 2)
			collectNonOmittables(check4mods, &colln)
			if len(colln) > 0 {
				if nargs+len(colln) > cap(args) {
					args0 := make([]Term, nargs, cap(args)+len(colln))
					copy(args0, args)
					args = args0
				}
				args = args[:nargs+len(colln)]
				copy(args[i+len(colln):], args[i:nargs])
				copy(args[i:i+len(colln)], colln)
				trm.args = args
			}
		}
	}
	if prCleanup {
		logger.Println("...finishing pass2stmts")
	}
	trm.args = args
}

// Pass2 of cleanup follows pass1 which collects info into cui. Pass2 does the actual cleanup, which involves
// (1) deleting useless statements; (2) replacing certain symbols with their bindings, thereby rendering their decls
// useless. Pass2 is mutually recursive with pass2stmts, which handles stmt deletion. Pass2 itself handles traversal
// of more fine grained parts of Terms.
func (cui *cleanupInfo) pass2(trm Term) Term {
	if trm == nil {
		return nil
	}
	switch trm.Tag() {
	case Stmts:
		cui.pass2stmts(trm.(*TermL))
	case IfStmt:
		trm0 := trm.(*TermL)
		trm0.args[0] = cui.pass2(trm0.args[0])
		tpart := trm0.args[1]
		if tpart.Tag() == Stmts {
			cui.pass2stmts(tpart.(*TermL))
		} else {
			trm0.args[1] = cui.pass2(tpart)
		}
		if len(trm0.args) == 3 {
			epart := trm0.args[2]
			if epart.Tag() == Stmts {
				cui.pass2stmts(epart.(*TermL))
			} else {
				trm0.args[2] = cui.pass2(epart)
			}
		}
	case LoopStmt:
		trm0 := trm.(*TermT)
		cui.pass2stmts(trm0.arg0.(*TermL))
	case Multiret, TypeAssertDecl:
		trm0 := trm.(*TermL)
		sym0 := trm0.args[0].(*Symbol)
		sym1 := trm0.args[1].(*Symbol)
		sc := cui.symdict[sym0]
		if sc.usecount == 0 {
			sym0.smash2Anon()
		}
		sc = cui.symdict[sym1]
		if sc.usecount == 0 {
			sym1.smash2Anon()
		}
		cui.pass2(trm0.args[2])
	case AsgnStmt:
		trm0 := trm.(*TermTT)
		// check for unused locals
		if trm0.arg0.Tag() == SymbolTag {
			sc := cui.symdict[trm0.arg0.(*Symbol)]
			if sc != nil && sc.usecount == 0 {
				fmt.Println("warning: variable appears to be unused:", trm0.String())
				return makeScopedTermL(Stmts, nil, []Term{}, TypeNothing, Pos(-1), Pos(-1))
			}
		}
		nv0 := cui.pass2(trm0.arg0)
		trm0.arg0 = nv0
		trm0.arg1 = cui.pass2(trm0.arg1)
	case EachStmt:
		trm0 := trm.(*TermL)
		itervar := trm0.args[0].(*Symbol)
		sc := cui.symdict[itervar]
		if sc.usecount == 0 && itervar.ident != "_" {
			iteree := trm0.args[1]
			isrange := iteree.Tag() == Range
			if iteree.Tag() == SymbolTag && itemExistsNot(iteree, "var") {
				itersym := iteree.(*Symbol)
				isrange = itersym.binding != nil && itersym.binding.Tag() == Range
			}
			if !isrange {
				//fmt.Println("smashing itervar in ", trm0.String())
				itervar.smash2Anon() // smash to something go won't complain about
			}
		}
		trm0.args[1] = cui.pass2(trm0.args[1])
		cui.pass2stmts(trm0.args[2].(*TermL))
	case Ifcase:
		for _, clz := range trm.(*TermL).args {
			clz0 := clz.(*TermTT)
			clz0.arg0 = cui.pass2(clz0.arg0)
			rhs := clz0.arg1
			if rhs.Tag() == Stmts {
				cui.pass2stmts(rhs.(*TermL))
			} else {
				cui.pass2(rhs)
			}
		}
	case Valdecl, Vardecl:
		sym := trm.(*TermT).arg0.(*Symbol)
		cui.pass2(sym)
		sym.binding = cui.pass2(sym.binding)
	default:
		// as we commonly do, handle the default case structurally
		switch trm0 := trm.(type) {
		case *Symbol:
			entry := cui.symdict[trm0]
			if entry != nil {
				singleUse := entry.usecount == 1 && entry.modcount == 0 && entry.disabled <= 0
				_, isLit := trm0.binding.(*TermB)
				if trm0.binding == nil {
					return trm0
				}
				btg := trm0.binding.Tag()
				realSimple := (isLit || btg == SymbolTag || btg == Symchain) && entry.disabled <= 0 && trm0.plist.Find("var") == nil
				modbdg := btg == Funcall && trm0.binding.(*TermTL).trm.Dtype().isMod()
				synmove := entry.synonym != nil && entry.disabled <= 0
				if realSimple || (singleUse && !entry.useIsDeeper && !modbdg) || synmove {
					entry.moved = true
					if synmove {
						// symbol interference can happen with trm0.ident, so use a gensym
						newident := gensymPfx("__c")
						// Smashing the synonym's ident to something random is very likely to screw up the ordering of
						// the Symdict it lives in.
						if entry.synonym.homescope != nil {
							upscope := entry.synonym.homescope
							upscope.entries.Remove(entry.synonym)
							entry.synonym.ident = newident
							upscope.Add(entry.synonym, 0)
						}
					}
					return cui.pass2(trm0.binding)
				}
			}
		case *Funinst:
			if trm0.body != nil {
				trm0.body = cui.pass2(trm0.body).(*TermL)
			}
		case *TermT:
			if trm0.kind == Valdecl || trm0.kind == Vardecl {
				sym := trm0.arg0.(*Symbol)
				if sym.binding != nil {
					sym.binding = cui.pass2(sym.binding)
				}
			} else if trm0.kind == Gdref {
				tmp := cui.pass2(trm0.arg0)
				tmptyp := tmp.Dtype()
				if tmp.Tag() != SymbolTag && tmptyp.family != TFOrtype {
					return tmp // flush the gdref unless it's ortype or not ptrzd
				}
				trm0.arg0 = tmp
			} else if trm0.arg0 != nil && trm0.kind != Fwddecl {
				trm0.arg0 = cui.pass2(trm0.arg0)
			}
		case *TermTT:
			//trm0.arg0 = cui.pass2(trm0.arg0)
			nv0 := cui.pass2(trm0.arg0)
			trm0.arg0 = nv0
			trm0.arg1 = cui.pass2(trm0.arg1)
		case *TermL:
			for i, a := range trm0.args {
				trm0.args[i] = cui.pass2(a)
			}
		case *TermTL:
			for i, a := range trm0.args {
				trm0.args[i] = cui.pass2(a)
			}
		}
	}
	return trm
}

// This runs in the optimize loop after every OOLXforms call. Its main job is to clean up decls of now unused
// symbols, do appropriate movements of single use symbols, etc. It's also going to be a place to put various
// "manual" optimizations.
func (cui *cleanupInfo) cleanup(trm Term) Term {
	cui.pass1(trm)
	if prCleanup {
		logger.Println("cleanup on ", trm.String())
		logger.Println("cui (after pass1):", cui.String())
	}
	trm0 := cui.pass2(trm)
	if prCleanup {
		logger.Println("post-pass2:", trm0.String())
	}
	return Simplify(trm0)
}

// Helper for flattenStmts1, called when we've modded a stmtlist and need to ensure that scopetree links are good.
// This is a recursive traversal (which is why it's a separate ftn) but doesn't go into deeper scopes.
// BTW, I need to figure out at some point whether and/or how to rationalize this with the ftn fixupScopetree over in ast.go.
func fixScopetrees(trm Term, tgtscope *Scope) {
	tg := trm.Tag()
	switch trm0 := trm.(type) {
	case *Funinst:
		trm0.scope.parent = tgtscope // payoff
	case *TermT:
		if tg == Valdecl || tg == Vardecl {
			sym := trm0.arg0.(*Symbol)
			if sym.binding != nil {
				fixScopetrees(sym.binding, tgtscope)
			}
		} else if trm0.arg0 != nil {
			fixScopetrees(trm0.arg0, tgtscope)
		}
	case *TermTT:
		fixScopetrees(trm0.arg0, tgtscope)
		fixScopetrees(trm0.arg1, tgtscope)
	case *TermL:
		if (tg == EachStmt || tg == Stmts) && trm0.scope != nil {
			trm0.scope.parent = tgtscope // payoff
		} else if tg == Symchain {
			fixScopetrees(trm0.args[0], tgtscope)
		} else if tg == IfStmt {
			fixScopetrees(trm0.args[0], tgtscope)
			fixScopetrees(trm0.args[1], tgtscope)
			if len(trm0.args) > 2 {
				fixScopetrees(trm0.args[2], tgtscope)
			}
		} else if tg == Litform0 {
			for _, x := range trm0.args {
				fixScopetrees(x, tgtscope)
			}
		}
	case *TermTL:
		fixScopetrees(trm0.trm, tgtscope)
		for _, x := range trm0.args {
			fixScopetrees(x, tgtscope)
		}
	}
}

// Flatten nested stmt lists. Always by smashing. Can receive any kind of Term but will only do anything
// to stmtlists and terms directly containing same. Also fix up the scopes of stmtlists since they may be merged,
// and thread any scopetree linkages that get screwed up for the same reason.
func flattenStmts1(trm Term) {
	switch trm.Tag() {
	case Stmts:
		trm0 := trm.(*TermL)
		argsx := make([]Term, 0, len(trm0.args))
		for i := 0; i < len(trm0.args); i++ {
			s := trm0.args[i]
			if s.Tag() == Stmts {
				j := len(argsx)
				argsx = append(argsx, s.(*TermL).args...)
				for ; j < len(argsx); j++ {
					flattenStmts1(argsx[j])
				}
			} else {
				flattenStmts1(s) // changes don't propagate
				argsx = append(argsx, s)
			}
		}
		trm0.args = argsx
		// redo the scope and fix any dangling scopetree pointer
		if trm0.scope != nil {
			trm0.scope.entries = trm0.scope.entries[:0]
			for _, s := range argsx {
				tg := s.Tag()
				if tg == Valdecl || tg == Vardecl {
					decld := s.(*TermT).arg0.(*Symbol)
					trm0.scope.Add(decld, 0)
				} else if tg == Multiret {
					mrd := s.(*TermL)
					trm0.scope.Add(mrd.args[0].(*Symbol), 0)
					trm0.scope.Add(mrd.args[1].(*Symbol), 0)
				} else {
					fixScopetrees(s, trm0.scope)
				}
			}
		}
		if len(argsx) > 0 {
			sltyp := argsx[len(argsx)-1].Dtype()
			if !sltyp.isNilposs() {
				// the rationale for the condition is ITE branches can be definite; probably a better soln is to fix smashDtype
				smashDtype(trm, sltyp)
			}
		}
	case IfStmt:
		trm0 := trm.(*TermL)
		flattenStmts1(trm0.args[0])
		flattenStmts1(trm0.args[1])
		if len(trm0.args) > 2 {
			flattenStmts1(trm0.args[2])
		}
	case LoopStmt:
		trm0 := trm.(*TermT)
		flattenStmts1(trm0.arg0)
	case EachStmt:
		flattenStmts1(trm.(*TermL).args[2])
	case Ifcase:
		trm0 := trm.(*TermL)
		for _, clz := range trm0.args {
			flattenStmts1(clz.(*TermTT).arg1)
		}
	}
	return
}

// Generate a code size estimate, generally for a Funinst but works for Terms generally.
// Will not go into Symbols, except when they're being declared.
// For now, don't count the code in nested type defns but do count nested ftn defns.
func genCodesize(trm Term) (retval int) {
	tg := trm.Tag()
	switch trm0 := trm.(type) {
	case *Symbol:
		retval = 1
	case *Funinst:
		if trm0.body != nil {
			retval = trm0.codesize
			if retval == 0 {
				trm0.codesize = genCodesize(trm0.body)
				retval = trm0.codesize
			}
		}
	case *TermT:
		if tg == Valdecl || tg == Vardecl {
			sym := trm0.arg0.(*Symbol)
			if sym.binding != nil && sym.binding.Tag() != TypeTag {
				retval = 1 + genCodesize(sym.binding)
			}
		} else if tg == Range {
			retval = 4
		} else {
			incr := 1
			if tg == LoopStmt {
				incr = 4
			} else if tg == Gdref {
				incr = 0
			}
			if trm0.arg0 != nil {
				incr += genCodesize(trm0.arg0)
			}
			retval = incr
		}
	case *TermTT:
		if tg == AsgnStmt {
			retval = 1 + genCodesize(trm0.arg0) + genCodesize(trm0.arg1)
		} else if tg == EqExpr || tg == EqeqExpr {
			retval = 1
			argfam := trm0.arg0.Dtype().family
			// the following numbers are rectally extracted
			if argfam == TFList || argfam == TFSpace {
				retval = 8
			} else if argfam == TFTuple {
				retval = 5
			}
			retval += genCodesize(trm0.arg0) + genCodesize(trm0.arg1)
		} else if tg == Typecond {
			retval = 4
		} else if tg == Arrow {
			retval = genCodesize(trm0.arg0) + genCodesize(trm0.arg1)
		} else {
			panic("unexpected") // this is really so I catch in tests, should eventually go away
		}
	case *TermL:
		retval = 1
		if tg == EachStmt {
			retval = 4
		} else if tg == IfStmt || tg == Ifcase {
			retval = 3
		} else if tg == Multiret || tg == TypeAssertDecl {
			retval = 0
		} else if tg == AndandExpr || tg == OrorExpr {
			retval = 2
		} else if !(tg == Stmts || tg == Litform0 || tg == Symchain) {
			panic("unexpected")
		}
		for _, a := range trm0.args {
			retval += genCodesize(a)
		}
	case *TermTL:
		if tg == Funcall || tg == Typecase {
			retval = genCodesize(trm0.trm)
			for _, a := range trm0.args {
				retval += genCodesize(a)
			}
		} else {
			panic("unexpected") // probably really shouldn't happen
		}
	}
	return
}

// Here begin definitions related to the FSM derived from the set of active rewrite rules, IOW active
// patterns to matchquery those rules. Naively, the matching process is O(N*M) where N is # active rules and
// M is # subterms of the query. Calling the full pattern matcher every time would likely not be catastrophic
// but the FSM can speed things up by checking tags and related things. The first level of checking is the tag.
// Patterns don't have to require a specific tag to match but they almost always do. If they don't, every subterm
// needs to be checked. The only way this can happen is if the (entire) pattern is a TEM. A RWRule that checks every
// term of a given type is probably a bad idea, but it's allowed by the language.
//
// The secondary rule is optional. If present, it checks whether a specific subterm of the term under consideration
// is a funcall to a function with a given name. The nm slot of secondaryRuleInfo is the string; the code tells whether
// the rule is active and if so, where it's to be checked.
type secondaryRuleInfo struct {
	code int
	nm   string
}

const (
	secCodeNone int = iota
	secCode0        // arg0 or binding
	secCode1        // arg1 (for TT)
	secCodeN        // base for args (for T, TL)
)

// This is called from the "FSM compiler"; it decides whether ptn should use a secondary rule and sets it up if so.
func (sd *secondaryRuleInfo) setup(ptn *Pattern) {
	var ptnFuncall = func(ptn0 *Pattern) (nm0 string) {
		if ptn0.tag == Funcall {
			p00 := ptn0.parts[0]
			if p00.tag == SymbolTag {
				nm0 = p00.aTerm.(*Symbol).ident
			} else if p00.tag == SymbolPattern && p00.parts[0].tag == Labellit {
				nm0 = p00.parts[0].aTerm.(*TermB).value
			}
		}
		return
	}
	switch ptn.tag {
	case Valdecl, Vardecl, IfStmt, Funcall:
		nm := ptnFuncall(ptn.parts[0])
		if nm != "" {
			sd.code = secCode0
			sd.nm = nm
		}
	case EachStmt:
		// only the list arg makes sense to require a funcall so try that
		ptn0 := ptn.parts[1]
		if ptn0.tag == SymbolTag && ptn0.flexcount == 0 {
			ptn0 = ptn0.parts[0]
		}
		nm := ptnFuncall(ptn0)
		if nm != "" {
			sd.code = secCode1
			sd.nm = nm
		}
	}
}

// This is the runtime for secondary rules.
// Check a term against sd. If sd is a no-op, anything checks; else implement the checks coded by setup.
func (sd *secondaryRuleInfo) check(trm Term) bool {
	if sd.code != secCodeNone {
		switch trm.Tag() {
		case Valdecl, Vardecl:
			return callingNamed(trm.(*TermT).arg0.(*Symbol).binding, sd.nm)
		case IfStmt:
			return callingNamed(trm.(*TermL).args[0], sd.nm)
		case Funcall:
			return callingNamed(trm, sd.nm)
		case EachStmt:
			// Don't need code here, it can only be the list arg.
			return callingNamed(trm.(*TermL).args[1], sd.nm)
		}
	}
	return true
}

// Here, we have a different kind of secondary rule, one that applies only to Stmts terms, ie stmtlists.
// When a pattern matches a stmtlist with some specific statement types interspersed with xprstmts matchables,
// this rule kicks in. The runtime part kicks out any actual stmtlists that don't have the required statement
// types in the right order. The stmtruleInfo also includes the pattern (mqp slot) and the xprftn to interpret if
// the pattern matches.
// Use the rwruleInfo for patterns that start with a specific tag (other than Stmts). Check the tag, then
// check the secondary rule, and call the matcher with mqp if both match.
type rwruleInfo struct {
	sequence []Termtag
	mqp      *Pattern
	rule     *Funinst
	addcrit  secondaryRuleInfo
	actuals  []Term
}

func (rwri *rwruleInfo) clear() {
	rwri.rule.scope.parent.clearGTS()
	bodyscope := rwri.rule.body.scope
	for _, sx := range bodyscope.entries {
		mtchble := itemExists(sx.sym, "TEM") || itemExists(sx.sym, "PEM")
		if mtchble {
			sx.sym.binding = nil
		}
	}
}

// One of these per scope while inlining to manage where we are in a stmtlist. Equipped with methods to help.
type inlineInfo struct {
	stmtlist *TermL // stmtlist we're currently traversing and into which we're depositing OOL'd stmts
	oolStmts []Term // stmts to be inserted for current stmt
	stmtsinx int    // index for traversal
}

// Accumulate stmts into the oolStmts slot and merge scp into the scope of ili.stmtlist. Note that
// although the statements are collected separately from ili.stmtlist (in oolStmts) the symbols are integrated
// immediately. This non-obvious thing is done because otherwise, identifierOutOfScope doesn't work!
// Rettype is an historical artifact from insertStmts: if not nothing, it pops the last stmt and returns it.
func (ili *inlineInfo) insertOOL(stmts []Term, scp *Scope, rettype *Type) (retval Term) {
	laststmt := len(stmts) - 1
	if rettype != TypeNothing {
		retval = stmts[laststmt]
		stmts = stmts[:laststmt]
	} else if laststmt >= 0 {
		lastnew := stmts[laststmt]
		if lastnew.Dtype() != TypeNothing {
			smashDtype(lastnew, TypeNothing)
		}
	}
	ili.oolStmts = append(ili.oolStmts, stmts...)
	if scp != nil {
		for _, sx := range scp.entries {
			ili.stmtlist.scope.Add(sx.sym, sx.xcount)
		}
	}
	return
}

// Insert ili.oolStmts into ili.stmtlist at ili.stmtsinx. Scan inserted stmts for scopes and fix parent links.
func (ili *inlineInfo) insertStmts() {
	xargs := ili.stmtlist.args
	nstmts := len(ili.oolStmts)
	if nstmts > 0 {
		if len(xargs)+nstmts > cap(xargs) {
			actvs0 := make([]Term, len(xargs), 2*(len(xargs)+nstmts))
			copy(actvs0[:len(xargs)], xargs)
			xargs = actvs0
		}
		ili.stmtlist.args = xargs[:len(xargs)+nstmts]
		copy(ili.stmtlist.args[ili.stmtsinx+nstmts:], xargs[ili.stmtsinx:])
		copy(ili.stmtlist.args[ili.stmtsinx:ili.stmtsinx+nstmts], ili.oolStmts)

		// scan for stmts containing scopes and redo their parents
		for _, s := range ili.oolStmts {
			var reLink = func(trm Term) {
				if trm.Tag() == Stmts {
					trm.(*TermL).scope.parent = ili.stmtlist.scope
				}
			}
			tg := s.Tag()
			if tg == IfStmt {
				args := s.(*TermL).args
				reLink(args[1])
				if len(args) == 3 {
					reLink(args[2])
				}
			} else if tg == EachStmt {
				s.(*TermL).scope.parent = ili.stmtlist.scope
			} else if tg == LoopStmt {
				reLink(s.(*TermT).arg0)
			}
		}
		ili.oolStmts = ili.oolStmts[:0]
	} else {
		ili.stmtsinx += 1
	}
}

// Inline a single ftn, returning nil if the ftn is nothing-valued else its final expr repg
// the return value. (Inlineable ftns never have return stmts.) The rest of the ftn body is added
// to iln.stmtlist via the inlineInfo.insertOOL call. We make a copy of the funi in case it's
// inlined again. This includes copies of locally decld Symbols but not others.
func (iln *inlineInfo) inlineOne(funi *Funinst, args []Term, rettype *Type) Term {
	funi0 := funi.copyFuninst()
	frett := funi.dtype.v.(*Ftntype).rettype
	if rettype != TypeNothing && !frett.Equal(rettype) {
		// this can happen due to a cast and inlining doesn't pick up the cast unless we rework the ftn we're about to inline
		// Since a funinst with return stmts isn't inlineable we don't need to worry about returns.
		ftntype := funi0.dtype.Copy().(*Type)
		ftntype.v.(*Ftntype).rettype = rettype
		changept := funi.body.args[len(funi.body.args)-1]
		if changept.Tag() == SymbolTag {
			sym := changept.(*Symbol)
			syminx := funi.body.scope.entries.IndexExact(sym)
			if syminx >= 0 {
				// it's local, smash the dtype and change the binding in nscope
				sym0 := funi0.body.scope.entries[syminx].sym
				sym0.dtype = rettype
				smashDtype(sym0.binding, rettype)
			} else {
				panic("can't yet handle non-local in " + funi.body.String())
			}
		} else {
			smashDtype(changept, rettype)
		}
		//fmt.Println("inlining gets rettype=", rettype.String(), " and funi.rettype=", ftntype.rettype.String())
	}

	rcvrScope := iln.stmtlist.scope
	localFormals := make([]*Symbol, 0, 2) // holds symbols bound to actuals
	substsB4 := make([]Term, 0, 2)
	substsAftr := make([]Term, 0, 2)
	nested := make([]*Scope, 0, 2)
	findNested(funi0.body, &nested)
	// Also collect nested scopes that occur after the current stmt
	for _, t := range iln.stmtlist.args[iln.stmtsinx+1:] {
		findNested(t, &nested)
	}
	ftntype := funi0.dtype.v.(*Ftntype)
	// findNested leaves the direct locals of funi0, if any, in nested[0]. Pop these as they're
	// what we want to rename next (away from anything in the cxt).
	if funi0.body.scope != nil && len(funi0.body.scope.entries) > 0 {
		nnest := len(nested) - 1
		directLocals := nested[0]
		copy(nested, nested[1:])
		nested = nested[:nnest]
		for _, lclx := range directLocals.entries {
			ident0 := identifierOutOfScope(rcvrScope, nested, lclx.sym.ident, lclx.sym)
			//if lclx.sym.ident != ident0 {
			//	fmt.Println("renaming ", lclx.sym.ident, "to", ident0)
			//}
			lclx.sym.ident = ident0
		}
	}
	lvalindexHack := false
	for i, fml := range funi0.funargs {
		actl := args[i]
		actualTag := actl.Tag()
		noAlloc := actualTag == SymbolTag || actualTag == Symchain || actualTag.baselit()
		isRVI := callingNamed(actl, "rvalindex")
		if i == 0 && !ftntype.pure && isRVI {
			// rcvr of mod ftn is xx[yy] which must become lvalindex if there are asgnmts to rcvr
			noAlloc = true
			lvalindexHack = true
		}
		if noAlloc {
			// We're not going to introduce a local for this actual because it's a symchain or literal. The symbol
			// can't interfere with localFormals, as we just renamed them away. (See above.) So we just need to
			// set up substns and remove the fml from the local scope, since it's going away
			substsB4 = append(substsB4, fml)
			// Since this symbol (or root of symchain) is going to be substd into the body, we need to rename it
			// away from any nested symbols.
			if actualTag == SymbolTag || actualTag == Symchain {
				var actl0 *Symbol
				if actualTag == Symchain {
					tg := actl.(*TermL).args[0].Tag()
					if tg == SymbolTag {
						actl0 = actl.(*TermL).args[0].(*Symbol)
					} else if tg == Gdref {
						inGdref := actl.(*TermL).args[0].(*TermT).arg0
						if inGdref.Tag() == SymbolTag {
							actl0 = inGdref.(*Symbol)
						} else {
							tmp := inGdref.(*TermL).args[0]
							if tmp.Tag() == SymbolTag {
								actl0 = inGdref.(*TermL).args[0].(*Symbol)
							}
						}
					}
				} else {
					actl0 = actl.(*Symbol)
				}
				if actl0 != nil {
					ident0 := identifierOutOfScope(nil, nested, actl0.ident, actl0)
					actl0.ident = ident0
				}
			}
			substsAftr = append(substsAftr, actl)
			funi0.scope.entries.Remove(fml) // and delete orphan symbol from scope
		} else {
			// here, get a working identifier derived from fmlarg and record it in localFormals
			ident0 := identifierOutOfScope(rcvrScope, nested, fml.ident, fml)
			//if ident0 != fml.ident {
			//	fmt.Println("renaming fml ", fml.ident, " to ", ident0)
			//}
			fml.ident = ident0
			fml.binding = actl
			if fml.dtype.family == TFTypred { // specialize dtype if fml is a typred
				fml.dtype = actl.Dtype()
			}
			if i == 0 && funi.dtype.isMod() {
				fml.plist.Add("var", true)
			}
			localFormals = append(localFormals, fml)
		}
	}
	// The following is very hackish but is the best soln I've come up with so far. The issue is that expectedType
	// can be subject to QC-time eval and it's convenient to do it in Simplify but Simplify is stateless. The funi is marked
	// if expectedType is used, so in that case replace the orig symbol with one having the current (possibly concrete) type,
	// at which point Simplify can do something
	xtsym := funi.plist.Find("expectedType")
	if xtsym != nil {
		substsB4 = append(substsB4, xtsym.(*Symbol))
		substsAftr = append(substsAftr, makeSymbol("expectedType", nil, rettype))
	}
	// next, we do substitute+lvalindex hack on the copy in funi0, then insert the new local decls at the front of the body
	funi0.body.scope.parent = iln.stmtlist.scope
	body0 := subst4Inline(funi0.body, substsB4, substsAftr, lvalindexHack)
	// Also simplify now
	body0s := Simplify(body0)
	if body0s.Tag() == Stmts {
		body0 = body0s.(*TermL)
	} else {
		// Note: I am pretty sure this can't happen but Simplify might remove single stmts either now or in future
		body0 = makeScopedTermL(Stmts, body0.scope, []Term{body0s}, body0s.Dtype(), body0s.First(), body0s.Final())
	}

	nlfs := len(localFormals)
	if nlfs > 0 {
		// create space for localFormals to become decls in copied funinst body
		nlen := len(body0.args) + nlfs
		if cap(body0.args) < nlen {
			body1 := make([]Term, nlen)
			copy(body1[nlfs:], body0.args)
			body0.args = body1
		} else {
			body0.args = body0.args[:nlen]
			copy(body0.args[nlfs:], body0.args[:nlen-nlfs])
		}
	}
	for i, lcl := range localFormals {
		tg := Valdecl
		if lcl.plist.Find("var") != nil {
			tg = Vardecl
		}
		body0.args[i] = makeTermT(tg, lcl, TypeNothing, Pos(-1), Pos(-1))
		body0.scope.Add(lcl, 0)
	}
	retval := iln.insertOOL(body0.args, body0.scope, rettype)
	return retval
}

func hasReturn(trm Term) bool {
	if trm.Tag() == Stmts || trm.Tag() == LoopStmt {
		var ss *TermL
		if trm.Tag() == Stmts {
			ss = trm.(*TermL)
		} else {
			ss = trm.(*TermT).arg0.(*TermL)
		}
		for _, s := range ss.args {
			if hasReturn(s) {
				return true
			}
		}
	} else if trm.Tag() == IfStmt {
		trm0 := trm.(*TermL)
		return hasReturn(trm0.args[1]) || (len(trm0.args) == 3 && hasReturn(trm0.args[2]))
	}
	return trm.Tag() == ReturnStmt
}

// ifxpr will be an expr tagged IfStmt, Ifcase, or Typecase, which is allowed in d8m but not in go. Apply a transform
// to make it go compatible by introducing a new uninitialized gensym, then assigning to it in the if/ifcase branches.
// Note that the incoming term can be smashed, so that's what we do. The stmt is replaced by the gensym and the 2
// other stmts (decl and if/ifcase) are inserted in front.
func (iln *inlineInfo) ifexpr2go(ifxpr Term) Term {
	ifxtyp := ifxpr.Dtype()
	var xfmBranch = func(part Term, gensym *Symbol) Term {
		// don't xfm exit-valued branches
		if part.Dtype() == TypeExit {
			return part
		}
		// nor branches that return
		if hasReturn(part) {
			return part
		}
		if part.Tag() != Stmts {
			scp := makeScope()
			scp.parent = iln.stmtlist.scope
			part = makeScopedTermL(Stmts, scp, []Term{part}, ifxtyp, Pos(-1), Pos(-1))
		}
		part0 := part.(*TermL)
		nargs := len(part0.args)
		part0.args[nargs-1] = makeTermTT(AsgnStmt, gensym, part0.args[nargs-1], TypeNothing, Pos(-1), Pos(-1))
		part0.dtype = TypeNothing
		return part0
	}
	if prInline {
		logger.Println("ifexpr2go on ", ifxpr.String())
	}
	nested := make([]*Scope, 0, 2)
	findNested(iln.stmtlist, &nested)
	symident := identifierOutOfScope(iln.stmtlist.scope, nested, "_v", nil)
	newstmts := make([]Term, 2)
	gensym := makeSymbol(symident, ifxtyp, makeTermT(ZeroValue, nil, ifxtyp, Pos(-1), Pos(-1)))
	gensym.plist.Add("var", true)
	nscope := makeScope() // this is just to get gensym copied into iln.stmtlist.scope, it doesn't go into scopetree
	nscope.Add(gensym, 0)
	newstmts[0] = makeTermT(Vardecl, gensym, TypeNothing, Pos(-1), Pos(-1))
	newstmts[1] = ifxpr
	if ifxpr.Tag() == IfStmt {
		ifxpr0 := ifxpr.(*TermL)
		ifxpr0.args[1] = xfmBranch(ifxpr0.args[1], gensym)
		ifxpr0.args[2] = xfmBranch(ifxpr0.args[2], gensym)
		ifxpr0.dtype = TypeNothing
	} else {
		var args []Term
		if ifxpr.Tag() == Ifcase {
			ifxpr0 := ifxpr.(*TermL)
			args = ifxpr0.args
			ifxpr0.dtype = TypeNothing
		} else {
			ifxpr0 := ifxpr.(*TermTL)
			args = ifxpr0.args
			ifxpr0.dtype = TypeNothing
		}
		for _, clz := range args {
			clz0 := clz.(*TermTT)
			clz0.arg1 = xfmBranch(clz0.arg1, gensym)
		}
	}
	iln.insertOOL(newstmts, nscope, TypeNothing)
	return gensym
}

// Trm is a Range term not the iteree of each. Replace it with code that generates a list.
func (iln *inlineInfo) genRange(trm Term) Term {
	rng := trm.(*TermT)
	rngtyp := rng.arg0.(*TermL).args[0].Dtype() // will be integer or float
	nested := make([]*Scope, 0, 3)
	findNested(iln.stmtlist, &nested)
	symident := identifierOutOfScope(iln.stmtlist.scope, nested, "_v", nil)
	newstmts := make([]Term, 2)
	rslttyp := makeListType(rngtyp)
	gensym := makeSymbol(symident, rslttyp, makeTermT(ZeroValue, nil, rslttyp, Pos(-1), Pos(-1)))
	gensym.plist.Add("var", true)
	nscope := makeScope() // this is just to get gensym copied into iln.stmtlist.scope, it doesn't go into scopetree
	nscope.Add(gensym, 0)
	newstmts[0] = makeTermT(Vardecl, gensym, TypeNothing, Pos(-1), Pos(-1))
	itervar := makeSymbol("i", rngtyp, nil)
	pushb := rslttyp.methods.Find("pushb")
	eb0 := makeFuncall(pushb, []Term{gensym, itervar}, TypeNothing, nil)
	ebody := makeScopedTermL(Stmts, makeScope(), []Term{eb0}, TypeNothing, Pos(-1), Pos(-1))
	each := makeScopedTermL(EachStmt, makeScope(), []Term{itervar, rng, ebody}, TypeNothing, Pos(-1), Pos(-1))
	each.scope.Add(itervar, 0)
	ebody.scope.parent = each.scope
	each.scope.parent = nscope
	newstmts[1] = each
	iln.insertOOL(newstmts, nscope, TypeNothing)
	return gensym
}

// We're given a symbol whose binding is an && or || expression. And we know that at least one of the clauses
// of this expression needs to generate OOL code. We can't do that for fear that an earlier clause protects later
// ones in some way. More precisely, we can't do it because it violates the semantics of && and ||. My favorite solution
// is to replace the condop with a nested if stmt that sets the symbol. We scan the list of clauses and for every one
// that requires OOL coding, we give it a new if stmt that sets the symbol in the right way.
func (iln *inlineInfo) condopsXfm(sym *Symbol) {
	var makeCond = func(ctg Termtag) Term { // if(c) ... for &&; if(!c) ... for ||
		var rslt Term
		if ctg == AndandExpr {
			rslt = sym
		} else {
			rslt = makeTermT(Lognot, sym, TypeBool, Pos(-1), Pos(-1))
		}
		return rslt
	}
	var makeStmtlist = func() *TermL {
		slist := make([]Term, 1)
		scp := makeScope()
		scp.parent = iln.stmtlist.scope
		return makeScopedTermL(Stmts, scp, slist, TypeNothing, Pos(-1), Pos(-1))
	}
	condop := sym.binding.(*TermL)
	tg := condop.Tag()
	inx := 1
	state := 0 // 0:bind to sym, 1:first asgnmt; 2:append ifstmt
	top := len(condop.args)
	ifargs := make([]Term, 2)
	ifargs[0] = makeCond(tg)
	stmts := makeStmtlist()
	ifargs[1] = stmts
	ifstmt := makeTermL(IfStmt, ifargs, TypeNothing, Pos(-1), Pos(-1))
	initial := 0
	var stepstate = func(lo, hi int) {
		var cond Term
		if lo+1 == hi {
			cond = condop.args[lo]
		} else {
			clauses := make([]Term, hi-lo)
			copy(clauses, condop.args[lo:hi])
			cond = makeTermL(tg, clauses, TypeBool, Pos(-1), Pos(-1))
		}
		ifstmt.args[0] = makeCond(tg)
		if state == 0 {
			sym.binding = cond
			state = 1
		} else if state == 1 {
			stmts.args[0] = makeTermTT(AsgnStmt, sym, cond, TypeNothing, Pos(-1), Pos(-1))
			state = 2
		} else {
			nstmts := makeStmtlist()
			nstmts.args[0] = makeTermTT(AsgnStmt, sym, cond, TypeNothing, Pos(-1), Pos(-1))
			stmts.args = append(stmts.args, makeTermL(IfStmt, []Term{makeCond(tg), nstmts}, TypeNothing, Pos(-1), Pos(-1)))
			stmts = nstmts // this is how we get further nesting
		}
	}
	for inx < top {
		cls := condop.args[inx]
		if anyOOL(cls) {
			if initial < inx {
				stepstate(initial, inx)
			}
			stepstate(inx, inx+1)
			initial = inx + 1
		}
		inx += 1
	}
	if initial < top {
		stepstate(initial, top)
	}
	// now we stick the ifstmt *after* the term we received, which we assume is at iln.stmtsinx
	argc := len(iln.stmtlist.args)
	if argc == cap(iln.stmtlist.args) {
		nargs := make([]Term, argc, argc+1)
		copy(nargs, iln.stmtlist.args)
		iln.stmtlist.args = nargs
	}
	iln.stmtlist.args = iln.stmtlist.args[:argc+1]
	si := iln.stmtsinx
	copy(iln.stmtlist.args[si+2:], iln.stmtlist.args[si+1:])
	iln.stmtlist.args[si+1] = ifstmt
}

// We've got a Typecond which arises from a tag check on an ortype. It must be on an ortype-valued symbol.
// Typically, if([Typecond xx T]) A else B. Insert a stmt "x0,x1 := xx.(T)" and replace the [Typecond xx T] with x1.
// Also arrange to replace any [gdref xx T] with x0 and, assuming the ortype is a pair, any [gdref xx U] with
// xx.(U) where U is the other variant of the ortype.
func (iln *inlineInfo) xfmTypecond(tcterm *TermTT) Term {
	tc0 := gensym() // this will have the type requested
	tc1 := gensym() // bool
	sym0 := makeSymbol(tc0, tcterm.arg1.(*Type), nil)
	sym1 := makeSymbol(tc1, TypeBool, nil)
	taterm := makeTermT(TypeAssert, tcterm.arg0, tcterm.arg1.(*Type), Pos(-1), Pos(-1))
	decl := makeTermL(TypeAssertDecl, []Term{sym0, sym1, taterm}, TypeNothing, Pos(-1), Pos(-1))
	scp := makeScope()
	scp.entries.Add(sym0, 0)
	scp.entries.Add(sym1, 0)
	iln.insertOOL([]Term{decl}, scp, TypeNothing)
	return sym1
}

// eachstmt will be an each statement whose iteree obeys the eachstart protocol. Replace it with a stmtlist:
// val self = iteree
// ...code for eachstart(iteree)...
// var _v = [eachstart retval]
// loop { if(_v.eachdone()) break; val itervar = _v.value; body; _v.eachstep() }
// As we do in ifexpr2go, we synth these stmts into a stmtlist then insert that into iln.
// Caller will arrange for the orig each stmt to be deleted.
func (iln *inlineInfo) each2eachstart(eachstmt *TermL, iteree Term) {
	esmthd := iteree.Dtype().methods.Find("eachstart")
	if esmthd == nil {
		panic("internal error: eachstart method")
	}
	esfuni := esmthd.binding.(*Funinst)
	esfuni0 := esfuni.copyFuninst()
	// Shortcut inlining code since we know eachstart signature: this binds self to the actual
	selfsym := esfuni0.funargs[0]
	selfident := identifierOutOfScope(iln.stmtlist.scope, nil, selfsym.ident, selfsym)
	selfsym.ident = selfident
	selfsym.binding = iteree
	// The rettype of esfuni0 has the methods needed to synth the loop
	estyp := esfuni0.dtype.v.(*Ftntype).rettype
	itervar := eachstmt.args[0].(*Symbol)
	eachbody := eachstmt.args[2].(*TermL)
	eachbodyargs := eachbody.args
	symident := identifierOutOfScope(iln.stmtlist.scope, nil, "_v", nil)
	gensym := makeSymbol(symident, estyp, nil)
	gensym.plist.Add("var", true)
	escount := len(esfuni0.body.args)
	// newstmts is for decl self; stmtlist with eachstart body, last expr replaced by _v decl; loopstmt.
	newstmts := make([]Term, escount+2)
	newstmts[0] = makeTermT(Valdecl, selfsym, TypeNothing, Pos(-1), Pos(-1))
	copy(newstmts[1:], esfuni0.body.args)
	gensym.binding = newstmts[escount] // pick up the retval from eachstart
	newstmts[escount] = makeTermT(Vardecl, gensym, TypeNothing, Pos(-1), Pos(-1))
	// now we're ready to synth the loop stmt into newstmts[escount+1]
	nscope := makeScope()
	nscope.Add(gensym, 0)
	loopstmts := make([]Term, 3+len(eachbodyargs))
	// note: we've already copied eachbody so can reuse
	lscope := eachbody.scope
	copy(loopstmts[2:], eachbodyargs)
	edcall := makeTermTL(Funcall, estyp.methods.Find("eachdone"), []Term{gensym}, TypeBool, Pos(-1), Pos(-1))
	ifthen := makeScopedTermL(Stmts, makeScope(),
		[]Term{makeTermT(BreakStmt, nil, TypeNothing, Pos(-1), Pos(-1))}, TypeNothing, Pos(-1), Pos(-1))
	ifstmt := makeTermL(IfStmt, []Term{edcall, ifthen}, TypeNothing, Pos(-1), Pos(-1))
	loopstmts[0] = ifstmt
	vmthd := estyp.methods.Find("value")
	itervar.binding = makeFuncall(vmthd, []Term{gensym}, vmthd.dtype.v.(*Ftntype).rettype, nil)
	loopstmts[1] = makeTermT(Valdecl, itervar, TypeNothing, Pos(-1), Pos(-1))
	eachstepstmt := makeFuncall(estyp.methods.Find("eachstep"), []Term{gensym}, TypeNothing, nil)
	loopstmts[len(loopstmts)-1] = eachstepstmt
	looplist := makeScopedTermL(Stmts, lscope, loopstmts, TypeNothing, Pos(-1), Pos(-1))
	lscope.parent = nscope
	newstmts[escount+1] = makeTermT(LoopStmt, looplist, TypeNothing, Pos(-1), Pos(-1))
	if prInline {
		logger.Println("each2eachstart inserts stmts:")
		for _, s := range newstmts {
			logger.Println(s.String())
		}
	}
	iln.insertOOL(newstmts, nscope, TypeNothing)
}

// Set in compiler settings
var unrollEachloops = true

func shouldUnrollEach(eachstmt *TermL) bool {
	iteree := eachstmt.args[1]
	tfam := iteree.Dtype().family
	if unrollEachloops && tfam == TFList && iteree.Tag() == Litform0 {
		// last test: yes unless body contains break or continue
		var brkcontWF = func(trm Term, _ interface{}) bool {
			return trm.Tag() == BreakStmt || trm.Tag() == ContinueStmt
		}
		return !eachstmt.args[2].Termfind(brkcontWF, nil)
	}
	return false
}

// Called if the iteree is a litform and we've decided to unroll it.
// Subst the value of each litform elt for the itervar and return a stmtlist collecting these.
func (iln *inlineInfo) unrollEach(eachstmt *TermL) {
	iterargs := eachstmt.args[1].(*TermL)
	if prInline {
		logger.Println("unrolling each loop with ", len(iterargs.args), "values:", iterargs.String())
	}
	body := eachstmt.args[2].(*TermL)
	bodycount := len(body.args)
	newstmts := make([]Term, len(iterargs.args)*bodycount)
	// I've got a todo item to clean up the various substitution and copying fns
	// Meanwhile, runSubstn was written assuming the Term it receives can be smashed which isn't
	// the case here, since we want to make one copy of the body for each iteration. The funinst
	// copying code is the best match and we adapt it here to copy just the body of the each stmt.
	// Another issue is locals in the each body. Since we're unrolling, we need to rename these as
	// we go, else we get multiple defns of the same vbl in the unrolled stmts. We do this with a
	// "fake" Scope that keeps track of the identifiers used so far.
	var fcsv = &funiCopySV{eachstmt.scope, eachstmt.scope, nil}
	b4 := []Term{eachstmt.args[0]}
	if len(eachstmt.args) == 4 {
		b4 = append(b4, eachstmt.args[3])
	}
	aftr := make([]Term, len(b4))
	si0 := &substInfo{b4, aftr, nil, false, false}
	localScope := makeScope()
	localScope.parent = eachstmt.scope
	for _, symx := range body.scope.entries {
		localScope.Add(symx.sym, 0)
	}
	for i, value := range iterargs.args {
		aftr[0] = value
		if len(aftr) > 1 {
			aftr[1] = makeIntlit(i)
		}
		bcopy := si0.substn(fcsv.bodyCopy(body)).(*TermL)
		for _, symx := range bcopy.scope.entries {
			id0 := identifierOutOfScope(localScope, nil, symx.sym.ident, symx.sym)
			symx.sym.ident = id0
			localScope.Add(symx.sym, 0)
		}
		copy(newstmts[i*bodycount:], bcopy.args)
	}
	body.scope.entries = localScope.entries // make sure all new locals are recorded
	iln.insertOOL(newstmts, body.scope, TypeNothing)
}

// Substitute code that checks lists for extensional equality. This is done by inlining a "secret"
// method of list named __eq. At entry we know that eqexpr's args are both lists.
func (iln *inlineInfo) listeqXfm(eqexpr *TermTT) Term {
	listT := eqexpr.arg0.Dtype()
	eqmthd := listT.methods.Find("__eq")
	return iln.inlineOne(eqmthd.binding.(*Funinst), []Term{eqexpr.arg0, eqexpr.arg1}, TypeBool)
}

// Handle the case of accessing a gomap only to check if the key is not in it.
// We're going to create a multiret decl of the key lookup and return the bool var.
func (iln *inlineInfo) multiretGomap4Nil(trm *TermTT) Term {
	mrargs := make([]Term, 3)
	mrargs[0] = makeSymbol("_", TypeNothing, nil) // this is ignored, the type is dont-care
	mrargs[1] = makeSymbol(gensym(), TypeBool, nil)
	mrargs[2] = trm.arg0
	newstmts := make([]Term, 2)
	newstmts[0] = makeTermL(Multiret, mrargs, TypeNothing, Pos(-1), Pos(-1))
	newstmts[1] = makeTermT(Lognot, mrargs[1], TypeBool, Pos(-1), Pos(-1))
	tmpscope := makeScope() // vehicle to copy gensym into inserted scope
	tmpscope.Add(mrargs[1].(*Symbol), 0)
	return iln.insertOOL(newstmts, tmpscope, TypeBool)
}

// Trm is a decl of "X:tuple = litform". In order to figure out whether we should "deconstruct" it, check for all
// of the stmts that follow whether X appears in asgnmt or funcall, other than as a symchain root.
// Return false if so, true otherwise.
// Note that this sense of the result is the opposite of what we want. This was more convenient to code though.
func tuplitConditions(trm Term, stmts []Term) bool {
	var tlcondAll func(sym *Symbol, stmts []Term) bool
	var tlcondOne func(sym *Symbol, stmt Term) bool
	tlcondOne = func(sym *Symbol, stmt Term) bool {
		switch s0 := stmt.(type) {
		case *Symbol:
			return sym == s0
		case *Funinst:
			return tlcondAll(sym, s0.body.args)
		case *TermT:
			if s0.kind == Valdecl || s0.kind == Vardecl {
				sym0 := s0.arg0.(*Symbol)
				if sym0.binding != nil {
					//fmt.Println("tlcOne on sym:", sym0.ident, sym0.binding.String())
					return tlcondOne(sym, sym0.binding)
				}
			} else if tlcondOne(sym, s0.arg0) {
				return true
			}
		case *TermTT:
			if s0.kind == AsgnStmt && (s0.arg0 == sym || s0.arg1 == sym) {
				return true
			}
			return tlcondOne(sym, s0.arg0) || tlcondOne(sym, s0.arg1)
		case *TermL:
			if s0.kind == Symchain {
				return false
			}
			return tlcondAll(sym, s0.args)
		case *TermTL: // Typecase isn't handled
			// only check ftn if iaaf
			if s0.kind == Funcall {
				if s0.trm.Tag() == FuninstTag && tlcondOne(sym, s0.trm) {
					return true
				}
				for _, a := range s0.args {
					if sym == a {
						return true
					}
				}
			}
			if tlcondAll(sym, s0.args) {
				return true
			}
		}
		return false
	}
	tlcondAll = func(sym *Symbol, stmts []Term) bool {
		//fmt.Println("tlcAll on ", sym.ident)
		for _, s := range stmts {
			//fmt.Println("tlcAll:", i, s.String())
			if tlcondOne(sym, s) {
				return true
			}
		}
		return false
	}
	return tlcondAll(trm.(*TermT).arg0.(*Symbol), stmts)
}

// Carry out tuple deconstruction: replace trm. The tg arg will be Valdecl or Vardecl, mimicking the tuple decl being replaced
func (iln *inlineInfo) deconstructTuple(sym *Symbol, tg Termtag) Term {
	atrbs := sym.dtype.v.(*Tupletype).attribs
	syms := make([]Term, len(atrbs))
	decls := make([]Term, len(atrbs))
	chains := make([]Term, len(atrbs))
	lfargs := sym.binding.(*TermL).args // binding is known to be a Litform0
	nscope := makeScope()
	for i, atrb := range atrbs {
		ident := sym.ident + atrb.ident
		// todo: add identifierOutOfScope to the above
		nsym := makeSymbol(ident, lfargs[i].Dtype(), lfargs[i])
		if tg == Vardecl {
			nsym.plist.Add("var", true)
		}
		nscope.Add(nsym, 0)
		syms[i] = nsym
		decls[i] = makeTermT(tg, syms[i], TypeNothing, Pos(-1), Pos(-1))
		chains[i] = makeTermL(Symchain, []Term{sym, atrb}, atrb.dtype, Pos(-1), Pos(-1))
	}
	// in the stmts following iln.stmtsinx, substitute the new symbols for symchains involving the old one.
	si0 := &substInfo{chains, syms, nil, false, false}
	ofs := iln.stmtsinx + 1
	for i, s := range iln.stmtlist.args[ofs:] {
		iln.stmtlist.args[ofs+i] = si0.substn(s)
	}
	retval := iln.insertOOL(decls, nscope, TypeNothing)
	if prInline {
		logger.Println("after deconstruct on", sym.ident, "stmtinx=", iln.stmtsinx, "; stmts=", iln.stmtlist.String())
	}
	return retval
}

// For now, a QCInfo is created per call to the query compiler. It's possible that a "real" REPL interpreter
// would benefit from a session-persistent entity, so that's something to consider later.
type QCInfo struct {
	// We build a FSM that speeds up the search for subterms of the query matching any active xprftn
	// Note that in general, entering and exiting scopes as we traverse the query can affect this FSM.
	stmtrules []rwruleInfo
	symrules  []rwruleInfo // top level Symbol rules will be rare; reuse rwruleInfo with tag ignored
	tagrules  []rwruleInfo
	// Next, slots for the info pass that occurs after stmapping
	scopelevel int
	usedTypes  symbolSlice
	// Next, slots for inlining
	inlineCounter    int
	ilnscopes        []*inlineInfo // one per scope
	seen             symbolSlice   // record symbols seen for infloop prevention, etc.
	seentypes        []*Type       // used by pointerizeType for recursion control
	recursiveInlines symbolSlice   // yet another recursion cntl, this one for when recursive fns are (rarely) inlined
	optimizing       *Symbol       // note the thing being optimized, again for recursion control
	externs          symbolSlice   // these end up as globals in the output, plus code for main at index 0
	gomethods        []*Symbol     // UD ftns asserted to be go methods (note: not go methods decld in pkgs)
	tagstack         []Termtag     // lets us know what kind of Term we're embedded in
	// Next, slots for codegen
	precedences  []int       // used in codegen of expressions to control insertion of parens
	imports      []string    // the pkgs imported in the query, sorted on path
	importMnames [][]string  // slice of [path, QI] pairs for the case where an import uses a non-default QI
	postmapGbls  symbolSlice // for codegen, we need a list of the Symbols derived from gbls
	smi          *STMapInfo  // state info for stmapping, saved for occasional use in rewriting
}

// STMapping is done before this is called. The task here is to create the FSM for speeding up subterm matching of
// any active rewrite rule. A separate QCInfo method uses this for rewriting and other backend xfm activities.
func makeQCInfo(rewrites []Term, smi *STMapInfo) *QCInfo {
	qci := &QCInfo{nil, nil, nil, 0, make(symbolSlice, 0, 5), 0,
		make([]*inlineInfo, 0, 2), make(symbolSlice, 0, 8),
		make([]*Type, 0, 2), make(symbolSlice, 0, 2), nil, make([]*Symbol, 0, 8),
		nil, make([]Termtag, 0, 10), make([]int, 0, 8),
		make([]string, 0, 4), make([][]string, 0, 1), make(symbolSlice, 0, 10), smi}
	// Populate the postmapGbls and prep the symdict for use in rwrules by ensuring that anything already
	// copied is not copied again. We do this by collecting all the postsyms and then entering them into symdict as no-ops
	postsyms := make([]*Symbol, 0, len(smi.symdict))
	for _, sde := range smi.symdict {
		k := sde.pre
		if sde.post == nil {
			continue
		}
		if (k.plist.Find("qisym") != nil || gblScope.entries.IndexExact(k) >= 0) && k.dtype != nil {
			if k.dtype.family == TFFtn {
				if k.binding == nil {
					continue
				}
				funi := k.binding.(*Funinst)
				if funi.body == nil || funi.plist.Find("biFtn") != nil {
					continue
				}
				//fmt.Println("admitting gbl ftn ", k.ident)
			}
			qci.postmapGbls.insert(qci.postmapGbls.binsearch(k.ident), sde.post.(*Symbol))
		}
		psym := sde.post
		if psym.Tag() == SymbolTag {
			postsyms = append(postsyms, psym.(*Symbol))
		}
	}
	for _, s := range postsyms {
		smi.symdict.insert(s, s, nil)
	}
	// The given rewrites are from "rewrite(xprftn)" or "rewrite(xprftn(arg1,...))".
	// The code below sorts xprftns into cases that depend on how their matchquery patterns work.
	// The QCInfo.applyRewrites method uses this to avoid most calls to the full pattern matcher as
	// it traverses the term structure of the query.
	for _, rw := range rewrites {
		xprftnsym, issym := rw.(*Symbol)
		actuals := []Term{}
		if !issym {
			rwcall := rw.(*TermTL)
			xprftnsym = rwcall.trm.(*Symbol)
			actuals = rwcall.args
		}
		mqp := xprftnsym.binding.Plist().Find("MQP")
		if mqp == nil {
			panic("impossible")
		}
		xprfuni := xprftnsym.binding.(*Funinst)
		xprfuni.plist.Add("xprname", xprftnsym.ident) // this is for debugging only
		mqp0 := mqp.(*Pattern)
		secondary := secondaryRuleInfo{secCodeNone, ""}
		// xprfuni.scope is the funargs, usually empty. If non-empty, we're going to set the actuals in their bindings.
		// (It's already linked into the scopetree.)
		// This is pretty incompatible with changing the activation of rewrite rules as we flow in and out of scopes,
		// but that feature isn't implemented. And BTW it's even less compatible with activating the same rwrule in two
		// different ways (in shadowing scopes), which is probably a bad idea anyway but isn't explicitly illegal.
		for i, a := range xprfuni.funargs {
			a.binding = actuals[i]
		}
		rwinf := rwruleInfo{[]Termtag{mqp0.tag}, mqp0, xprfuni, secondary, actuals}
		if mqp0.tag == Stmts {
			var seq []Termtag
			if mqp0.flexcount > 0 {
				seq = make([]Termtag, 0, len(mqp0.parts))
				tried := false
				for _, ptn := range mqp0.parts {
					// the following cond corresp to not a TEM
					if ptn.tag != SymbolTag || ptn.flexcount == 0 {
						if ptn.tag == SymbolTag { // here for PEMs
							ptn = ptn.parts[0]
						}
						if !tried { // attempt secondary only on first anchor pt
							secondary.setup(ptn)
							tried = true
						}
						seq = append(seq, ptn.tag)
					}
				}
			}
			rwinf.sequence = seq
			qci.stmtrules = append(qci.stmtrules, rwinf)
		} else if mqp0.tag == SymbolTag {
			qci.symrules = append(qci.symrules, rwinf)
		} else {
			secondary.setup(mqp0)
			qci.tagrules = append(qci.tagrules, rwinf)
		}
	}
	return qci
}

// Add sym to externs with set semantics
func (qci *QCInfo) addExtern(sym *Symbol) {
	for _, sym0 := range qci.externs {
		if sym == sym0 {
			return
		}
	}
	// I want to ensure that gbl symbols don't interfere with anything but qci currently has
	// no notion of scope. For now, I'm using gblScope. In future, maybe a more complete idea of nested.
	if qci.postmapGbls.findsym(sym) < 0 {
		id0 := identifierOutOfScope(gblScope, nil, sym.ident, sym)
		sym.ident = id0
	}
	if prOptimize {
		logger.Println("adding extern ", sym.ident)
	}
	if sym.binding != nil && sym.binding.Tag() == FuninstTag {
		funi := sym.binding.(*Funinst)
		for _, fa := range funi.funargs {
			id0 := identifierOutOfScope(gblScope, nil, fa.ident, fa)
			fa.ident = id0
		}
	}
	//fmt.Println("adding", sym.ident, "to qci.externs")
	qci.externs = append(qci.externs, sym)
}

// store in alpha order of path, not so much to speed up access (though it does) as to make output
// order independent of how query traversal finds pkg references.
func (qci *QCInfo) addImport(pkginfo *QIVal) {
	inx := binsearchStrings(qci.imports, pkginfo.path)
	if inx < 0 || inx == len(qci.imports) || qci.imports[inx] != pkginfo.path {
		qci.imports = insertStrings(qci.imports, inx, pkginfo.path)
		if pkginfo.path != pkginfo.mname {
			slashinx := strings.LastIndexByte(pkginfo.path, '/')
			if slashinx < 0 || pkginfo.mname != pkginfo.path[slashinx+1:] {
				found := false
				for _, pr := range qci.importMnames {
					if pr[0] == pkginfo.path {
						found = true
						break
					}
				}
				if !found {
					qci.importMnames = append(qci.importMnames, []string{pkginfo.path, pkginfo.mname})
				}
			}
		}
	}
}

// only for dbgg
func (scp *Scope) prBindings(prestrg string) {
	fmt.Println(prestrg)
	for _, sx := range scp.entries {
		fmt.Println(sx.sym.ident, ":", sx.sym.binding)
	}
}

// Traverse the given term, which can be modded in place. Apply applicable rewrite rules to do so, keeping track
// of whether anything is changed and returning that in the 2nd retval. The first retval is usually trm
// but if it's modded at top level that's what is returned.
// We return (with the 2nd retval true) after any rewrite occurs in a term, or after a full traversal with no rewrites
// (with 2nd retval false). Note that "after any rewrite occurs in a term" is not the same as "after a single rewrite".
// Complex terms, such as TermL's, can have rewrites applied to each of their subparts. Since the effect of a rewrite
// can't extend beyond it, these are certain to be independent.
func (qci *QCInfo) applyRewrites0(trm Term) (Term, bool) {
	qci.seen = qci.seen[0:0]
	return qci.applyRewrites(trm)
}

// This is the recursion point; see applyRewrites0 for details.
func (qci *QCInfo) applyRewrites(trm Term) (Term, bool) {
	if trm == nil {
		return nil, false
	}
	changed := false
	// step 1: does trm itself match any active rule?
	tg := trm.Tag()
	// symrules are hopefully rare -- they're a single matchable and so we have to do a full pmatch on all of them
	for _, symrule := range qci.symrules {
		tmp, changed := ptnMatch(trm, &symrule, qci.smi)
		if changed || tmp.Tag() == ErrorTag {
			return tmp, true
		}
		// if we get here, the rule skipped out, so keep looking
	}
	// next, check stmtrules if we're at a Stmts term
	if tg == Stmts {
		trmargs := trm.(*TermL).args
		for _, sr := range qci.stmtrules {
			if len(sr.sequence) == 0 {
				panic("should not happen")
			}
			runpm := false
			sinx := 0
			for _, s := range trmargs {
				if s.Tag() == sr.sequence[sinx] {
					sinx++
					if sinx == len(sr.sequence) {
						runpm = true
						break
					}
				}
			}
			if runpm {
				tmp, changed := ptnMatch(trm, &sr, qci.smi)
				if tmp.Tag() == ErrorTag {
					return tmp, true
				}
				if changed {
					// Here's a subtlety: relink the scopetree
					tmp.(*TermL).scope.parent = trm.(*TermL).scope.parent
					return tmp, true
				}
				// fall through means look at next rule
			}
		}
	} else {
		// not at a Stmts term; check the "regular" rules
		for _, tgrule := range qci.tagrules {
			if tgrule.sequence[0] == tg && tgrule.addcrit.check(trm) {
				tmp, changed := ptnMatch(trm, &tgrule, qci.smi)
				if changed || tmp.Tag() == ErrorTag {
					return tmp, true
				}
			}
		}
	}
	// step 2: Nothing matched here so recursively apply rewrites to subterms
	// Note: ignoring TermB on the assumption that no rule will match.
	switch trm0 := trm.(type) {
	case *Symbol:
		if qci.processed(trm0) {
			return trm, changed
		}
		if trm0.binding != nil && trm0.binding.Tag() != TypeTag {
			bdg, changed0 := qci.applyRewrites(trm0.binding)
			changed = changed || changed0
			trm0.binding = bdg
		}
	case *Funinst:
		if trm0.body != nil {
			qci.smi.scopetree = trm0.scope
			body0, changed0 := qci.applyRewrites(trm0.body)
			qci.smi.scopetree = trm0.scope.parent
			changed = changed || changed0
			//fmt.Println("post-RW: ", body0.String())
			if changed0 {
				rettype := trm0.Dtype().v.(*Ftntype).rettype
				if body0.Tag() == Stmts {
					body0.(*TermL).dtype = rettype
				} else {
					body0 = makeScopedTermL(Stmts, makeScope(), []Term{body0}, rettype, body0.First(), body0.Final())
				}
				trm0.body = body0.(*TermL)
				body0.(*TermL).scope.parent = trm0.scope
			}
		}
	case *TermT:
		if trm0.arg0 != nil && trm0.kind != Fwddecl {
			a0, changed0 := qci.applyRewrites(trm0.arg0)
			changed = changed || changed0
			trm0.arg0 = a0
		}
	case *TermTT:
		if trm0.arg0 != nil {
			a0, changed0 := qci.applyRewrites(trm0.arg0)
			changed = changed || changed0
			trm0.arg0 = a0
		}
		if trm0.arg1 != nil {
			a1, changed0 := qci.applyRewrites(trm0.arg1)
			changed = changed || changed0
			trm0.arg1 = a1
		}
	case *TermL:
		if trm0.scope != nil {
			qci.smi.scopetree = trm0.scope
		}
		insertedStmtlist := false
		for i, t := range trm0.args {
			if t != nil {
				//tstrg := t.String()
				tmp, changed0 := qci.applyRewrites(t)
				changed = changed || changed0
				insertedStmtlist = insertedStmtlist || (changed0 && tmp.Tag() == Stmts)
				trm0.args[i] = tmp
			}
		}
		if trm0.scope != nil {
			qci.smi.scopetree = trm0.scope.parent
		}
		// The rewrite engine flattens stmtlists internally, but it can't handle this case, so we do it here.
		if trm0.Tag() == Stmts && insertedStmtlist {
			flattenStmts1(trm0)
		}
	case *TermTL:
		tmp, changed0 := qci.applyRewrites(trm0.trm)
		changed = changed || changed0
		trm0.trm = tmp
		for i, t := range trm0.args {
			tmp, changed0 = qci.applyRewrites(t)
			changed = changed || changed0
			trm0.args[i] = tmp
		}
	}
	return trm, changed
}

// typ has tnm0 in its plist as "typename" so we want to use it in codegen. We need to check for dups and
// rename if we find them, in addition to handling the ordered insert.
// Also, since every pkg defined type with go methods is named, this is a convenient place to
// mark these as needing to be generated.
func (qci *QCInfo) markUsedType(typ *Type, tnm0 string) {
	inx := qci.usedTypes.binsearch(tnm0)
	if inx >= 0 { // cannot be a dup if < 0
		// check for re-occurrence of a type already seen, because of renaming it can only be same ident
		sym := qci.usedTypes[inx]
		if sym.ident == tnm0 && sym.dtype == typ {
			return // re-occurrence
		}
		if sym.ident == tnm0 {
			// must rename; borrow from identifierOutOfScope, but this isn't scope based so not identical
			renamed := false
			for i := 0; i < 100; i++ {
				istrg := fmt.Sprintf("%d", i)
				ident0 := tnm0 + istrg
				inx = qci.usedTypes.binsearch(ident0)
				if inx < 0 || qci.usedTypes[inx].ident != ident0 {
					renamed = true
					typ.plist.Add("typename", ident0)
					typ.cachedString = ident0
					tnm0 = ident0
					break
				}
			}
			if !renamed {
				panic("hunh?")
			}
		}
	}
	qci.usedTypes.insert(inx, makeSymbol(tnm0, typ, nil))
	for _, mx := range typ.methods {
		gm := mx.sym.plist.Find("Gomethod")
		if gm != nil {
			found := false
			for _, xx := range qci.gomethods {
				if xx == mx.sym {
					found = true
					break
				}
			}
			if !found {
				qci.gomethods = append(qci.gomethods, mx.sym)
			}
		}
	}
}

// Collect named types into the usedTypes map.
// While we're at it, get ready for any generic fns and uses of typecase.
func (qci *QCInfo) namedTypes(trm Term) {
	if trm == nil {
		return
	}
	// ensure that named types get recorded in the usedTypes symbolSlice
	var collectType func(typ *Type, seen *[]*Type)
	collectType = func(typ *Type, seen *[]*Type) {
		for _, t := range *seen {
			if t == typ {
				return
			}
		}
		*seen = append(*seen, typ)
		tnm := typ.plist.Find("typename")
		if tnm != nil && !typ.isNilposs() {
			qci.markUsedType(typ, tnm.(string))
		}
		switch typ.family {
		case TFList:
			collectType(typ.v.(*Type), seen)
		case TFSpace:
			collectType(typ.v.(*Spacetype).elttype, seen)
		case TFTuple:
			for _, a := range typ.v.(*Tupletype).attribs {
				collectType(a.dtype, seen)
			}
		case TFFtn:
			for _, t := range typ.v.(*Ftntype).fmlargs {
				collectType(t, seen)
			}
			collectType(typ.v.(*Ftntype).rettype, seen)
		case TFTypred:
			typred := typ.v.(*Typred)
			if !(typred.ident == "list" || typred.ident == "entity") {
				for _, mthd := range typred.affs {
					if strings.Contains(mthd.ident, "<>+-/*%") || mthd.ident[0] == '_' {
						panic("cannot (yet) handle non-alphabetic method identifiers")
					}
					ident0 := mthd.ident
					byte0 := strings.ToUpper(ident0[0:1])
					if byte0 != ident0[0:1] {
						// smash to capitalized, since symbols are shared this should propagate (?)
						mthd.ident = byte0 + ident0[1:]
					}
				}
			}
		}
	}
	tg := trm.Tag()
	typ := trm.Dtype()
	seen := []*Type{}
	if typ != nil {
		collectType(typ, &seen)
	}
	switch trm0 := trm.(type) {
	case *Symbol:
		if trm0.dtype == nil {
			collectType(trm0.binding.(*Type), &seen)
		} else {
			collectType(trm0.dtype, &seen)
		}
	case *Funinst:
		// collect info on the body if present
		// Note that this is visited only if defined within query scope
		if trm0.body != nil {
			trm0.codesize = genCodesize(trm0)
			qci.namedTypes(trm0.body)
		}
	case *TermT:
		if tg == Valdecl || tg == Vardecl {
			sym := trm0.arg0.(*Symbol) // since this is a decl, binding should never be nil
			if sym.binding != nil {
				if sym.binding.Tag() == TypeTag {
					collectType(sym.binding.(*Type), &seen)
				} else {
					qci.namedTypes(sym.binding)
				}
			}
		} else if trm0.arg0 != nil {
			qci.namedTypes(trm0.arg0)
		}
	case *TermTT:
		qci.namedTypes(trm0.arg0)
		qci.namedTypes(trm0.arg1)
	case *TermL:
		if tg == Stmts {
			qci.scopelevel += 1
		}
		for _, s := range trm0.args {
			if s != nil {
				qci.namedTypes(s)
			}
		}
		if tg == Stmts {
			qci.scopelevel -= 1
		}
	case *TermTL:
		qci.namedTypes(trm0.trm)
		for _, s := range trm0.args {
			qci.namedTypes(s)
		}
		if trm0.kind == Typecase {
			reflinx := binsearchStrings(qci.imports, "reflect")
			if reflinx >= 0 && qci.imports[reflinx] == "reflect" {
				// check the cases and if any are structural, mark that we need the reflect pkg
				for _, clz := range trm0.args {
					casetyp := clz.(*TermTT).arg0.(*Type)
					if casetyp.family == TFTypred && casetyp.v.(*Typred).ident != "entity" {
						// note that we'll translate entity as default
						qci.addImport(&QIVal{nil, false, 1, "reflect", "reflect"})
						break
					}
				}
			}
		}
	}
}

// Can be called with any type, will respond yes with either a nilposs type of basic family or a basic
// family. The latter assumes the arg is the main variant of a nilposs type.
func nilpossPtrzble(typ *Type) bool {
	if typ.isNilposs() {
		typ = typ.mainType()
	}
	return typ.family == TFString || typ.family == TFInt || typ.family == TFFloat || typ.family == TFByte
}

// Final step before codegen: wrap types needing pointers, simplify nilposs(T) to *T where relevant, etc.
// This is all done via modding.
func (qci *QCInfo) pointerize() {
	qci.pointerize0(qci.externs[0].binding)
	for _, s := range qci.externs[1:] {
		qci.pointerize0(s)
		qci.pointerize0(s.binding)
	}
	for i, ts := range qci.usedTypes {
		qci.usedTypes[i].dtype = pointerizeType(ts.dtype, &qci.seentypes)
	}
}

func pointerizeType(typ *Type, seen *[]*Type) *Type {
	for i := 0; i < len(*seen); i += 2 {
		if typ == (*seen)[i] {
			return (*seen)[i+1]
		}
	}
	pkgt := typ.plist.Find("noPointer")
	if pkgt != nil {
		return typ
	}
	// This is where to cvt pointerizable nilposs types
	if typ.isNilposs() {
		mntyp := typ.mainType()
		pmn := pointerizeType(mntyp, seen)
		if pmn != mntyp {
			if mntyp.family == TFTuple {
				pmn.plist.Add("wasNilposs", true)
			}
			return pmn // note: will already be on seen dict
		}
		// special case int, string, float
		if nilpossPtrzble(mntyp) {
			return mntyp.wrapPointer()
		}
		return pmn
	} else if typ.family == TFList {
		// create a new type that's retractable
		eltyp := typ.v.(*Type)
		nlt := makeType(TFList, eltyp)
		nseen := len(*seen)
		*seen = append(*seen, typ, nlt)
		tmp := pointerizeType(eltyp, seen)
		if tmp == eltyp {
			// retract
			copy((*seen)[nseen:], (*seen)[nseen+2:])
			*seen = (*seen)[:len(*seen)-2]
		} else {
			nlt.v = tmp
			nlt.plist = *typ.plist.Copy()
			return nlt
		}
	} else if typ.family == TFOrtype {
		ort := typ.v.(*Ortype)
		nvnts := make([]*Symbol, len(ort.variants))
		changed := false
		for i, vnt := range ort.variants {
			vnt0 := vnt
			tmp := pointerizeType(vnt.dtype, seen)
			if tmp != vnt.dtype {
				changed = true
				vnt0 = makeSymbol(vnt.ident, tmp, nil)
			}
			nvnts[i] = vnt0
		}
		if changed {
			typ = makeType0(TFOrtype, &Ortype{nvnts, false}, typ.cachedString)
		}
	} else if typ.family == TFSpace {
		spctyp := typ.v.(*Spacetype)
		spctyp.elttype = pointerizeType(spctyp.elttype, seen)
	} else if typ.family == TFGochan {
		chantyp := typ.v.(*ChanInfo)
		chantyp.elttype = pointerizeType(chantyp.elttype, seen)
	} else if typ.family == TFGomap {
		pt := typ.v.([]*Type)
		nkt := pointerizeType(pt[0], seen)
		if pt[0].family == TFTuple {
			nkt = pt[0] // don't ptrize tuples that are keys
		}
		nvt := pointerizeType(pt[1], seen)
		if nkt != pt[0] || nvt != pt[1] {
			typ = makeType0(TFGomap, []*Type{nkt, nvt}, typ.cachedString)
		}
	} else if typ.family == TFFtn {
		changed := false
		ftntyp := typ.v.(*Ftntype)
		argsx := make([]*Type, len(ftntyp.fmlargs))
		for i, ty := range ftntyp.fmlargs {
			argsx[i] = pointerizeType(ty, seen)
			// special wrap for slice rcvrs of mod ftns
			if i == 0 && !ftntyp.pure && (ty.family == TFList || ty.family == TFSpace) {
				argsx[i] = ty.wrapPointer()
			}
			changed = changed || argsx[i] != ty
		}
		retx := pointerizeType(ftntyp.rettype, seen)
		if changed || retx != ftntyp.rettype {
			nft := makeType(TFFtn, argsx, retx, ftntyp.pure)
			*seen = append(*seen, typ, nft)
			return nft
		}
	}
	if typ.family == TFTuple {
		ptrzd := typ.wrapPointer()
		*seen = append(*seen, typ, ptrzd)
		ptrzd.plist = *typ.plist.Copy()
		tupt := typ.v.(*Tupletype)
		nattribs := make([]*Symbol, len(tupt.attribs))
		for i, a := range tupt.attribs {
			nattribs[i] = makeSymbol(a.ident, pointerizeType(a.dtype, seen), nil)
		}
		// underlying type needs copy of plist??
		typ0 := makeType0(TFTuple, &Tupletype{nattribs, tupt.situated}, typ.cachedString)
		typ0.plist = *typ.plist.Copy()
		ptrzd.v = typ0
		return ptrzd
	}
	return typ
}

// This is the recursion point for pointerize.
func (qci *QCInfo) pointerize0(trm Term) {
	switch trm0 := trm.(type) {
	case *Symbol:
		if trm0.dtype != nil {
			trm0.dtype = pointerizeType(trm0.dtype, &qci.seentypes)
		}
	case *Funinst:
		trm0.dtype = pointerizeType(trm0.dtype, &qci.seentypes)
		for _, fa := range trm0.funargs {
			qci.pointerize0(fa)
		}
		if trm0.body != nil {
			qci.pointerize0(trm0.body)
		}
	case *TermT:
		if trm0.kind == Valdecl || trm0.kind == Vardecl {
			sym := trm0.arg0.(*Symbol)
			if sym.dtype != nil {
				dtyb4 := sym.dtype
				sym.dtype = pointerizeType(sym.dtype, &qci.seentypes)
				// a nilposs tuple type with zerovalue initzn gets initzd to nil instead
				if sym.binding != nil && sym.binding.Tag() == ZeroValue && dtyb4.isNilposs() && dtyb4.mainType().family == TFTuple {
					sym.binding = nilEntity
				} else {
					qci.pointerize0(sym.binding)
				}
			}
		} else if trm0.kind == Gdref {
			qci.pointerize0(trm0.arg0)
			if trm0.arg0.Dtype().family == TFPtr {
				trm0.dtype = trm0.dtype.wrapPointer()
			} else {
				seen := []*Type{}
				trm0.dtype = pointerizeType(trm0.dtype, &seen)
			}
		} else {
			trm0.dtype = pointerizeType(trm0.dtype, &qci.seentypes)
			if trm0.arg0 != nil {
				qci.pointerize0(trm0.arg0)
			}
		}
	case *TermTT:
		trm0.dtype = pointerizeType(trm0.dtype, &qci.seentypes)
		// TermTT can be Arrow which can have both entity and type exprs on lhs
		if trm0.arg0.Tag() == TypeTag {
			trm0.arg0 = pointerizeType(trm0.arg0.(*Type), &qci.seentypes)
		} else {
			qci.pointerize0(trm0.arg0)
		}
		qci.pointerize0(trm0.arg1)
	case *TermL:
		trm0.dtype = pointerizeType(trm0.dtype, &qci.seentypes)
		for _, x := range trm0.args {
			qci.pointerize0(x)
		}
	case *TermTL:
		trm0.dtype = pointerizeType(trm0.dtype, &qci.seentypes)
		qci.pointerize0(trm0.trm)
		// avoid ptrzg the key of gomap oprns
		noPtr1 := -1
		if callingNamed(trm0, "lvalindex") || callingNamed(trm0, "rvalindex") || callingNamed(trm0, "rvalindexWD") {
			if trm0.args[0].Dtype().family == TFGomap {
				noPtr1 = 1
			}
		}
		for inx, x := range trm0.args {
			if inx != noPtr1 {
				qci.pointerize0(x)
			}
		}
		// post-ptrz fixup for the asgnftn case: change ftn dtype (wrt rcvr) and dtype of rcvr actual
		if trm0.trm.Tag() == SymbolTag {
			funi := trm0.trm.(*Symbol).binding
			if funi != nil && itemExists(funi, "asgnftn") {
				fntyp := trm0.trm.Dtype().v.(*Ftntype)
				fntyp.fmlargs[0] = fntyp.fmlargs[0].wrapPointer()
				funi0 := funi.(*Funinst)
				rcvrtyp := funi0.funargs[0].dtype
				funi0.funargs[0].dtype = pointerizeType(rcvrtyp, &qci.seentypes).wrapPointer()
				funi0.funargs[0].plist.Add("ptrzdRcvr", true)
				rcvrtyp = trm0.args[0].Dtype().wrapPointer()
				smashDtype(trm0.args[0], rcvrtyp)
			}
		}
	}
}

// This is a helper for noticing the *[]X pattern that occurs for receivers of certain mod ftns
func isPtr2List(trm Term) bool {
	typ := trm.Dtype()
	return typ.family == TFPtr && typ.v.(*Type).family == TFList
}

var callcount = 0

// Similarly, this is for noticing things like *int used to code nilposs.
// The hackish bit for Goerror is because it's coded as nilposs(string). I should probably change that instead
// of doing this.
func isNPviaPtr(trm Term) bool {
	callcount += 1
	if callcount >= 30 {
		fmt.Print()
	}
	typ := trm.Dtype()
	if typ.family == TFPtr && typ != TypeGoerror {
		ptr2 := typ.v.(*Type).family
		return ptr2 == TFInt || ptr2 == TFString || ptr2 == TFFloat
	}
	return false
}

// Fill in an initially empty list of the nested scopes of trm, which normally starts as a Stmts term.
// This code traverses everywhere except into symbols not being decld, in order to capture embedded
// Funinsts. The order of scopes is lexico plus from least to most nested; for funis the funi scope is first.
func findNested(trm Term, nested *[]*Scope) {
	if trm != nil {
		switch trm1 := trm.(type) {
		case *Symbol:
		case *TermB:
		case *Type:
		case *TermT:
			if trm1.kind == Valdecl || trm1.kind == Vardecl {
				sym := trm1.arg0.(*Symbol)
				// this doesn't traverse type defns but I think those don't matter here.
				if sym.dtype != nil && sym.binding != nil {
					findNested(sym.binding, nested)
				}
			} else {
				findNested(trm1.arg0, nested)
			}
		case *TermTT:
			findNested(trm1.arg0, nested)
			findNested(trm1.arg1, nested)
		case *TermL:
			if trm1.scope != nil {
				// scope == nil can be ok, eg Litform0 and more
				*nested = append(*nested, trm1.scope)
			}
			for _, a := range trm1.args {
				findNested(a, nested)
			}
		case *TermTL:
			findNested(trm1.trm, nested)
			for _, a := range trm1.args {
				findNested(a, nested)
			}
		case *Funinst:
			// We don't go through Symbols so this is a thunk or iaaf, can't have nil body
			if trm1.scope == nil {
				panic("got nil scope")
			}
			if len(trm1.scope.entries) > 0 {
				*nested = append(*nested, trm1.scope)
			}
			findNested(trm1.body, nested)
		default:
			panic("unhandled in findNested")
		}
	}
}

// another debug helper: scan a Funinst for consistency of scope trees
// returns true if things check out, false if any error
type chkscopeInfo struct {
	scopestack []*Scope
	printall   bool
	errcnt     int
}

func (chki *chkscopeInfo) push(scp *Scope) {
	chki.scopestack = append(chki.scopestack, scp)
}
func (chki *chkscopeInfo) pop() {
	chki.scopestack = chki.scopestack[:len(chki.scopestack)-1]
}

// Check the scopetree for scp; call this before pushing scp onto scopestack
// Note that this is a diagnostic function only
func (chki *chkscopeInfo) check1(scp *Scope) {
	curscp := scp.parent
	if chki.printall {
		fmt.Print(scp.String())
	}
	sep := ": "
	top := len(chki.scopestack)
	for i := 1; i <= len(chki.scopestack); i++ {
		onstack := chki.scopestack[top-i]
		if curscp == nil {
			fmt.Println("got nil vs ", onstack.String())
			chki.errcnt++
			return
		}
		if curscp.plist.Find("givenscope") != nil {
			curscp = curscp.parent
		}
		if onstack.plist.Find("givenscope") != nil {
			onstack = onstack.parent
		}
		if curscp != onstack {
			fmt.Println("expected ", onstack.String(), " but got ", curscp.String())
			chki.errcnt++
			return
		} else if chki.printall {
			fmt.Print(sep, onstack.String())
			sep = ", "
		}
		curscp = curscp.parent
	}
	fmt.Println()
}

// This is the recursion point for scope checking
func (chki *chkscopeInfo) checkAll(trm Term) {
	if trm == nil {
		return
	}
	switch trm1 := trm.(type) {
	case *Symbol:
	case *TermB:
	case *TermT:
		if trm1.kind == Valdecl || trm1.kind == Vardecl {
			sym := trm1.arg0.(*Symbol)
			// this doesn't traverse type defns but I think those don't matter here.
			if sym.dtype != nil && sym.binding != nil {
				chki.checkAll(sym.binding)
			}
		} else {
			chki.checkAll(trm1.arg0)
		}
	case *TermTT:
		chki.checkAll(trm1.arg0)
		chki.checkAll(trm1.arg1)
	case *TermL:
		if trm1.scope != nil {
			chki.check1(trm1.scope)
			chki.push(trm1.scope)
		}
		for _, a := range trm1.args {
			chki.checkAll(a)
		}
		if trm1.scope != nil {
			chki.pop()
		}
	case *TermTL:
		chki.checkAll(trm1.trm)
		for _, a := range trm1.args {
			chki.checkAll(a)
		}
	case *Funinst:
		// We don't go through Symbols so this is a thunk or iaaf, can't have nil body
		if trm1.scope == nil {
			panic("got nil scope")
		}
		chki.check1(trm1.scope)
		chki.push(trm1.scope)
		chki.checkAll(trm1.body)
		chki.pop()
	default:
		panic("unhandled in checkScopes")
	}
}

// Entry point for scope checking: create a chkscopeInfo and call checkAll.
func checkScopes(trm Term, printall bool) bool {
	chki := chkscopeInfo{make([]*Scope, 0, 4), printall, 0}
	chki.push(gblScope)
	//tg := trm.Tag()
	//if tg == FuninstTag {
	//	chki.push(trm.(*Funinst).scope)
	//} else if tg == Stmts {
	//	chki.push(trm.(*TermL).scope)
	//}
	chki.checkAll(trm)
	return chki.errcnt == 0
}

// Find and return an identifier based on ident that doesn't conflict with anything in the scope of a Stmts term whose
// Scope is used to search upscope and nested to search downscope. We always look both ways, this is overly
// conservative but shouldn't cause problems. Since ftns (esp BI methods) sometimes don't link back to gblscope, we
// refnc it as a gbl.
// We're planning to inject a symbol whose identifier is ident into this scope, to make that work we'll rename Symbols
// with identifier ident found here and in nested scopes. Here, "based on" means we try ident, ident0, ident1, ident2, etc.
// The nested scopes part requires a traversal of trm, looking for Stmt terms nested in Ifstmt, etc. Here, we assume that
// traversal has generated the list of nested scopes.
// We pass in the symbol from which the ident comes so that we don't try to rename that one. Sometimes we're called without
// a symbol in which case origsym comes in as nil.
func identifierOutOfScope(scope *Scope, nested []*Scope, ident string, origsym *Symbol) string {
	ident0 := ident
	for i := 0; i < 100; i++ {
		var sym *Symbol
		if scope != nil {
			sym, _ = scope.Lookup(ident0)
		}
		if sym == nil || origsym == sym {
			// Either there isn't a direct symbol or there is but it's the origsym
			found := false
			for _, ns := range nested {
				esym := ns.entries.Find(ident0)
				if esym != nil && esym != origsym {
					found = true
					break
				}
			}
			if !found {
				rsvinx := binsearchStrings(reservedInGo, ident0)
				if rsvinx < 0 || reservedInGo[rsvinx] != ident0 {
					return ident0
				}
			}
		}
		istrg := fmt.Sprintf("%d", i)
		ident0 = ident + istrg
	}
	panic("I give up")
}

// This alternate version of identifierOutOfScope (aka "IOOS") is added late in the game as a workaround for
// a fairly serious bug in cleanup. The bug is this: when a val symbol gets bound to another symbol, cleanup treats it
// as a synonym and tends to merge the two into (as it happens) the 2nd decld symbol and delete its decln (because the
// first symbol decln point is where the original 2nd is now decld). When it's going to do this, it checks for interference
// on the second symbol, the one that remains. The serious bug is that in general, you can't do this unless you check for
// interference across the entire lifetime of the first, but we don't have easy access to that decln point when we're
// doing this.
/*
func identifierOutOfScope2(scope *Scope, nested []*Scope, sym0 *Symbol) string {
	ident0 := ident
	for i := 0; i < 100; i++ {
		var sym *Symbol
		if scope != nil {
			sym, _ = scope.Lookup(ident0)
		}
		if sym == nil {
			found := false
			for _, ns := range nested {
				if ns.entries.Find(ident0) != nil {
					found = true
					break
				}
			}
			if !found {
				rsvinx := binsearchStrings(reservedInGo, ident0)
				if rsvinx < 0 || reservedInGo[rsvinx] != ident0 {
					return ident0
				}
			}
		}
		istrg := fmt.Sprintf("%d", i)
		ident0 = ident + istrg
	}
	panic("I give up")
}
*/

// Rename any gbl ("extern") symbols with conflicting names. Do this by smashing the idents of relevant Symbols.
func (qci *QCInfo) rename2go() {
	idents := make([]string, 0, len(qci.externs))
	type conflictT struct {
		id   string
		cntr int
	}
	var handleConflict = func(cflct *conflictT, sym *Symbol) bool {
		var strg string
		for j := cflct.cntr; j < 100; j++ {
			strg = fmt.Sprintf("%s%d", cflct.id, j)
			inx := binsearchStrings(idents, strg)
			if inx < 0 || idents[inx] != strg {
				idents = insertStrings(idents, inx, strg)
				cflct.cntr = j + 1
				sym.ident = strg // smash!
				return true
			}
		}
		return false
	}
	conflicts := make([]conflictT, 0, 10)
	for _, sym := range qci.externs {
		inx := binsearchStrings(idents, sym.ident)
		if inx < 0 || idents[inx] != sym.ident {
			// here, it's first time, ie no conflict
			idents = insertStrings(idents, inx, sym.ident)
		} else {
			// conflict here
			handled := false
			for _, cflct := range conflicts {
				if cflct.id == sym.ident {
					handled = handleConflict(&cflct, sym)
					if !handled {
						panic("out of patience")
					}
					break
				}
			}
			// here, handled is false iff this is a new conflict; create a confictT in that case
			if !handled {
				cflct := conflictT{sym.ident, 0}
				conflicts = append(conflicts, cflct)
				if !handleConflict(&cflct, sym) {
					panic("passing strange")
				}
			}
		}
	}
}

// Special augmentation of substn on a stmtlist for inlining purposes. The augmentation is that if lvalindexHack
// is set, we replace an asgnmt with first Term in b4 on lhs with lvalindex. If set, the first Term in
// b4 will be an rvalindex funcall.
// Since this is but a small variant on std substn, we use that.
func subst4Inline(trm Term, b4, aftr []Term, lvalHack bool) *TermL {
	if len(b4) == 0 {
		return trm.(*TermL)
	}
	sbi := &substInfo{b4, aftr, make([]int, len(b4)), lvalHack, false}
	return sbi.substn(trm).(*TermL)
}

// Another helper for inlining. This one abstracts out detection of symbols already processed, due to
// recursion or any other cause.
func (qci *QCInfo) processed(sym *Symbol) bool {
	if qci.seen.findsym(sym) >= 0 {
		return true
	}
	qci.seen.insert(qci.seen.binsearch(sym.ident), sym)
	return false
}

// Helper for inlining: select and return the top elt of inlineInfo stack
func (qci *QCInfo) activeILN() *inlineInfo {
	nscopes := len(qci.ilnscopes)
	if nscopes == 0 {
		return nil
	}
	iln := qci.ilnscopes[nscopes-1]
	return iln
}

// Called from OOLXforms with the binding of a symbol being declared; check if it is some kind of call to a multiret ftn
// and if so, change it to a Multiret term. We look for two forms: a direct call and a call wrapped in a deref of one of
// the attribs of the returned tuple. In the second case, we're ignoring the other branch. Check if this branch is an
// error return and if so, do the go convention thing, pass it along to panic.
func tryMultiret(sym *Symbol, scp *Scope) (retval Term) {
	retval = sym
	bdg := sym.binding
	symtag := bdg.Tag()
	multival := false
	form := SymbolTag
	if symtag == Funcall && bdg.(*TermTL).trm.Tag() == SymbolTag {
		multival = itemExists(bdg.(*TermTL).trm, "multiret")
	} else if symtag == Symchain && bdg.(*TermL).args[0].Tag() == Funcall {
		fcall := bdg.(*TermL).args[0].(*TermTL)
		if fcall.trm.Tag() == SymbolTag {
			multival = itemExists(fcall.trm, "multiret")
			form = Symchain
		}
	}
	if multival {
		mrargs := make([]Term, 3)
		pairT := sym.dtype
		if form == Symchain {
			pairT = bdg.(*TermL).args[0].Dtype()
		}
		var typ1, typ2 *Type
		if pairT.isNilposs() {
			// meaning: encoded as nilposs in d8m and [T, bool] multiret in go. It's possible this only
			// applies to Gomap.rvalindex.
			typ1 = pairT.mainType()
			typ2 = TypeBool
		} else if pairT.family == TFTuple && len(pairT.v.(*Tupletype).attribs) == 2 {
			atrbs := pairT.v.(*Tupletype).attribs
			typ1 = atrbs[0].dtype
			typ2 = atrbs[1].dtype
		} else {
			panic("shouldn't happen")
		}
		// the symchain case must select one of the 2 attribs which normally will be the first; but check anyway
		chaintag := 0
		if form == Symchain && typ2.Equal(sym.dtype) {
			chaintag = 1
			mrargs[0] = makeSymbol(gensym(), typ1, nil)
			mrargs[1] = makeSymbol(sym.ident, typ2, nil)
		} else {
			mrargs[0] = makeSymbol(sym.ident, typ1, nil)
			mrargs[1] = makeSymbol(gensym(), typ2, nil)
		}
		scp.Add(mrargs[0].(*Symbol), 0)
		scp.Add(mrargs[1].(*Symbol), 0)
		mrargs[2] = bdg
		retval = makeTermL(Multiret, mrargs, TypeNothing, Pos(-1), Pos(-1))
		if form == Symchain {
			retval.Plist().Add("symchain", chaintag)
		}
	}
	return
}

// Helper for when a decl becomes a Multiret: formulate exprs to substitute and apply to stmts. Oldsym comes in
// as a Term but is known to be a *Symbol; similarly, mrstmt has Multiret tag and is therefore a *TermL.
// Multiret handles 2 kinds of retvals: certain nilposs(T) become T or boolean; certain pair-tuples get deconstructed.
// Check the type of oldsym to figure out which of these applies.
// Besides the flavor of the multiret call itself, there's also an option for it to be immediately consumed by refncg
// one of the retvals, usually the first. This applies especially with the common go pattern where the 2nd retval is error
// when non-nil and the std coding practice is to check it for that and panic if so. Rather than make people do this in d8m,
// I've decided to code it for them so in those cases you can immediately take the first attrib and the err checking will
// be done anyway.
func (iln *inlineInfo) multiretSubstn(oldsym0, mrstmt0 Term) {
	oldsym := oldsym0.(*Symbol)
	mrstmt := mrstmt0.(*TermL)
	nsym1 := mrstmt.args[0].(*Symbol)
	nsym2 := mrstmt.args[1].(*Symbol)
	b4 := make([]Term, 2)
	aftr := make([]Term, 2)
	if oldsym.dtype.isNilposs() {
		// here, "oldsym = nil" --> nsym2 and [gdref oldsym] --> nsym1 (where the gdref is to maintype
		b4[0] = makeTermTT(EqExpr, oldsym, nilEntity, TypeBool, Pos(-1), Pos(-1))
		b4[1] = makeTermT(Gdref, oldsym, oldsym.dtype.mainType(), Pos(-1), Pos(-1))
		aftr[0] = makeTermT(Lognot, nsym2, TypeBool, Pos(-1), Pos(-1))
		aftr[1] = nsym1
		mntyp := oldsym.dtype.mainType()
		if nilpossPtrzble(mntyp) {
			oldsym.dtype = mntyp
			if oldsym.binding != nil {
				smashDtype(oldsym.binding, mntyp)
			}
		}
	} else if oldsym.binding.Tag() == Symchain {
		// here, substn on the selected branch is between old and new symbols; the other retval is inaccessible
		b4[0] = oldsym
		aftr[0] = nsym1
		b4 = b4[0:1]
		aftr = aftr[0:1]
		mrstmt.args[2] = mrstmt.args[2].(*TermL).args[0] // strip the symchain, similar to substn
		if nsym2.dtype.family == TFGoerror {
			// this is where we create the std go behavior: check and exit
			cond := makeTermTT(EqExpr, nsym2, nilEntity, TypeBool, Pos(-1), Pos(-1))
			errstrg := makeFuncall(TypeGoerror.methods[0].sym, []Term{nsym2}, TypeString, nil)
			callexit := makeFuncall(biScope["exit"][0], []Term{errstrg}, TypeExit, nil)
			cestmts := makeScopedTermL(Stmts, makeScope(), []Term{callexit}, TypeNothing, Pos(-1), Pos(-1))
			checkstmt := makeTermL(IfStmt, []Term{cond, cestmts}, TypeNothing, Pos(-1), Pos(-1))
			args := iln.stmtlist.args
			if len(args) == cap(args) {
				argsx := make([]Term, len(args), len(args)+1)
				copy(argsx, args)
				args = argsx
			}
			args = args[:len(args)+1]
			copy(args[iln.stmtsinx+2:], args[iln.stmtsinx+1:])
			iln.stmtlist.args = args
			args[iln.stmtsinx+1] = checkstmt
		} else {
			// otherwise, smash the 2nd multiret symbol to "_" so go compiler doesn't complain
			nsym2.smash2Anon()
		}
	} else if oldsym.dtype.family == TFTuple {
		// here, the substn lhs on both branches is a symchain rooted at oldsym
		ostyp := oldsym.dtype
		ttyp := ostyp.v.(*Tupletype)
		if len(ttyp.attribs) != 2 {
			panic("can't happen")
		}
		b4[0] = makeTermL(Symchain, []Term{oldsym, ttyp.attribs[0]}, ttyp.attribs[0].dtype, Pos(-1), Pos(-1))
		b4[1] = makeTermL(Symchain, []Term{oldsym, ttyp.attribs[1]}, ttyp.attribs[1].dtype, Pos(-1), Pos(-1))
		aftr[0] = nsym1
		aftr[1] = nsym2
	}
	// unwrap substn so we can access the substn count info it generates
	stmts := iln.stmtlist.args[iln.stmtsinx+1:]
	si0 := &substInfo{b4, aftr, make([]int, len(b4)), false, false}
	for i, s := range stmts {
		stmts[i] = si0.substn(s)
	}
	if si0.counts[0] == 0 {
		nsym1.smash2Anon()
	}
	if len(si0.counts) > 1 && si0.counts[1] == 0 {
		nsym2.smash2Anon()
	}
}

// Similar to multiretSubstn but slightly different. In particular, the first stmt is the type assert decl.
func typeAssertSubstn(stmts []Term) {
	mrstmt0 := stmts[0].(*TermL)
	typasrt := mrstmt0.args[2].(*TermT)
	b4 := make([]Term, 1, 2)
	aftr := make([]Term, 1, 2)
	ort := typasrt.arg0.Dtype()
	b4[0] = makeTermT(Gdref, typasrt.arg0, typasrt.dtype, Pos(-1), Pos(-1))
	aftr[0] = mrstmt0.args[0] // subst the validated type that the type assert decl checks on
	vnts := ort.v.(*Ortype).variants
	if len(vnts) == 2 {
		vinx := 0
		if typasrt.dtype == vnts[0].dtype {
			vinx = 1
		}
		b4 = append(b4, makeTermT(Gdref, typasrt.arg0, vnts[vinx].dtype, Pos(-1), Pos(-1)))
		aftr = append(aftr, makeTermT(TypeAssert, typasrt.arg0, vnts[vinx].dtype, Pos(-1), Pos(-1)))
	}
	for i, s := range stmts[1:] {
		stmts[i+1] = runSubstn(b4, aftr, s, false)
	}
}

// Should be called on a "stringify" method whose body is nil. Synthesize based on the argtype and mod the body.
func (sym *Symbol) synthStringify() {
	// local helper to synth a call to stringify, first vfyg that it's defined for the type
	var sfycall = func(attrib *Symbol, fml0 Term) Term {
		var schn Term
		if fml0 == nil {
			schn = attrib
		} else {
			schn = makeTermL(Symchain, []Term{fml0, attrib}, attrib.dtype, Pos(-1), Pos(-1))
		}
		var ret Term
		if attrib.dtype.family == TFString {
			ret = schn
		} else {
			stgfy := attrib.dtype.stringifyMethod(nil)
			ret = makeFuncall(stgfy, []Term{schn}, TypeString, nil)
		}
		return ret
	}

	rcvrtype := sym.dtype.v.(*Ftntype).fmlargs[0]
	sprintf := biScope["__sprintf"][0]
	funi := sym.binding.(*Funinst)
	body := makeScopedTermL(Stmts, makeScope(), make([]Term, 0, 1), TypeString, Pos(-1), Pos(-1))
	switch rcvrtype.family {
	default:
		panic("unhandled case")
	case TFInt, TFInt64:
		fmtstrg := makeStringTerm("%d", Pos(-1), false)
		body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
	case TFFloat:
		fmtstrg := makeStringTerm("%.6f", Pos(-1), false)
		body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
	case TFString:
		fmtstrg := makeStringTerm("%v", Pos(-1), false)
		body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
	case TFByte:
		fmtstrg := makeStringTerm("%v", Pos(-1), false)
		body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
	case TFBool:
		fmtstrg := makeStringTerm("%v", Pos(-1), false)
		body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
	case TFLabel:
		// Not really safe...
		body.args = append(body.args, makeFuncall(sprintf, []Term{funi.funargs[0]}, TypeString, sym))
	case TFSpace:
		if rcvrtype == biScope["bits64"][0].binding.(*Type) {
			fmtstrg := makeStringTerm("%v", Pos(-1), false)
			body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
		} else {
			panic("space case not written")
		}
	case TFOrtype:
		// this is a kluge, needs fixing
		fmtstrg := makeStringTerm("%v", Pos(-1), false)
		body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
		// below is a first attempt at fixing, but optimzn and codegen of typecase is immature
		/*		ort := rcvrtype.v.(*Ortype)
				if ort.isNilposs {
					fmtstrg := makeStringTerm("%v", Pos(-1), false)
					body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
				} else {
					clauses := make([]Term, len(ort.variants))	// each clause a TermTT with arg0 the variant type
					for i, v := range ort.variants {
						gdref := makeTermT(Gdref, funi.funargs[0], v.dtype, Pos(-1), Pos(-1))
						scall := makeFuncall(v.dtype.stringifyMethod(), []Term{gdref}, TypeString, nil)
						clz := makeTermTT(Arrow, v.dtype, scall, TypeNothing, Pos(-1), Pos(-1))
						clauses[i] = clz
					}
					casestmt := makeTermTL(Typecase, funi.funargs[0], clauses, TypeNothing, Pos(-1), Pos(-1))
					body.args = append(body.args, casestmt)
				}*/
	case TFGomap:
		// the following is a bit of a cheat
		fmtstrg := makeStringTerm("%v", Pos(-1), false)
		body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
	case TFTuple, TFList:
		if itemExists(rcvrtype, "interfaceType") {
			body.args = append(body.args, makeStringTerm("can't print intfc type", Pos(-1), false))
		} else if itemExists(rcvrtype, "pkgtype") {
			// can't do this with bldr, let go fmt handle it
			fmtstrg := makeStringTerm("%v", Pos(-1), false)
			body.args = append(body.args, makeFuncall(sprintf, []Term{fmtstrg, funi.funargs[0]}, TypeString, sym))
		} else {
			// Here's are the non-trivial cases with some sharing. Use strings.Builder:
			var strgsinfo *QIVal
			pmi := QIPathMap["strings"]
			if pmi != nil && pmi.pkg != nil {
				strgsinfo = pmi.pkg
			} else {
				inx := binsearchStrings(pkgdbNames, "strings")
				pkgdb[inx].AddPkg("strings", "strings")
				strgsinfo = QIScope["strings"]
			}
			strgscp := strgsinfo.scope
			bldrT := strgscp.entries.Find("Builder").binding.(*Type)
			bldr := makeSymbol("bldr", bldrT, makeTermT(ZeroValue, nil, bldrT, Pos(-1), Pos(-1)))
			body.scope.Add(bldr, 0)
			body.scope.parent = funi.scope
			bldrdecl := makeTermT(Vardecl, bldr, TypeNothing, Pos(-1), Pos(-1))
			wstrg := bldrT.methods.Find("WriteString")
			openbrkt := makeFuncall(wstrg, []Term{bldr, makeStringTerm("[", Pos(-1), false)}, TypeNothing, nil)
			closebrkt := makeFuncall(wstrg, []Term{bldr, makeStringTerm("]", Pos(-1), false)}, TypeNothing, nil)
			if rcvrtype.family == TFTuple {
				// code for tuple is
				// decl bldr; write(open bracket); each(attrib) {write stringify elt and separator}; write(close); gen string.
				ttyp := rcvrtype.v.(*Tupletype)
				atrb0 := makeFuncall(wstrg, []Term{bldr, sfycall(ttyp.attribs[0], funi.funargs[0])}, TypeNothing, nil)
				body.args = append(body.args, bldrdecl, openbrkt, atrb0)
				for _, a := range ttyp.attribs[1:] {
					sep := makeFuncall(wstrg, []Term{bldr, makeStringTerm("; ", Pos(-1), false)}, TypeNothing, nil)
					nxwrite := makeFuncall(wstrg, []Term{bldr, sfycall(a, funi.funargs[0])}, TypeNothing, nil)
					body.args = append(body.args, sep, nxwrite)
				}
				body.args = append(body.args, closebrkt)
			} else {
				// decl bldr and index of last elt; write(open bracket);
				// each(elt^fmlarg[0]) {write stringify elt; if(index < last) write separator}; write(close); gen string.
				eltyp := rcvrtype.v.(*Type)
				tci := makeTCInfo(funi.scope)
				lsbdg := parseString("count(arg)-1").Typecheck(TypeInt, tci)
				lastsym := makeSymbol(gensym(), TypeInt, lsbdg)
				lastdecl := makeTermT(Vardecl, lastsym, TypeNothing, Pos(-1), Pos(-1))
				inxsym := makeSymbol(gensym(), TypeInt, makeIntlit(0))
				inxsym.plist.Add("var", true)
				inxdecl := makeTermT(Vardecl, inxsym, TypeNothing, Pos(-1), Pos(-1))
				body.scope.Add(lastsym, 0)
				body.scope.Add(inxsym, 0)
				itervar := makeSymbol(gensym(), eltyp, nil)
				ebody := makeScopedTermL(Stmts, makeScope(), make([]Term, 0, 1), TypeNothing, Pos(-1), Pos(-1))
				nxwrite := makeFuncall(wstrg, []Term{bldr, sfycall(itervar, nil)}, TypeNothing, nil)
				cond := makeTermT(Lognot, makeTermTT(EqExpr, lastsym, inxsym, TypeBool, Pos(-1), Pos(-1)), TypeBool, Pos(-1), Pos(-1))
				sepwrite := makeFuncall(wstrg, []Term{bldr, makeStringTerm(", ", Pos(-1), false)}, TypeNothing, nil)
				tpart := makeScopedTermL(Stmts, makeScope(), []Term{sepwrite}, TypeNothing, Pos(-1), Pos(-1))
				tpart.scope.parent = ebody.scope
				sepcond := makeTermL(IfStmt, []Term{cond, tpart}, TypeNothing, Pos(-1), Pos(-1))
				incrrhs := makeFuncall(TypeInt.methods.Find("+"), []Term{inxsym, makeIntlit(1)}, TypeNothing, nil)
				incr := makeTermTT(AsgnStmt, inxsym, incrrhs, TypeNothing, Pos(-1), Pos(-1))
				ebody.args = append(ebody.args, nxwrite, sepcond, incr)
				each := makeScopedTermL(EachStmt, makeScope(), []Term{itervar, funi.funargs[0], ebody}, TypeNothing, Pos(-1), Pos(-1))
				each.scope.Add(itervar, 0)
				ebody.scope.parent = each.scope
				each.scope.parent = body.scope
				body.args = append(body.args, bldrdecl, inxdecl, lastdecl, openbrkt, each, closebrkt)
			}
			out := makeFuncall(bldrT.methods.Find("String"), []Term{bldr}, TypeString, nil)
			body.args = append(body.args, out)
			//fmt.Println("synthd stringify body for type", rcvrtype.String())
		}
	}
	body.scope.parent = funi.scope
	funi.body = body
}

// Inlining is conceptually straightforward but the corner cases get tricky. We're going to inject a bunch of
// local definitions into the calling context; need to avoid identifier conflicts with those and with the formal
// args. If the inlined function is a mod function that assigns to its receiver, we need to convey that assignment
// out into the context receiving the inlined code. If the thing passed to the receiver is a rvalindex, we need to
// change it to lvalindex.
//

/*
func (trml *TermL) prScopetree() {
	var bldr strings.Builder
	var tgstrg = "Stmts"
	tg := trml.Tag()
	if tg == IfStmt {
		tgstrg = "Ifstmt"
	} else if tg == EachStmt {
		tgstrg = "Each"
	} else if tg == Litform0 {
		tgstrg = "LF0"
	} else if tg != Stmts {
		tgstrg = fmt.Sprint(tg)
	}
	bldr.WriteString(fmt.Sprint("prScopetree in ", tgstrg, ": "))
	if trml.scope == nil {
		bldr.WriteString("nil")
	} else {
		for scp := trml.scope; scp != nil; scp = scp.parent {
			ecnt := len(scp.entries)
			if ecnt > 0 {
				bldr.WriteString(fmt.Sprint(ecnt, " syms, [0]=", scp.entries[0].sym.ident, "; "))
			} else {
				bldr.WriteString("no syms; ")
			}
		}
	}
	fmt.Println(bldr.String())
}
*/

// OOLXforms occasionally wants to be able to check the next higher tag; tagUp enables this
func (qci *QCInfo) pushTag(tg Termtag) {
	qci.tagstack = append(qci.tagstack, tg)
}

func (qci *QCInfo) popTag() {
	qci.tagstack = qci.tagstack[:len(qci.tagstack)-1]
}

func (qci *QCInfo) tagUp() Termtag {
	ntags := len(qci.tagstack)
	if ntags == 1 {
		return NoTerm
	} else {
		return qci.tagstack[ntags-2]
	}
}

// True if trm or any subterm thereof requires an OOL xfm.
// This is currently used to trigger condopsXfm but might someday be useful for other things.
func anyOOL(trm Term) bool {
	var anyOOLAll func(stmts []Term) bool
	var anyOOLOne func(stmt Term) bool
	anyOOLAll = func(stmts []Term) bool {
		for _, s := range stmts {
			if anyOOLOne(s) {
				return true
			}
		}
		return false
	}
	anyOOLOne = func(stmt Term) bool {
		switch s0 := stmt.(type) {
		case *TermTL:
			if s0.kind == Funcall {
				if s0.trm.Tag() == FuninstTag { // iaafs must be inlined
					return true
				}
				fnsym := s0.trm.(*Symbol)
				// hack alert: "simpleInline" is posted to a few BI methods, like count, so we don't OOL just for that
				if fnsym.binding != nil && fnsym.plist.Find("simpleInline") == nil && fnsym.binding.(*Funinst).inlineable(s0.args) {
					return true
				}
			}
			return anyOOLAll(s0.args)
		case *TermL:
			if s0.kind == IfStmt {
				return s0.dtype != TypeNothing
			}
			return anyOOLAll(s0.args)
		case *TermT:
			if s0.arg0 != nil {
				return anyOOLOne(s0.arg0)
			}
		case *TermTT:
			if s0.kind == Typecond {
				return true
			} else if s0.kind == EqExpr { // eqexpr has 2 cases: extensional equality or special casing of Gomap check for nil
				a0t := s0.arg0.Dtype()
				if a0t.family == TFList && !(s0.arg1.Tag() == Litform0 && len(s0.arg1.(*TermL).args) == 0) {
					// actually, I think this is a funcall that's usually inlined, might not require OOL
					return true
				}
				if symbolNamed(s0.arg1, "nil") && callingNamed(s0.arg0, "rvalindex") {
					rcvrtyp := s0.arg0.(*TermTL).trm.(*Symbol).dtype.v.(*Ftntype).fmlargs[0]
					if rcvrtyp.family == TFGomap && rcvrtyp.v.([]*Type)[1].family != TFTuple {
						return true
					}
				}
			}
			return anyOOLOne(s0.arg0) || anyOOLOne(s0.arg1)
		}
		return false
	}
	return anyOOLOne(trm)
}

var prInline = true
var prCleanup = true
var prOptimize = true
var prTiming = false
var prRewrite = true

// Traverse trm, performing all transforms that might introduce "out of line" statements. (Hence "OOL".)
// Historically, inlining was the first of these and the code started out just inlining. However, the machinery to
// replace an expression with something calculated earlier is needed not only for inlining but for various other backend
// transforms as well. Thus, inlining got generalized.
// This is the recursion point for the traversal, so qci needs to be set up before the call and the original
// trm passed in must be a Stmts term. Much of the effect of inlining is seen via modding of the original term, but the
// value returned from OOLXforms usually replaces the original expr. (Nil for stmts means delete.)
func (qci *QCInfo) OOLXforms(trm Term) (retval Term) {
	tg := trm.Tag()
	qci.pushTag(tg)
	defer qci.popTag()
	iln := qci.activeILN()
	// Here, look for OOL transforms based first on termtag, then on whatever auxiliary conditions apply
	if tg == Funcall {
		// If we inline here, we'll return the answer for insertion into whatever cxt is calling.
		fn0 := trm.(*TermTL)
		ok2inline := fn0.trm.Tag() == FuninstTag
		var funi *Funinst
		var inliningWhat *Symbol
		if ok2inline {
			funi = fn0.trm.(*Funinst)
			//fmt.Print("checking a funi with", len(funi.body.args), "lines")
		} else {
			sym, isSym := fn0.trm.(*Symbol)
			if isSym {
				symbdg := sym.binding
				if symbdg == nil && itemExists(sym, "tpaffdnc") {
					// a bit hackish but... A typepred affordance might have gotten inlined; in that case we should fix it.
					// The affordance is a method, so we can find the real function as a method of arg 0
					fntyp := sym.dtype.v.(*Ftntype)
					realftn := fntyp.fmlargs[0].methodLookup(sym.ident)
					if realftn == nil {
						log.Fatal("whoops, couldn't find the affordance method: " + sym.ident)
					}
					symbdg = realftn.binding
					fn0.trm = realftn
				}
				if symbdg != nil {
					var isFuni bool
					funi, isFuni = symbdg.(*Funinst)
					if isFuni {
						inliningWhat = sym
						ok2inline = funi.inlineable(fn0.args)
						// limit inlining of recursive fns to once per pass (which requires clearing qci.recursiveInlines)
						// also, completely forbid inlining sym if that's what we're optimizing
						if ok2inline && sym.plist.Find("recursive") != nil {
							if sym == qci.optimizing || qci.recursiveInlines.findsym(sym) >= 0 {
								ok2inline = false
							} else {
								qci.recursiveInlines.insert(qci.recursiveInlines.binsearch(sym.ident), sym)
							}
						}
					}
				}
			}
		}
		if ok2inline {
			if prInline {
				if inliningWhat != nil {
					logger.Println("...inlining", inliningWhat.ident)
					//if inliningWhat.ident == "__v10" {
					//	fmt.Print()
					//}
				} else {
					logger.Println("...inlining a funi")
				}
				logger.Println("before inlining, iln.stmtinx=", iln.stmtsinx, "; nstmts=", len(iln.stmtlist.args))
				logger.Println("and we're inlining ", funi.String(), "with args", fn0.args)
			}
			// special case the list methods that accept guarded args
			if inliningWhat != nil && !fn0.dtype.isNilposs() &&
				(inliningWhat.ident == "last" || inliningWhat.ident == "head" || inliningWhat.ident == "tail") &&
				len(fn0.args) == 1 && fn0.args[0].Dtype().family == TFList {
				// this somewhat hackish code counts on (1) the 3 named ftns consist of a single if expr; (2) the guarded
				// equivalent is just the else clause.
				// Make a copy, then smash the if expr with its else part, then that's a good funi to inline.
				funi = funi.copyFuninst()
				ftyp := funi.dtype.v.(*Ftntype)
				funi.dtype = makeType(TFFtn, ftyp.fmlargs, fn0.dtype, ftyp.pure)
				funi.body.args[0] = funi.body.args[0].(*TermL).args[2].(*TermL).args[0]
			}
			retval = iln.inlineOne(funi, fn0.args, fn0.dtype)
			// The following is not great. I found a bug (with great difficulty) due to the fact that an
			// inline that returns just an ifstmt as the retval needs to be ifexpr2go'd but is not. That's a
			// problem because the scope tree of the ITE is screwed up. Very likely the right soln is to ensure
			// that the scope tree is good but this works too.
			if retval != nil && retval.Tag() == IfStmt {
				retval = qci.OOLXforms(retval) // must ifexpr2go
			}
			qci.inlineCounter++
			if prInline {
				logger.Println("after: ", iln.stmtlist.String(), "; retval=", retval)
			}
			return
		} else if fn0.trm.Tag() == SymbolTag {
			// not ok2inline and a symbol: possibly extern it and do a few other checks
			fnsym := fn0.trm.(*Symbol)
			if fnsym.ident == "stringify" && fnsym.binding.(*Funinst).body == nil {
				fnsym.synthStringify()
				// in certain cases we fail to inline if we move fwd here
				return qci.OOLXforms(fn0)
			} else if fnsym.ident == "rvalindexWD" && fnsym.binding == nil {
				// this is for Gomap; check if default is ok
				valT := fn0.dtype
				dval := fn0.args[2]
				seen := make([]*Type, 2)
				if !(Manifest(dval) && dval.Equal(defaultValue(valT, &seen))) {
					panic("write code to fix rvalindexWD with non-std default value")
				}
			} else if fnsym.binding != nil && fnsym.binding.Tag() == FuninstTag {
				bfun := fnsym.binding.(*Funinst)
				if bfun.body != nil && !bfun.isLocalfn {
					qci.addExtern(fnsym)
				}
			}
			// Finally, this seems like a good place to scan print and println for things to stringify
			if fnsym.ident == "println" || fnsym.ident == "print" {
				for i, a := range fn0.args {
					atyp := a.Dtype()
					if atyp.family == TFString {
						continue // else infloop
					}
					stgfy := atyp.stringifyMethod(nil)
					fn0.args[i] = makeFuncall(stgfy, []Term{a}, TypeString, a)
				}
			}
		}
	} else if tg == Valdecl || tg == Vardecl {
		var tuplitDecl = func(trm Term) bool {
			decld := trm.(*TermT).arg0.(*Symbol)
			return decld.dtype != nil && decld.dtype.family == TFTuple && decld.binding != nil && decld.binding.Tag() == Litform0
		}
		// handle case of symbol bound to condop; this case is created by the next clause
		bdg := trm.(*TermT).arg0.(*Symbol).binding
		if bdg != nil && (bdg.Tag() == AndandExpr || bdg.Tag() == OrorExpr) {
			condop := bdg.(*TermL)
			mustmove := condop.plist.Find("OOLCondop") != nil
			if !mustmove {
				for _, cls := range condop.args[1:] {
					if anyOOL(cls) {
						mustmove = true
						break
					}
				}
			}
			if mustmove {
				iln.condopsXfm(trm.(*TermT).arg0.(*Symbol))
				if prInline {
					logger.Println("after condopsXfm: ", iln.stmtlist.String())
				}
				return trm
			}
		} else if tuplitDecl(trm) && !tuplitConditions(trm, iln.stmtlist.args[iln.stmtsinx+1:]) {
			// consider tuple deconstruction, note that the stmtlist for tuplitConditions assumes trm came from iln.stmtsinx
			return iln.deconstructTuple(trm.(*TermT).arg0.(*Symbol), trm.Tag())
		}
	} else if (tg == IfStmt || tg == Ifcase || tg == Typecase) && trm.Dtype() != TypeNothing {
		ok2go := true
		if tg == IfStmt {
			trml := trm.(*TermL)
			// note that if dtype isn't nothing, it should be ITE, not just else.
			ok2go = !(hasReturn(trml.args[1]) || hasReturn(trml.args[2]))
		}
		if ok2go {
			return iln.ifexpr2go(trm)
		}
	} else if tg == EachStmt {
		// There's an option to unroll each: if the iteree is literal and "short"
		// Note that 10 is picked out of the air
		trm0 := trm.(*TermL)
		if shouldUnrollEach(trm0) {
			iln.unrollEach(trm0)
			qci.inlineCounter += 1
			return // will be nil
		}
		iteree := trm0.args[1]
		tfam := iteree.Dtype().family
		// check if any enum changes are needed
		// Note: the typred clause is for generic code
		if !(tfam == TFList || tfam == TFSpace || tfam == TFTypred) {
			// yes, changes needed
			a0 := modEAPEnumerable(iteree) // if non-nil, this replaces iteree
			if a0 != nil {
				trm0.args[1] = a0
			} else {
				iln.each2eachstart(trm0, iteree)
				return // will be nil
			}
		}
	} else if tg == AndandExpr || tg == OrorExpr {
		// Since decls with this binding were already checked, being here means we might need to move the expr to a decl
		tgu := qci.tagUp()
		if tgu != Valdecl && tgu != Vardecl {
			mustmove := false
			for _, cls := range trm.(*TermL).args[1:] {
				if anyOOL(cls) {
					mustmove = true
					break
				}
			}
			if mustmove {
				tmp := makeSymbol(gensym(), TypeBool, trm)
				trm.Plist().Add("OOLCondop", true) // flag that we did this
				scp := makeScope()
				scp.Add(tmp, 0)
				if prInline {
					logger.Println("OOLCondop: adding decl of", tmp.String(), "; binding=", tmp.binding.String())
				}
				iln.insertOOL([]Term{makeTermT(Valdecl, tmp, TypeNothing, Pos(-1), Pos(-1))}, scp, TypeNothing)
				return tmp
			}
		}
	} else if tg == Typecond {
		return iln.xfmTypecond(trm.(*TermTT))
	} else if tg == EqExpr {
		trm0 := trm.(*TermTT)
		argtyp := trm0.arg0.Dtype()
		// Two checks here: (1) reqmt to synth extensional equality check; (2) throwaway multiret nil checks.
		// Note that recursive types aren't handled for extensional equality; will likely cause runtime stack overflow
		if argtyp.family == TFList {
			// handle "lst = []" as a special case
			if !(trm0.arg1.Tag() == Litform0 && len(trm0.arg1.(*TermL).args) == 0) {
				return iln.listeqXfm(trm0)
			}
		} else if argtyp.family == TFSpace {
			spctyp := argtyp.v.(*Spacetype)
			// Not written but special-case bitset64 as it will be handled in gencode.
			if spctyp.elttype != TypeBit {
				panic("unwritten")
			}
		} else if symbolNamed(trm0.arg1, "nil") && callingNamed(trm0.arg0, "rvalindex") {
			// do multiretGomap4Nil if the rvalindex rcvr is a Gomap with a non-tuple value type
			rcvrtyp := trm0.arg0.(*TermTL).trm.(*Symbol).dtype.v.(*Ftntype).fmlargs[0]
			if rcvrtyp.family == TFGomap && rcvrtyp.v.([]*Type)[1].family != TFTuple {
				return iln.multiretGomap4Nil(trm0)
			}
		}
	} else if tg == Range {
		return iln.genRange(trm)
	}
	retval = trm
	switch trm0 := trm.(type) {
	case *Symbol:
		// calling processed also adds trm0 to seen list; filter type decls
		if (trm0.dtype != nil || trm0.binding != nil && trm0.binding.Tag() != TypeTag) && !qci.processed(trm0) {
			pkginfo := trm0.plist.Find("pkgsym")
			if pkginfo != nil {
				qci.addImport(pkginfo.(*QIVal))
			}
			pkginfo = trm0.dtype.plist.Find("pkgsym")
			if pkginfo != nil {
				qci.addImport(pkginfo.(*QIVal))
			}
			// if trm0 designates a non-ftn, we need to extern it if it's a global. We've created qci.postmapGbls
			// just for this purpose
			if qci.postmapGbls.findsym(trm0) >= 0 {
				qci.addExtern(trm0)
			}
		}
	case *Funinst:
		if trm0.body != nil {
			trm0.body = qci.OOLXforms(trm0.body).(*TermL)
		}
	case *TermT:
		if tg == Valdecl || tg == Vardecl {
			sym := trm0.arg0.(*Symbol)
			if sym.binding != nil && sym.binding.Tag() != TypeTag {
				if sym.binding.Tag() != FuninstTag {
					tmp := tryMultiret(sym, iln.stmtlist.scope)
					if tmp != sym {
						retval = tmp
						return
					}
				}
				tmp := qci.OOLXforms(sym.binding)
				if tmp.Tag() == Stmts && len(tmp.(*TermL).args) == 1 {
					tmp = tmp.(*TermL).args[0]
				}
				sym.binding = tmp
			}
		} else if trm0.arg0 != nil {
			trm0.arg0 = qci.OOLXforms(trm0.arg0)
		}
	case *TermTT:
		trm0.arg0 = qci.OOLXforms(trm0.arg0)
		trm0.arg1 = qci.OOLXforms(trm0.arg1)
	case *TermL:
		if tg == Stmts {
			// here, we've got a new scope
			ilncount := len(qci.ilnscopes)
			iln0 := inlineInfo{trm0, nil, 0}
			if prInline {
				logger.Println("PUSHING A ILNSCOPE cnt=", ilncount)
			}
			qci.ilnscopes = append(qci.ilnscopes, &iln0)
			// must do this with open loop because callee may change state of iln0; this way we iteratively handle nested inlining
			//ilcb4 := qci.inlineCounter
			for iln0.stmtsinx < len(iln0.stmtlist.args) {
				stmt0 := iln0.stmtlist.args[iln0.stmtsinx]
				if prInline {
					logger.Println("considering stmt", iln0.stmtsinx, "of", len(iln0.stmtlist.args), ":", stmt0)
				}
				tmp := qci.OOLXforms(stmt0)
				if tmp != nil && tmp.Tag() == Stmts {
					// insert the stmts as if OOL, upcoming insertStmts will take care of the rest
					tmp0 := tmp.(*TermL)
					if len(tmp0.args) > 0 {
						iln0.insertOOL(tmp0.args, tmp0.scope, TypeNothing) // should be tmp0.dtype??
					}
					tmp = nil // stmt is to be replaced
				}
				if tmp == nil { // delete stmt at stmtsinx
					if prInline {
						logger.Println("deleting ", stmt0)
					}
					copy(iln0.stmtlist.args[iln0.stmtsinx:], iln0.stmtlist.args[iln0.stmtsinx+1:])
					iln0.stmtlist.args = iln0.stmtlist.args[:len(iln0.stmtlist.args)-1]
				} else {
					iln0.stmtlist.args[iln0.stmtsinx] = tmp
					if tmp.Tag() == Multiret && stmt0.Tag() != Multiret {
						// Here, stmt0 is a std decl and OOLXforms changed it to a Multiret term. Now we're at stmt
						// level; we need to substitute new vbls for old in rest of scope: stmt0 has old, tmp has new
						iln0.multiretSubstn(stmt0.(*TermT).arg0, tmp)
					} else if tmp.Tag() == TypeAssertDecl {
						typeAssertSubstn(iln0.stmtlist.args[iln0.stmtsinx:])
					}
				}
				oolcnt := len(iln0.oolStmts)
				iln0.insertStmts()
				if prInline && oolcnt > 0 {
					logger.Println("after insertion:", iln0.stmtlist)
				}
			}
			qci.ilnscopes = qci.ilnscopes[:ilncount] // pop
			if prInline {
				logger.Println("POPPING ILNSCOPE cnt=", ilncount)
			}
		} else {
			// Other TermL's, like IfStmt, don't have their own scope so no need to push+pop
			// Specifically exempt range as iteree of each.
			for i, x := range trm0.args {
				if x != nil {
					if tg == EachStmt && i == 1 && x.Tag() == Range {
						// We've got special code to turn non-iteree ranges into actual list structure but we still need
						// to check inside ranges in each. So we do it here manually.
						xt := x.(*TermT)
						xt.arg0 = qci.OOLXforms(xt.arg0)
					} else {
						trm0.args[i] = qci.OOLXforms(x)
					}
				}
			}
		}
	case *TermTL:
		// This is pretty much funcall, eg falling through because we didn't inline (above).
		// For fn, call for Funinst (it might be IAAF); call actual args too because this is a traversal.
		trm0.trm = qci.OOLXforms(trm0.trm)
		for i, x := range trm0.args {
			trm0.args[i] = qci.OOLXforms(x)
		}
	}
	if retval != nil {
		retval = Simplify(retval)
	}
	return
}

// Special traversal to handle various builtin rewrites that belong near the end of the optimzn process. Rewrites done:
// - turn X = [] into len(X) = 0
// - adding attrib refnc to a non-list iteree of each that's enumerable because of "assert enumerable(attrib)"
// (Note that eachstart-enabled iterees are handled in OOLXforms, so they shouldn't occur here.)
// - add explicit return stmts to make go code correct
// - remove Gdrefs that are about to become wrong because of nilposs to ptr xfms
// In addition, this pass notes (in qci.modules) all the imported pkgs, and checks for the special case that all
// calls to some fn in qci.externs were eliminated in optimzn. Since fns are added to qci.externs only if not inlined,
// this can only happen if a recursive fn is partly inlined due to having gnc args and a typecase.
func (qci *QCInfo) backendFixups() {
	gncRecursed := make([]int, 0, 2)
	for inx, sym := range qci.externs {
		if sym.plist.Find("recursive") != nil && sym.binding.(*Funinst).anyGncSignature() >= 0 {
			gncRecursed = append(gncRecursed, inx)
		}
	}
	for i := len(gncRecursed) - 1; i >= 0; i-- {
		soughtInx := gncRecursed[i]
		sought := qci.externs[soughtInx]
		stillInUse := false
		for j, tgt := range qci.externs {
			if j == soughtInx {
				continue
			}
			if TraversePre(tgt.binding, identWorkfn, sought) {
				stillInUse = true
			}
		}
		if !stillInUse {
			if prOptimize {
				logger.Println("deleting extern ", soughtInx, " = ", qci.externs[soughtInx].ident)
			}
			qci.externs.delete(soughtInx)
		}
	}
	trm0, ch := qci.backendFixups0(qci.externs[0].binding)
	if ch {
		qci.externs[0].binding = trm0
	}
	for _, sym := range qci.externs[1:] {
		if sym.binding.Tag() == FuninstTag {
			funi := sym.binding.(*Funinst)
			funi0, ch := qci.backendFixups0(funi)
			if ch {
				sym.binding = funi0
			}
		} else if sym.binding.Tag() == Funcall && sym.binding.(*TermTL).trm.Tag() == FuninstTag {
			funi := sym.binding.(*TermTL).trm.(*Funinst)
			funi0, ch := qci.backendFixups0(funi)
			if ch {
				sym.binding.(*TermTL).trm = funi0
			}
		}
	}
	for _, sym := range qci.gomethods {
		trm0, ch = qci.backendFixups0(sym.binding)
		if ch {
			sym.binding = trm0
		}

	}
}

// Check if trm's enumerability is EAP based and if so, return a modified term that's enumerable in the normal way.
// Assume this is as a list or space.
func modEAPEnumerable(trm Term) Term {
	eap := trm.Dtype().plist.Find("enumerable")
	if eap == nil {
		return nil
	}
	atrb := eap.(*Symbol)
	if trm.Tag() == Symchain {
		xpr0 := trm.(*TermL)
		xpr0.args = append(xpr0.args, atrb)
		xpr0.dtype = atrb.dtype
	} else {
		trm = Simplify(makeTermL(Symchain, []Term{trm, atrb}, atrb.dtype, trm.First(), trm.Final()))
	}
	return trm
}

// helper for special case in backendFixups
func shouldOror(trm *TermTT) bool {
	a0t := trm.arg0.Dtype()
	a1t := trm.arg1.Dtype()
	return a0t.isNilposs() && a0t.mainType().family != TFTuple && !(a1t.isNilposs() || a1t.family == TFNil)
}

// Here's the recursion point for backendFixups. The bool retval is a changed indicator.
func (qci *QCInfo) backendFixups0(trm Term) (Term, bool) {
	var trm1 Term
	var ch, ch2 bool
	switch trm0 := trm.(type) {
	case *TermB, *Type:
		return trm, false
	case *Symbol:
		// only follow symbol bindings in their declarations
		pkginfo := trm0.plist.Find("pkgsym")
		if pkginfo != nil {
			qci.addImport(pkginfo.(*QIVal))
		}
		return trm, false
	case *Funinst:
		if trm0.body != nil {
			trm1, ch = qci.backendFixups0(trm0.body)
			trm1s := trm1.(*TermL)
			last := len(trm1s.args) - 1
			a := trm1s.args[last]
			atg := a.Tag()
			// Note: the exception for ifstmt is that ifexpr2go has rewritten if stmts except if they have returns on both branches
			if a.Dtype() != TypeNothing && a.Dtype() != TypeExit && atg != ReturnStmt && atg != IfStmt {
				trm1s.args[last] = makeTermT(ReturnStmt, a, a.Dtype(), Pos(-1), Pos(-1))
				ch = true
			}
			if ch {
				trm0.body = trm1.(*TermL)
			}
			return trm0, ch
		}
	case *TermT:
		if trm0.kind == Valdecl || trm0.kind == Vardecl {
			sym := trm0.arg0.(*Symbol)
			if sym.dtype != nil && sym.binding != nil {
				trm1, ch = qci.backendFixups0(sym.binding)
				if ch {
					sym.binding = trm1
				}
				return trm0, ch
			}
		} else if trm0.kind == Gdref {
			a0 := trm0.arg0
			if nilpossPtrzble(a0.Dtype()) {
				return a0, true
			}
		} else if trm0.arg0 != nil {
			trm0.arg0, ch = qci.backendFixups0(trm0.arg0)
		}
		trm1 = trm0
	case *TermTT:
		// turn X = [] into len(X) = 0
		if (trm0.kind == EqExpr || trm0.kind == EqeqExpr) && trm0.arg1.Tag() == Litform0 && len(trm0.arg1.(*TermL).args) == 0 {
			ch = true
			trm0.arg0, _ = qci.backendFixups0(trm0.arg0)
			trm0.arg0 = makeFuncall(biScope["__len"][0], []Term{trm0.arg0}, TypeInt, nil)
			trm0.arg1 = makeIntlit(0)
		} else if trm0.kind == EqExpr && shouldOror(trm0) {
			// comparing nilposs and not nilposs is allowed in d8m but not go, so turn this into || with nil check
			// note that this is quite likely suboptimal and might change completely at some point.
			nilcheck := makeTermTT(EqeqExpr, trm0.arg0, nilEntity, TypeBool, Pos(-1), Pos(-1))
			trm1 = makeTermL(OrorExpr, []Term{nilcheck, trm0}, TypeBool, Pos(-1), Pos(-1))
			return trm1, true
		} else {
			trm0.arg0, ch = qci.backendFixups0(trm0.arg0)
			trm0.arg1, ch2 = qci.backendFixups0(trm0.arg1)
			ch = ch || ch2
		}
		trm1 = trm0
	case *TermL:
		if trm0.kind == EachStmt { // here's the payoff
			trm1 = trm0 // default return
			iteree := trm0.args[1]
			tfam := iteree.Dtype().family
			if !(tfam == TFList || tfam == TFSpace || tfam == TFTypred) {
				// fixup needed
				ch = true
				a0 := modEAPEnumerable(iteree)
				if a0 == nil {
					panic("internal error: eachstart in backend")
				}
				trm0.args[1] = a0
			}
			trm0.args[2], ch2 = qci.backendFixups0(trm0.args[2])
			ch = ch || ch2
		} else {
			for i, a := range trm0.args {
				a, ch2 = qci.backendFixups0(a)
				trm0.args[i] = a
				ch = ch || ch2
			}
			trm1 = trm0
		}
	case *TermTL:
		trm0.trm, ch = qci.backendFixups0(trm0.trm)
		for i, a := range trm0.args {
			a, ch2 = qci.backendFixups0(a)
			trm0.args[i] = a
			ch = ch || ch2
		}
		trm1 = trm0
	}
	return trm1, ch
}

// Codegen starts here

func write1Import(pth, mname string, bldr *strings.Builder) {
	if mname != "" {
		bldr.WriteString(mname)
		bldr.WriteByte(' ')
	}
	bldr.WriteByte('"')
	bldr.WriteString(pth)
	bldr.WriteByte('"')
}

func (qci *QCInfo) writeImports(bldr *strings.Builder) {
	bldr.WriteString("package main\n\nimport ")
	var mname string
	if len(qci.imports) == 1 {
		if len(qci.importMnames) > 0 {
			mname = qci.importMnames[0][1]
		}
		write1Import(qci.imports[0], mname, bldr)
	} else {
		bldr.WriteString("(\n")
		for _, s := range qci.imports {
			mname = ""
			for _, pr := range qci.importMnames {
				if pr[0] == s {
					mname = pr[1]
					break
				}
			}
			write1Import(s, mname, bldr)
			bldr.WriteByte('\n')
		}
		bldr.WriteString(")\n")
	}
	bldr.WriteString("\n")
}

func (qci *QCInfo) gencode() string {
	qci.scopelevel = 0
	qci.precedences = qci.precedences[0:0]
	qci.pushCxt(0)
	var bldr strings.Builder
	qci.writeImports(&bldr)
	// Check for equivalences among these types and arrange to rename such cases when outputting.
	// The "typeEquiv" tag marks equivalences.
	for i, typsym := range qci.usedTypes {
		for j := i + 1; j < len(qci.usedTypes); j++ {
			otypsym := qci.usedTypes[j]
			if typsym.dtype.Equal(otypsym.dtype) && itemExistsNot(typsym.dtype, "ELT") {
				// tag otypsym's dtype so that it prints as the name of typsym.
				// Take care to check and mod the "body" type rather than a TFPtr decoration that may wrap it.
				// Otherwise, writeType can't figure it out.
				tnm := typsym.ident
				typtyp := typsym.dtype
				if typtyp.family == TFPtr {
					typtyp = typtyp.v.(*Type)
				}
				typeqv := typtyp.plist.Find("typeEquiv")
				if typeqv != nil {
					tnm = typeqv.(string)
				}
				otyptyp := otypsym.dtype
				if otyptyp.family == TFPtr {
					otyptyp = otyptyp.v.(*Type)
				}
				otyptyp.plist.Add("typeEquiv", tnm)
			}
		}
	}
	// now we're ready to gen code to define typsym.dtype
	for _, typsym := range qci.usedTypes {
		typ := typsym.dtype
		tnm := typsym.ident
		if typ.family == TFPtr {
			typ = typ.v.(*Type)
		}
		typeqv := typ.plist.Find("typeEquiv")
		// note: skip defn of already defined things
		if typeqv == nil {
			bldr.WriteString("type ")
			bldr.WriteString(tnm)
			bldr.WriteByte(' ')
			qci.writeType(typ, true, &bldr)
			bldr.WriteByte('\n')
		}
	}
	// we've got a separate list of Gomethods
	for _, mthdsym := range qci.gomethods {
		qci.gencodeGomethod(mthdsym, &bldr)
	}
	// take care of any gbls we alloc'd to generate strings for ELTs
	for _, infoE := range qci.smi.gblELTSlices {
		strgs := make([]string, len(infoE.values))
		for i, s := range infoE.values {
			strgs[i] = fmt.Sprintf(`"%s"`, s)
		}
		fmt.Fprint(&bldr, "var ", infoE.nm, " = []string{", strings.Join(strgs, ", "), "}\n")
	}

	var flaginit []*Symbol
	maincode := qci.externs[0].binding
	for _, sym := range qci.externs[1:] {
		if sym.ident == "" {
			continue
		}
		//fmt.Println("generating for ", sym.ident)
		if sym.binding.Tag() == FuninstTag {
			qci.gencodeFtn(sym, &bldr)
		} else if sym.binding.Tag() == Funcall && itemExists(sym.binding.(*TermTL).trm, "flagvar") {
			// here generate special code for a flag
			bldr.WriteString("var ")
			bldr.WriteString(sym.ident)
			bldr.WriteString(" ")
			qci.writeType(sym.dtype, false, &bldr)
			bldr.WriteByte('\n')
			flaginit = append(flaginit, sym)
		} else {
			qci.gencodeVar(sym, &bldr)
		}
	}
	// Finish special code for flags
	if len(flaginit) > 0 {
		bldr.WriteString("func init() {\n")
		for _, fit := range flaginit { // fit is the decld symbol
			fcall := fit.binding.(*TermTL)
			fsym := fcall.trm.(*Symbol)
			pkgsym := fsym.plist.Find("pkgsym")
			if pkgsym != nil {
				bldr.WriteString(pkgsym.(*QIVal).mname)
				bldr.WriteByte('.')
			}
			bldr.WriteString(fmt.Sprintf("%sVar(&%s", fsym.ident, fit.ident))
			for i := 0; i < 3; i++ {
				bldr.WriteString(", ")
				qci.gencode0(fcall.args[i], &bldr, false)
			}
			bldr.WriteString(")\n")
		}
		bldr.WriteString("}\n")
	}
	bldr.WriteString("func main() ")
	qci.gencode0(maincode, &bldr, false)
	return bldr.String()
}

// Called when we enter into any kind of nested "context" in codegen that might affect precedence calcs.
// I originally thought something also had to be done to record ptrzn status at these points but that seems
// not to be needed.
func (qci *QCInfo) pushCxt(prec int) {
	qci.precedences = append(qci.precedences, prec)
}

// Complement to pushCxt, this is called when we exit the context.
func (qci *QCInfo) popCxt() {
	stacklen := len(qci.precedences) - 1
	qci.precedences = qci.precedences[:stacklen]
}

// Generate the prec info of trm and compare it with the prec on top of precedences stack, returning true if should
// insert parens.
func (qci *QCInfo) parenthesize(trm Term) bool {
	tos := len(qci.precedences) - 1
	tg := trm.Tag()
	prec := 0
	if tg == EqExpr || tg == EqeqExpr {
		prec = 3
	} else if tg == AndandExpr {
		prec = 4
	} else if tg == OrorExpr {
		prec = 5
	}
	//fmt.Print("pz, tg=", tg, ",prec=", prec, "rslt=", prec < qci.precedences[tos], "; ")
	return prec > qci.precedences[tos]
}

func isCompareOperator(ftnname string) bool {
	return ftnname == "<" || ftnname == "<=" || ftnname == ">" || ftnname == ">="
}

// Should only be called on funcall terms; figures out if it's a go operator and returns the prec if so, else -1
func isOperator(fcall *TermTL) (int, string) {
	prec := -1
	retyp := fcall.dtype
	// symchain or other ftn designator can't be operator
	if fcall.trm.Tag() != SymbolTag {
		return -1, ""
	}
	fsym := fcall.trm.(*Symbol)
	ftnname := fsym.ident
	if ftnname == "+" || ftnname == "-" {
		if retyp == TypeInt || retyp == TypeFloat || retyp == TypeString || retyp == TypeUint8 || retyp == TypeByte {
			prec = 2
		}
	} else if ftnname == "*" || ftnname == "/" || ftnname == "%" {
		if retyp == TypeInt || retyp == TypeFloat {
			prec = 1
		}
	} else if isCompareOperator(ftnname) {
		a0t := fcall.args[0].Dtype()
		if a0t.family == TFInt || a0t.family == TFFloat || a0t.family == TFString || a0t.family == TFByte {
			prec = 3
		}
	} else if fsym.plist.Find("Operator") != nil {
		oprtbl := []struct {
			nm   string
			prec int
			fnnm string
		}{
			{"and", 1, "&"},
			{"or", 2, "|"},
			{"lshift", 1, "<<"},
			{"rshift", 1, ">>"},
			{"xor", 1, "^"},
			{"andnot", 1, "andnot"},
			{"bitnum", 1, "bitnum"},
		}
		for _, oprinfo := range oprtbl {
			if ftnname == oprinfo.nm {
				prec = oprinfo.prec
				ftnname = oprinfo.fnnm
				break
			}
		}
	}
	return prec, ftnname
}

// Write a type "from scratch"
func (qci *QCInfo) writeType(typ *Type, defn bool, bldr *strings.Builder) {
	qci.seentypes = qci.seentypes[0:0]
	if defn && typ.family == TFPtr {
		typ = typ.v.(*Type)
	}
	qci.writeType0(typ, defn, bldr)
}

// Write a generic type (family Typred)
func (qci *QCInfo) writeGncType(typred *Typred, bldr *strings.Builder) {
	if typred.ident == "entity" || typred.ident == "list" || typred.ident == "tuple" {
		bldr.WriteString("interface{}")
	} else {
		if typred.attribset != 0 {
			panic("codegen for attribute affordances is not implemented")
		}
		bldr.WriteString("interface{\n")
		for _, mthd := range typred.affs {
			bldr.WriteString(mthd.ident)
			qci.writeFtnSignature0(mthd.dtype.v.(*Ftntype), bldr)
			bldr.WriteString("\n")
		}
		bldr.WriteString("}")
	}
}

func (qci *QCInfo) writeFtnSignature0(fntyp *Ftntype, bldr *strings.Builder) {
	bldr.WriteByte('(')
	last := len(fntyp.fmlargs) - 1
	for i, a := range fntyp.fmlargs {
		qci.writeType0(a, false, bldr)
		if i != last {
			bldr.WriteString(", ")
		}
	}
	bldr.WriteByte(')')
	if fntyp.rettype != TypeNothing {
		bldr.WriteByte(' ')
		qci.writeType0(fntyp.rettype, false, bldr)
	}
}

// Recursion point for writeType; qci.seentypes builds up so we catch recursive types
func (qci *QCInfo) writeType0(typ *Type, defn bool, bldr *strings.Builder) {
	if typ.family == TFPtr {
		if itemExistsNot(typ, "interfaceType") {
			bldr.WriteByte('*')
		}
		typ = typ.v.(*Type)
	}
	if !defn {
		pkgt := typ.plist.Find("pkgtype")
		if pkgt != nil {
			qi := ""
			pkgsym := typ.plist.Find("pkgsym")
			if pkgsym != nil {
				qi = pkgsym.(*QIVal).mname
			}
			if qi != "" {
				bldr.WriteString(qi)
				bldr.WriteByte('.')
			}
			bldr.WriteString(pkgt.(string))
			return
		}
		tnm := typ.plist.Find("typeEquiv")
		if tnm == nil {
			tnm = typ.plist.Find("typename")
		}
		if tnm != nil {
			bldr.WriteString(tnm.(string))
			return
		}
	}
	for i := 0; i < len(gotypeFamilyStrings); i++ {
		if typ.family == gotypeFamilyStrings[i].fam {
			bldr.WriteString(gotypeFamilyStrings[i].ident)
			return
		}
	}
	switch typ.family {
	case TFNothing:
		return
	case TFList:
		qci.seentypes = append(qci.seentypes, typ)
		bldr.WriteString("[]")
		qci.writeType0(typ.v.(*Type), false, bldr)
	case TFSpace:
		qci.seentypes = append(qci.seentypes, typ)
		spctyp := typ.v.(*Spacetype)
		// space(bit,N) gets special treatment
		if spctyp.elttype == TypeBit {
			if Manifest(spctyp.dimensions) {
				dim := spctyp.dimensions.Plist().Find("intval").(int)
				if dim <= 64 {
					bldr.WriteString("uint64")
					return
				}
				panic("unwritten")
			}
		} else {
			bldr.WriteByte('[')
			qci.gencode0(spctyp.dimensions, bldr, false)
			bldr.WriteByte(']')
			qci.writeType0(spctyp.elttype, false, bldr)
		}
	case TFTuple:
		bldr.WriteString("struct {")
		tuptyp := typ.v.(*Tupletype)
		last := len(tuptyp.attribs) - 1
		for i, a := range tuptyp.attribs {
			bldr.WriteString(a.ident)
			bldr.WriteByte(' ')
			qci.writeType0(a.dtype, false, bldr)
			if i != last {
				bldr.WriteString("; ")
			}
		}
		bldr.WriteByte('}')
	case TFOrtype:
		bldr.WriteString("interface{}")
	case TFTypred:
		qci.writeGncType(typ.v.(*Typred), bldr)
	case TFFtn:
		bldr.WriteString("func")
		qci.writeFtnSignature0(typ.v.(*Ftntype), bldr)
	case TFGomap:
		bldr.WriteString("map[")
		kvtypes := typ.v.([]*Type)
		qci.writeType0(kvtypes[0], false, bldr)
		bldr.WriteByte(']')
		qci.writeType0(kvtypes[1], false, bldr)
	case TFGochan:
		chtyp := typ.v.(*ChanInfo)
		chanpart := ""
		if chtyp.dir == "both" {
			chanpart = "chan "
		} else if chtyp.dir == "send" {
			chanpart = "<- chan "
		} else {
			chanpart = "chan <- "
		}
		bldr.WriteString(chanpart)
		qci.writeType0(chtyp.elttype, false, bldr)
	default:
		panic("internal error: this type should not be presented here")
	}
}

func (qci *QCInfo) writeSymdecl(sym *Symbol, bldr *strings.Builder) {
	bldr.WriteString(sym.ident)
	bldr.WriteByte(' ')
	qci.writeType(sym.dtype, false, bldr)
}

func (qci *QCInfo) writeFtnSignature(funi *Funinst, asmthd bool, bldr *strings.Builder) {
	bldr.WriteByte('(')
	last := len(funi.funargs) - 1
	for i, argsym := range funi.funargs {
		if asmthd && i == 0 {
			continue
		}
		qci.writeSymdecl(argsym, bldr)
		if i != last {
			bldr.WriteString(", ")
		}
	}
	bldr.WriteByte(')')
	rettyp := funi.dtype.v.(*Ftntype).rettype
	if rettyp != TypeNothing {
		bldr.WriteByte(' ')
		qci.writeType(rettyp, false, bldr)
		bldr.WriteByte(' ')
	}
}

// Mostly, this is utterly straightforward. Need the newline arg to work around go's rule that right curly before
// newline inserts a semicolon.
func (qci *QCInfo) writeStmts(stmts *TermL, newline bool, bldr *strings.Builder) {
	qci.scopelevel += 1
	bldr.WriteString("{\n")
	for _, s := range stmts.args {
		qci.gencode0(s, bldr, false)
		bldr.WriteByte('\n')
	}
	bldr.WriteByte('}')
	if newline {
		bldr.WriteByte('\n')
	}
	qci.scopelevel -= 1
}

func (qci *QCInfo) gencodeFtn(fnsym *Symbol, bldr *strings.Builder) {
	funi := fnsym.binding.(*Funinst)
	bldr.WriteString("func ")
	bldr.WriteString(fnsym.ident)
	qci.writeFtnSignature(funi, false, bldr)
	qci.writeStmts(funi.body, true, bldr)
}

// Put out go text to generate a "zero value" of any type.
func (qci *QCInfo) zvGen(typ *Type, bldr *strings.Builder, seen *[]*Type, inDecl bool) {
	otyp := typ
	if typ.family == TFPtr {
		typ = typ.v.(*Type)
	}
	if typ.family == TFTuple || typ.family == TFGomap {
		if inDecl {
			bldr.WriteString("= ")
		}
		if itemExists(typ, "interfaceType") {
			bldr.WriteString("nil")
		} else if typ.family == TFTuple {
			// we need to avoid calls on mutually recursive types
			for i := 0; seen != nil && i < len(*seen); i++ {
				if typ == (*seen)[i] {
					bldr.WriteByte('&')
					qci.writeType(typ, false, bldr)
					bldr.WriteByte('{')
					bldr.WriteByte('}')
					return
				}
			}
			*seen = append(*seen, typ)
			if otyp.family == TFPtr {
				bldr.WriteByte('&')
			}
			qci.writeType(typ, false, bldr)
			bldr.WriteByte('{')
			needsep := false
			for _, a := range typ.v.(*Tupletype).attribs {
				if a.dtype.family == TFPtr && itemExistsNot(a.dtype, "wasNilposs") {
					if needsep {
						bldr.WriteString(", ")
					}
					bldr.WriteString(a.ident)
					bldr.WriteByte(':')
					qci.zvGen(a.dtype, bldr, seen, false)
					needsep = true
				}
			}
			bldr.WriteByte('}')
		} else {
			// very hackish handling of Gomap but will do for a first pass
			bldr.WriteString(" make(")
			qci.writeType(typ, false, bldr)
			bldr.WriteString(", 10)")
		}
	}
}

// Test for empty list, sometimes this can codegen as "nil"
func emptyListLit(trm Term) bool {
	return trm.Tag() == Litform0 && len(trm.(*TermL).args) == 0
}

func (qci *QCInfo) gencodeVar(sym *Symbol, bldr *strings.Builder) {
	bldr.WriteString("var ")
	bldr.WriteString(sym.ident)
	bldr.WriteByte(' ')
	if sym.binding == nil || sym.binding.Tag() == ZeroValue {
		qci.writeType(sym.dtype, false, bldr)
		seen := []*Type{}
		qci.zvGen(sym.dtype, bldr, &seen, true)
	} else {
		if sym.dtype.family == TFPtr && symbolNamed(sym.binding, "nil") {
			qci.writeType(sym.dtype, false, bldr)
			bldr.WriteByte(' ')
		}
		// clever (perhaps overly clever) way to to put out the decln then the defn as an asgnmt
		if itemExists(sym, "recursive") {
			qci.writeType(sym.dtype, false, bldr)
			bldr.WriteString("\n")
			bldr.WriteString(sym.ident)
		}
		bldr.WriteString("= ")
		if emptyListLit(sym.binding) {
			qci.gencodeLitform(sym.binding.(*TermL), bldr)
		} else {
			// The following is somewhat hackish: cvt ELTs but not other types
			wrapcvt := sym.dtype.family == TFInt && itemExists(sym.dtype, "typename")
			if wrapcvt {
				tnm := sym.dtype.plist.Find("typename").(string)
				bldr.WriteString(tnm)
				bldr.WriteByte('(')
			}
			qci.gencode0(sym.binding, bldr, sym.dtype.family != TFPtr && sym.binding.Dtype().family == TFPtr)
			if wrapcvt {
				bldr.WriteByte(')')
			}
		}
	}
	bldr.WriteByte('\n')
}

// Handle BI copy function
func (qci *QCInfo) gencodeCopy(trm0 Term, bldr *strings.Builder) {
	typ := trm0.Dtype()
	if typ.family == TFPtr {
		bldr.WriteByte('&')
		typ = typ.v.(*Type)
	}
	switch typ.family {
	case TFTuple:
		tupt := typ.v.(*Tupletype)
		cpargs := make([]Term, len(tupt.attribs))
		for i, a := range tupt.attribs {
			cpargs[i] = makeTermL(Symchain, []Term{trm0, a}, a.dtype, Pos(-1), Pos(-1))
		}
		lf := makeTermL(Litform0, cpargs, typ, Pos(-1), Pos(-1))
		qci.gencodeLitform(lf, bldr)
	default:
		panic("copy case not written")
	}
}

func (qci *QCInfo) gencodeLitform(trm0 *TermL, bldr *strings.Builder) {
	typ := trm0.dtype
	if typ == TypeNothing {
		// this is a workaround for a case where a litform isn't wanted. I haven't yet been
		// successful killing this at the source (ie smashDType) so I'm doing it here.
		return
	} else if itemExists(typ, "interfaceType") && len(trm0.args) == 0 {
		bldr.WriteString("nil")
		return
	}
	if typ.family == TFPtr {
		bldr.WriteByte('&')
		typ = typ.v.(*Type)
	}
	qci.writeType(typ, false, bldr)
	bldr.WriteByte('{')
	qci.pushCxt(0)
	last := len(trm0.args) - 1
	for i, a := range trm0.args {
		if a == nil {
			bldr.WriteString("nil")
		} else {
			qci.gencode0(a, bldr, false)
		}
		if i < last {
			bldr.WriteString(", ")
		}
	}
	qci.popCxt()
	bldr.WriteByte('}')
}

func (qci *QCInfo) gencodeGomethod(fnsym *Symbol, bldr *strings.Builder) {
	bldr.WriteString("func (")
	funi := fnsym.binding.(*Funinst)
	qci.writeSymdecl(funi.funargs[0], bldr)
	bldr.WriteString(") ")
	bldr.WriteString(fnsym.ident)
	qci.writeFtnSignature(funi, true, bldr)
	qci.writeStmts(funi.body, true, bldr)
}

// Generate code for trm with possible cast to a pkg type as guided by plistSrc.
// Return true if wrapping so caller can add closing paren.
func (qci *QCInfo) wrapPkgType(trm, plistSrc Term, bldr *strings.Builder) (retval bool) {
	pkgt := plistSrc.Plist().Find("pkgtype")
	if pkgt != nil {
		// skip the wrapping if pkgtypes of trm and plistSrc are equal because we infer that no type cnvn is needed in this case.
		// Also if plistSrc is marked as interfaceType since you don't cvt to those.
		if trm.Dtype().plist.Find("pkgtype") != pkgt && itemExistsNot(plistSrc, "interfaceType") {
			retval = true
			pkgtype := pkgt.(string)
			pkgqi := plistSrc.Plist().Find("pkgsym").(*QIVal)
			if pkgqi.mname != "" {
				bldr.WriteString(pkgqi.mname)
				bldr.WriteByte('.')
			}
			bldr.WriteString(pkgtype)
			bldr.WriteByte('(')
		}
	}
	return
}

// Handle terms in funcalls that have the "noPointer" annotation. For now, fail on Symbols
func dePtrzTerm(trm Term) Term {
	if trm.Dtype().family == TFPtr {
		if trm.Tag() == SymbolTag {
			panic("unwritten")
		} else {
			smashTo := trm.Dtype().v.(*Type) // get the unptrzd type
			smashDtype(trm, smashTo)
		}
	}
	return trm
}

// Write x(y,z,...) for simple cases.
// The fmlargs param guides conversion to pkgtypes if needed.
func (qci *QCInfo) writeFuncallForm(ftnname string, fcall *TermTL, fmlargs []*Type, bldr *strings.Builder) {
	last := len(fcall.args) - 1
	ptrzIndex := len(fcall.args) + 1 // default initzn ensures it's unused
	ptrz := fcall.trm.Plist().Find("Pointerize")
	if ptrz != nil {
		ptrzIndex = ptrz.(int)
	}
	bldr.WriteString(ftnname)
	bldr.WriteByte('(')
	for i, a := range fcall.args {
		pkgtype := false
		dptrz := false
		// Squirrely bit alert: the len check is to try to avoid varargs ftns, probably not very robust.
		// This generally squirrely code is there to handle pkgs with funny conventions, like gonum.org/v1/plot/...
		if len(fmlargs) == len(fcall.args) {
			pkgtype = qci.wrapPkgType(a, fmlargs[i], bldr)
			dptrz = fmlargs[i].plist.Find("noPointer") != nil
		}
		if dptrz {
			a = dePtrzTerm(a)
		} else if i >= ptrzIndex {
			bldr.WriteByte('&')
		}
		qci.gencode0(a, bldr, false)
		if pkgtype {
			bldr.WriteByte(')')
		}
		if i < last {
			bldr.WriteString(", ")
		}
	}
	bldr.WriteByte(')')
}

// This version is for cases that pointerize the rcvr
// I haven't yet added the fmlargs param but it may be needed.
func (qci *QCInfo) writeFuncallFormPR(ftnname string, args []Term, bldr *strings.Builder) {
	last := len(args) - 1
	bldr.WriteString(ftnname)
	bldr.WriteString("(&")
	for i, a := range args {
		qci.gencode0(a, bldr, false)
		if i < last {
			bldr.WriteString(", ")
		}
	}
	bldr.WriteByte(')')
}

// Strings are tricky because we prefer raw strings but these cannot have backquotes and some quoted chars don't work well.
// So we scan the contents of the string for backquote, newline, cr; and use dblquote string if we find any.
func genString(strg string, bldr *strings.Builder) {
	escapees := []byte{'`', '\n', '\r', '`'}
	replacements := []string{`\"`, `\n`, `\r`, `\\`}
	indices := make([]int, len(escapees))
	xcount := 0
	for i, b := range escapees[:3] {
		indices[i] = strings.IndexByte(strg, b)
		if indices[i] >= 0 {
			xcount += 1
		}
	}
	if xcount == 0 {
		bldr.WriteByte('`')
		bldr.WriteString(strg)
		bldr.WriteByte('`')
	} else {
		// once the dblquote decision is made we no longer care about '`' but we do care about dblquote and backslash
		bldr.WriteByte('"')
		escapees[0] = '"'
		indices[0] = strings.IndexByte(strg, '"')
		escapees[3] = '\\'
		indices[3] = strings.IndexByte(strg, '\\')
		offset := 0
		var findMin = func() int {
			rslt := -1
			curmin := len(strg)
			for i := 0; i < 4; i++ {
				if indices[i] >= 0 && indices[i] < curmin {
					rslt = i
					curmin = indices[i]
				}
			}
			return rslt
		}
		for {
			m := findMin()
			if m < 0 {
				break
			}
			minx := indices[m]
			bldr.WriteString(strg[offset:minx])
			bldr.WriteString(replacements[m])
			offset = minx + 1
			nxtofs := strings.IndexByte(strg[offset:], escapees[m])
			if nxtofs < 0 {
				indices[m] = -1
			} else {
				indices[m] = offset + nxtofs
			}
		}
		bldr.WriteString(strg[offset:])
		bldr.WriteByte('"')
	}
}

// Ultimately, we should be able to translate enumeration, plus all list methods, to reflect-based code.
// One simplification is that many list methods will have been inlined and simplified. Since
// we're in codegen here, we don't translate at the level of list methods but in terms of their
// implementations. For now, I'm doing a grossly simplified version and punting on the rest.
// Specifically, we translate each and __len. When called, valIdent is the ValueOf the subject term.
// We generate some terms to subst into clz, the gencode0 that.
func (qci *QCInfo) gencodeGncList(valIdent string, subject Term, clz Term, bldr *strings.Builder) {
	// I should use matching or similar to find a relevant each stmt anywhere but for now, I'll just
	// look in the stmtlist of the clause.
	eachinx := -1
	if clz.Tag() == Stmts {
		for i, s := range clz.(*TermL).args {
			if s.Tag() == EachStmt && s.(*TermL).args[1].Equal(subject) {
				eachinx = i
				break
			}
		}
	}
	if eachinx >= 0 {
		bldr.WriteString("xxxxxx")
	}
}

func (qci *QCInfo) genFuncall(trm0 *TermTL, bldr *strings.Builder) {
	if trm0.trm.Tag() == FuninstTag {
		// got a iaaf here, can only be one generated for initzg gbls
		bldr.WriteString("func() ")
		qci.writeType(trm0.dtype, false, bldr)
		qci.writeStmts(trm0.trm.(*Funinst).body, false, bldr)
		bldr.WriteString("()\n")
		return
	}
	ftnsym, isSym := trm0.trm.(*Symbol)
	fmlargs := trm0.trm.Dtype().v.(*Ftntype).fmlargs
	isOpr, ftnname := isOperator(trm0)
	if isOpr >= 0 {
		cxtprec := qci.precedences[len(qci.precedences)-1]
		//fmt.Print("isopr=", isOpr, "cxtprec=", cxtprec)
		paren := cxtprec <= isOpr
		if paren {
			bldr.WriteByte('(')
		}
		qci.pushCxt(isOpr)
		if ftnname == "bitnum" {
			bldr.WriteString("1 << ")
			qci.gencode0(trm0.args[0], bldr, true)
		} else if ftnname == "andnot" {
			qci.gencode0(trm0.args[0], bldr, true)
			bldr.WriteString(" & ~")
			rhstg := trm0.args[1].Tag()
			rparen := !(rhstg == SymbolTag || rhstg == Symchain || rhstg == Intlit)
			if rparen {
				bldr.WriteByte('(')
			}
			qci.gencode0(trm0.args[1], bldr, true)
			if rparen {
				bldr.WriteByte(')')
			}
		} else {
			if len(trm0.args) != 2 {
				panic("impossible")
			}
			qci.gencode0(trm0.args[0], bldr, true)
			bldr.WriteByte(' ')
			bldr.WriteString(ftnname)
			bldr.WriteByte(' ')
			qci.gencode0(trm0.args[1], bldr, true)
		}
		qci.popCxt()
		if paren {
			bldr.WriteByte(')')
		}
	} else if !isSym {
		qci.gencode0(trm0.trm, bldr, false)
		bldr.WriteByte('(')
		last := len(trm0.args) - 1
		for i, a := range trm0.args {
			qci.gencode0(a, bldr, false)
			if i != last {
				bldr.WriteString(", ")
			}
		}
		bldr.WriteByte(')')
	} else if ftnsym.binding == nil {
		pkginfo := ftnsym.plist.Find("pkgsym")
		ftnname = ftnsym.ident
		if pkginfo != nil {
			qi := pkginfo.(*QIVal).mname
			bldr.WriteString(qi)
			bldr.WriteByte('.')
			if qi == "fmt" {
				if ftnname == "println" {
					ftnname = "Println"
				} else if ftnname == "print" {
					ftnname = "Print"
				} else if ftnname[0] == '_' {
					ftnname = "Sprintf"
				}
			} else if ftnsym.plist.Find("applyargs") != nil {
				// This is the one where we need to ... the last arg since the pkg ftn takes a vbl list of
				// args and d8m wants to rep that as a list.
				// So far, I haven't found a better soln than to reproduce most of writeFuncallForm here
				last := len(trm0.args) - 1
				bldr.WriteString(ftnname)
				bldr.WriteByte('(')
				for i, a := range trm0.args {
					if i < last {
						qci.gencode0(a, bldr, false)
						bldr.WriteString(", ")
					} else if a.Tag() == Litform0 {
						a0 := a.(*TermL)
						last2 := len(a0.args) - 1
						for j, aa := range a0.args {
							qci.gencode0(aa, bldr, false)
							if j < last2 {
								bldr.WriteString(", ")
							}
						}
					} else {
						qci.gencode0(a, bldr, false)
						bldr.WriteString("...")
					}
				}
				bldr.WriteByte(')')
				return
			}
			qci.writeFuncallForm(ftnname, trm0, fmlargs, bldr)
			return
		} else if ftnsym.plist.Find("Gomethod") != nil {
			// hack alert: the pkg import stuff doesn't represent the rcvr of a go method explicitly so there's
			// noplace to put a "noPointer" item when the rcvr is a non-ptr tuple. Instead, I'm putting it into the
			// ftn symbol itself.
			if ftnsym.plist.Find("noPointer") != nil {
				trm0.args[0] = dePtrzTerm(trm0.args[0])
			}
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte('.')
			bldr.WriteString(ftnsym.ident)
			bldr.WriteByte('(')
			lastinx := len(trm0.args) - 2
			for i, a := range trm0.args[1:] {
				// varany Gomethods (log pkg) don't have fmlargs
				if len(fmlargs) > i+1 {
					if fmlargs[i+1].plist.Find("noPointer") != nil {
						a = dePtrzTerm(a)
					}
					pkgtype := qci.wrapPkgType(a, fmlargs[i+1], bldr)
					qci.gencode0(a, bldr, false)
					if pkgtype {
						bldr.WriteByte(')')
					}
				} else {
					qci.gencode0(a, bldr, false)
				}
				if i < lastinx {
					bldr.WriteString(", ")
				}
			}
			bldr.WriteByte(')')
			return
		}
		switch ftnname {
		default:
			if ftnsym.binding != nil && itemExists(ftnsym.binding, "biFtn") {
				panic("unhandled BI ftn")
			} else {
				qci.writeFuncallForm(ftnname, trm0, fmlargs, bldr)
			}
		case "copy":
			qci.gencodeCopy(trm0.args[0], bldr)
		case "cvt", "to_s":
			// this is probably not where cvt to string should be long term
			cvtT := "string"
			tynm := trm0.dtype.plist.Find("typename")
			if tynm != nil {
				cvtT = tynm.(string)
			}
			bldr.WriteString(cvtT)
			bldr.WriteByte('(')
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte(')')
		case "__append", "__appendDDD":
			bldr.WriteString("append(")
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteString(", ")
			qci.gencode0(trm0.args[1], bldr, false)
			if ftnname == "__append" {
				bldr.WriteByte(')')
			} else {
				bldr.WriteString("...)")
			}
		case "__make":
			bldr.WriteString("make(")
			qci.writeType(trm0.dtype, false, bldr)
			bldr.WriteString(", ")
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte(')')
		case "__cvtf":
			bldr.WriteString("int(")
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte(')')
		case "count":
			rcvrfam := ftnsym.dtype.v.(*Ftntype).fmlargs[0].family
			if rcvrfam != TFGomap {
				panic("internal error")
			}
			qci.writeFuncallForm("len", trm0, fmlargs, bldr)
		case "__len":
			qci.writeFuncallForm("len", trm0, fmlargs, bldr)
		case "__copy":
			qci.writeFuncallForm("copy", trm0, fmlargs, bldr)
		case "__slicex":
			lst := trm0.args[0]
			if isPtr2List(lst) {
				bldr.WriteByte('(')
			}
			qci.gencode0(lst, bldr, false)
			if isPtr2List(lst) {
				bldr.WriteByte(')')
			}
			bldr.WriteByte('[')
			qci.gencode0(trm0.args[1], bldr, false)
			bldr.WriteString(" : ")
			qci.gencode0(trm0.args[2], bldr, false)
			bldr.WriteByte(']')
		case "__cap":
			qci.writeFuncallForm("cap", trm0, fmlargs, bldr)
		case "__singletonSliceLit":
			qci.writeType(trm0.dtype, false, bldr)
			bldr.WriteByte('{')
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte('}')
		case "rvalindexWD":
			// note that if the default value isn't manifest and equal to that of the result type,
			// this code was rewritten, so at this point, we assume it.
			// Note that the following is identical to code for rvalindex, so maybe we should rewrite to that.
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte('[')
			// The following is a horrible hack, showing among other things that gencode0.shouldDeref arg doesn't work.
			// The deal is that Gomap keys that are tuples are never pointers, so we want to deref here.
			if trm0.args[1].Dtype().family == TFPtr {
				bldr.WriteByte('*')
			}
			qci.gencode0(trm0.args[1], bldr, true)
			bldr.WriteByte(']')
		case "print", "println", "__sprintf":
			// fudge the pkg stuff
			fn0 := "fmt.Println"
			if ftnname == "print" {
				fn0 = "fmt.Print"
			} else if ftnname[0] == '_' {
				fn0 = "fmt.Sprintf"
			}
			qci.writeFuncallForm(fn0, trm0, fmlargs, bldr)
		case "zerolist":
			bldr.WriteString("make(")
			qci.writeType(trm0.dtype, false, bldr)
			bldr.WriteString(", ")
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte(')')
		case "pow":
			if trm0.args[0].Tag() == Intlit && trm0.args[0].(*TermB).value == "2" {
				bldr.WriteString("1 << ")
				qci.gencode0(trm0.args[1], bldr, false)
			} else {
				panic("unwritten")
			}
		case "channelMake":
			bldr.WriteString("make(")
			qci.writeType0(trm0.args[0].(*Type), false, bldr)
			buffsize := trm0.args[1].(*TermB).value
			if buffsize != "0" {
				bldr.WriteString(", ")
				bldr.WriteString(buffsize)
			}
			bldr.WriteByte(')')
		case "channelSend":
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteString(" <- ")
			qci.gencode0(trm0.args[1], bldr, false)
		case "channelReceive":
			bldr.WriteString(" <- ")
			qci.gencode0(trm0.args[0], bldr, false)
		case "Gocall":
			bldr.WriteString("go ")
			qci.gencode0(trm0.args[0], bldr, false)
		}
	} else if ftnname == "rvalindex" || ftnname == "lvalindex" {
		lst := trm0.args[0]
		if isPtr2List(lst) {
			bldr.WriteByte('(')
		}
		qci.gencode0(lst, bldr, false)
		if isPtr2List(lst) {
			bldr.WriteByte(')')
		}
		bldr.WriteByte('[')
		//if isNPviaPtr(trm0.args[1]) {
		//	bldr.WriteByte('*')
		//}
		qci.gencode0(trm0.args[1], bldr, true)
		bldr.WriteByte(']')
		if ftnname == "lvalindex" {
			bldr.WriteString(" = ")
			qci.gencode0(trm0.args[2], bldr, false)
			bldr.WriteByte('\n')
		}
	} else if ftnname == "autocvt" || ftnname == "cvt" {
		rettype := ftnsym.dtype.v.(*Ftntype).rettype
		// hack alert! Convert go's error type to string
		argt := trm0.args[0].Dtype()
		if rettype == TypeString && (argt == TypeGoerror || argt == TypeGoerrorBase) {
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteString(".Error()")
		} else if trm0.args[0].Dtype().plist.Find("infoELT") != nil {
			infoE := trm0.args[0].Dtype().plist.Find("infoELT").(string)
			bldr.WriteString(infoE)
			bldr.WriteByte('[')
			qci.gencode0(trm0.args[0], bldr, false)
			bldr.WriteByte(']')
		} else {
			// insert go cnvn here but first check for cases where it's really not needed
			a0tg := trm0.args[0].Tag()
			rtf := rettype.family
			if (rtf == TFString && a0tg == Stringlit) || (rtf == TFLabel && a0tg == Labellit) || (rtf == TFInt && a0tg == Intlit) {
				qci.gencode0(trm0.args[0], bldr, false)
			} else {
				qci.writeType0(rettype, false, bldr)
				bldr.WriteByte('(')
				qci.gencode0(trm0.args[0], bldr, true)
				bldr.WriteByte(')')
			}
		}
	} else if ftnname == "to_f" {
		bldr.WriteString("float64(")
		qci.gencode0(trm0.args[0], bldr, true)
		bldr.WriteByte(')')
	} else {
		if ftnname == "exit" {
			ftnname = "panic"
		}
		if itemExists(ftnsym.binding, "asgnftn") {
			qci.writeFuncallFormPR(ftnname, trm0.args, bldr)
		} else {
			qci.writeFuncallForm(ftnname, trm0, fmlargs, bldr)
		}
	}
}

// This is the recursion point for codegen, it works by modding bldr.
func (qci *QCInfo) gencode0(trm Term, bldr *strings.Builder, shouldDeref bool) {
	// This local helper is only for equality comparison but all variants incl !=, !==, etc.
	var compareop = func(a0, a1 Term, opstrg string, op Termtag) {
		cxtprec := qci.precedences[len(qci.precedences)-1]
		// >= 3 captures that compare ops are non-assoc
		if cxtprec >= 3 {
			bldr.WriteByte('(')
		}
		a0t := a0.Dtype()
		qci.pushCxt(3)
		// derefTup is specially crafted for == and !=. A nil compare doesn't deref but anything else derefs both
		derefTup := op == EqExpr && a0t.family == TFPtr
		// maybe should clean up this way of identifying nil; it's a copy made in visit, could special case that.
		if derefTup && symbolNamed(a1, "nil") {
			derefTup = false
		}
		if derefTup {
			bldr.WriteByte('*')
			if a0.Tag() != SymbolTag {
				bldr.WriteByte('(')
			}
			qci.gencode0(a0, bldr, false)
			if a0.Tag() != SymbolTag {
				bldr.WriteByte(')')
			}
		} else {
			qci.gencode0(a0, bldr, !(isNPviaPtr(a0) && symbolNamed(a1, "nil")))
		}
		bldr.WriteByte(' ')
		bldr.WriteString(opstrg)
		bldr.WriteByte(' ')
		// disable derefTup if the rhs is not a pointer
		if derefTup && a1.Dtype().family != TFPtr {
			derefTup = false
		}
		if derefTup {
			bldr.WriteByte('*')
			if a1.Tag() != SymbolTag {
				bldr.WriteByte('(')
			}
			qci.gencode0(a1, bldr, false)
			if a1.Tag() != SymbolTag {
				bldr.WriteByte(')')
			}
		} else {
			if a0t.family == TFInt && a1.Tag() == Labellit {
				// special case ELT code mapping
				inx := indexELT(a0t, a1.(*TermB).value)
				bldr.WriteString(fmt.Sprintf("%d", inx))
			} else {
				qci.gencode0(a1, bldr, true)
			}
		}
		qci.popCxt()
		if cxtprec >= 3 {
			bldr.WriteByte(')')
		}
	}
	if isNPviaPtr(trm) && shouldDeref {
		bldr.WriteString("(*")
		qci.gencode0(trm, bldr, false)
		bldr.WriteByte(')')
		return
	}
	//fmt.Print("CG tg=", trm.Tag(), "; ")
	switch trm0 := trm.(type) {
	case *Symbol:
		if isPtr2List(trm0) {
			bldr.WriteByte('*')
		}
		pkgsym := trm0.plist.Find("pkgsym")
		if pkgsym != nil {
			bldr.WriteString(pkgsym.(*QIVal).mname)
			bldr.WriteByte('.')
		}
		bldr.WriteString(trm0.ident)
	case *Funinst:
		bldr.WriteString("func ")
		qci.writeFtnSignature(trm0, false, bldr)
		qci.writeStmts(trm0.body, false, bldr)
	case *TermB:
		pkgwrap := qci.wrapPkgType(trm0, trm0, bldr)
		tg := trm0.kind
		if tg == Stringlit {
			genString(trm0.value, bldr)
		} else if tg == Labellit { // gen labels as strings, always raw
			lt := trm0.plist.Find("labeltype")
			if lt != nil {
				lt0 := lt.(*Type) // will be a smallint type
				eltstrgs := lt0.plist.Find("ELT")
				if eltstrgs != nil {
					inx := stringInStrings(trm0.value, eltstrgs.([]string))
					if inx < 0 {
						panic("impossible")
					}
					fmt.Fprint(bldr, inx)
					// pkgwrap must be false here, so this is ok
					return
				}
			}
			bldr.WriteByte('`')
			bldr.WriteString(trm0.value)
			bldr.WriteByte('`')
		} else if tg == Intlit || tg == Floatlit || tg == Boollit {
			bldr.WriteString(trm0.value)
		} else if tg == Bytelit {
			bldr.WriteByte('\'')
			bval := trm0.value[0]
			if bval == '\n' {
				bldr.WriteString(`\n`)
			} else if bval == '\t' {
				bldr.WriteString(`\t`)
			} else if bval == '\\' {
				bldr.WriteString(`\\`)
			} else if bval == '\r' {
				bldr.WriteString(`\r`)
			} else if bval == '\'' {
				bldr.WriteString(`\'`)
			} else {
				bldr.WriteByte(trm0.value[0])
			}
			bldr.WriteByte('\'')
		} else {
			panic("what's left" + trm0.String())
		}
		if pkgwrap {
			bldr.WriteByte(')')
		}
	case *TermT:
		if trm0.kind == Valdecl || trm0.kind == Vardecl {
			sym := trm0.arg0.(*Symbol)
			if sym.dtype == nil {
				bldr.WriteString("type ")
				bldr.WriteString(sym.ident)
				qci.writeType(sym.binding.(*Type), false, bldr)
			} else if itemExists(sym, "fwddecld") {
				// this means we've already declared sym and now just need to bind it via asgnmt
				bldr.WriteString(fmt.Sprintf("%s = ", sym.ident))
				qci.gencode0(sym.binding, bldr, false)
				bldr.WriteByte('\n')
			} else {
				qci.gencodeVar(sym, bldr)
			}
		} else if trm0.kind == Fwddecl {
			// make just a decln
			sym := trm0.arg0.(*Symbol)
			bldr.WriteString(fmt.Sprintf("var %s ", sym.ident))
			qci.writeType(sym.dtype, false, bldr)
			bldr.WriteByte('\n')
		} else if trm0.kind == Lognot {
			// recover != here
			a0 := trm0.arg0
			tg0 := a0.Tag()
			if tg0 == EqExpr || tg0 == EqeqExpr {
				a0tt := a0.(*TermTT)
				compareop(a0tt.arg0, a0tt.arg1, "!=", tg0)
			} else if tg0 == Funcall && a0.(*TermTL).trm.Tag() == SymbolTag && isCompareOperator(a0.(*TermTL).trm.(*Symbol).ident) {
				a0fn := a0.(*TermTL)
				curop := a0fn.trm.(*Symbol).ident
				revop := ""
				if curop == "<" {
					revop = ">="
				} else if curop == "<=" {
					revop = ">"
				} else if curop == ">" {
					revop = "<="
				} else {
					revop = "<"
				}
				compareop(a0fn.args[0], a0fn.args[1], revop, tg0)
				//} else if tg0 == AndandExpr || tg0 == OrorExpr {
				//	fmt.Print("")
			} else {
				bldr.WriteByte('!')
				qci.pushCxt(0)
				prn := qci.parenthesize(a0)
				if prn {
					bldr.WriteByte('(')
				}
				qci.gencode0(a0, bldr, false)
				if prn {
					bldr.WriteByte(')')
				}
				qci.popCxt()
			}
		} else if trm0.kind == LoopStmt {
			bldr.WriteString("for ")
			qci.gencode0(trm0.arg0, bldr, false)
		} else if trm0.kind == BreakStmt {
			bldr.WriteString("break")
		} else if trm0.kind == Deferstmt {
			if trm0.arg0.Tag() == Stmts {
				bldr.WriteString("defer func()")
				qci.writeStmts(trm0.arg0.(*TermL), false, bldr)
				bldr.WriteString("()\n")
			} else {
				bldr.WriteString("defer ")
				qci.gencode0(trm0.arg0, bldr, false)
				bldr.WriteString("\n")
			}
		} else if trm0.kind == Negate {
			bldr.WriteString("-")
			qci.pushCxt(1)
			qci.gencode0(trm0.arg0, bldr, true)
			qci.popCxt()
		} else if trm0.kind == ContinueStmt {
			bldr.WriteString("continue")
		} else if trm0.kind == ReturnStmt {
			bldr.WriteString("return ")
			if trm0.arg0 != nil {
				qci.gencode0(trm0.arg0, bldr, false)
			}
		} else if trm0.kind == Gdref || trm0.kind == TypeAssert {
			qci.gencode0(trm0.arg0, bldr, false)
			innertype := trm0.arg0.Dtype()
			if innertype.family == TFOrtype && !innertype.isNilposs() {
				bldr.WriteString(".(")
				qci.writeType(trm0.dtype, false, bldr)
				bldr.WriteByte(')')
			}
		} else if trm0.kind == ZeroValue {
			seen := []*Type{}
			qci.zvGen(trm0.dtype, bldr, &seen, false)
		} else if trm0.kind != AssertStmt {
			panic("unwritten")
		}
	case *TermTT:
		if trm0.kind == EqExpr || trm0.kind == EqeqExpr {
			compareop(trm0.arg0, trm0.arg1, "==", trm0.kind)
		} else if trm0.kind == AsgnStmt {
			p0 := trm0.arg0.Dtype().family == TFPtr
			a0t := trm0.arg0.Dtype()
			a1t := trm0.arg1.Dtype()
			_, baselit := trm0.arg1.(*TermB)
			baselit = baselit || a1t.family == TFNil
			p1 := a1t.family == TFPtr
			if !(p0 || p1) && (callingNamed(trm0.arg1, "+") || callingNamed(trm0.arg1, "-")) {
				// gratuitous optimzn of X = X + 1 to X++. At this point we have some of the conds; check the rest.
				fcall := trm0.arg1.(*TermTL)
				if fcall.args[0].Equal(trm0.arg0) && fcall.args[1].Tag() == Intlit && fcall.args[1].(*TermB).value == "1" {
					qci.gencode0(trm0.arg0, bldr, false)
					if fcall.trm.(*Symbol).ident == "+" {
						bldr.WriteString("++")
					} else {
						bldr.WriteString("--")
					}
					return
				}
			}
			// handle another special case that I don't understand well enough to generalize properly.
			// Assignment of literals to nilpossible base types that have been ptrzd requires special attn.
			if p0 && baselit && a1t.family != TFNil {
				qci.gencode0(trm0.arg0, bldr, false)
				bldr.WriteString(" = new(")
				qci.writeType(a1t, false, bldr)
				bldr.WriteString("); ")
				qci.gencode0(trm0.arg0, bldr, true)
				bldr.WriteString(" = ")
				qci.gencode0(trm0.arg1, bldr, false)
				bldr.WriteByte('\n')
				return
			}
			if itemExists(trm0.arg0, "ptrzdRcvr") {
				p0 = false
			}
			lhs := trm0.arg0
			if lhs.Tag() == Gdref {
				lhs = lhs.(*TermT).arg0
			}
			qci.gencode0(lhs, bldr, false)
			bldr.WriteString(" = ")
			if !baselit {
				if p0 && !p1 {
					bldr.WriteByte('&')
					//} else if p1 && !p0 {
					//	bldr.WriteByte('*')
				}
			}
			tnm := a0t.plist.Find("typename")
			if a0t.family == TFInt && tnm != nil {
				bldr.WriteString(tnm.(string))
				bldr.WriteByte('(')
			}
			qci.gencode0(trm0.arg1, bldr, true)
			if a0t.family == TFInt && tnm != nil {
				bldr.WriteByte(')')
			}
			bldr.WriteByte('\n')
		} else {
			panic("unwritten")
		}
	case *TermL:
		last := len(trm0.args) - 1
		if trm0.kind == Stmts {
			qci.writeStmts(trm0, true, bldr)
		} else if trm0.kind == IfStmt {
			bldr.WriteString("if ")
			qci.gencode0(trm0.args[0], bldr, false)
			if trm0.args[1].Tag() == Stmts {
				qci.writeStmts(trm0.args[1].(*TermL), len(trm0.args) == 2, bldr)
			} else {
				bldr.WriteString("{ ")
				qci.gencode0(trm0.args[1], bldr, false)
				bldr.WriteString(" }")
			}
			if len(trm0.args) == 3 {
				bldr.WriteString(" else ")
				epart := trm0.args[2]
				elsif := epart
				if epart.Tag() == Stmts && len(epart.(*TermL).args) == 1 {
					elsif = epart.(*TermL).args[0]
				}
				if elsif.Tag() == IfStmt {
					qci.gencode0(elsif, bldr, false)
				} else {
					qci.gencode0(epart, bldr, false)
				}
			}
		} else if trm0.kind == EachStmt {
			itervar := trm0.args[0].(*Symbol)
			iteree := trm0.args[1]
			if iteree.Tag() == Range {
				rangelit := iteree.(*TermT).arg0.(*TermL)
				low := rangelit.args[0]
				high := rangelit.args[1]
				step := rangelit.args[2]
				incl := rangelit.args[3]
				endcond := "<"
				if incl == TrueLiteral {
					endcond = "<="
				}
				neg := false
				if step.Dtype() == TypeInt {
					ival := step.Plist().Find("intval").(int)
					if ival < 0 {
						neg = true
					}
				} else {
					fval := step.Plist().Find("floatval").(float64)
					if fval < 0.0 {
						neg = true
					}
				}
				if neg {
					endcond = ">="
					if incl == FalseLiteral {
						endcond = ">"
					}
				}
				if len(trm0.args) > 3 {
					indexsym := trm0.args[3].(*Symbol)
					fmt.Fprintf(bldr, "%s := 0\n", indexsym.ident)
					stmtargs := trm0.args[2].(*TermL).args
					incrx := makeFuncall(TypeInt.methods.Find("+"), []Term{indexsym, makeIntlit(1)}, TypeNothing, nil)
					trm0.args[2].(*TermL).args = append(stmtargs, makeTermTT(AsgnStmt, indexsym, incrx, TypeNothing, Pos(-1), Pos(-1)))
				}
				fmt.Fprintf(bldr, "for %s := ", itervar.ident)
				qci.gencode0(low, bldr, false)
				fmt.Fprintf(bldr, "; %s %s ", itervar.ident, endcond)
				qci.gencode0(high, bldr, false)
				fmt.Fprintf(bldr, "; %s += ", itervar.ident)
				qci.gencode0(step, bldr, false)
			} else {
				inxident := "_"
				// TraversePre checks if the index symbol is still refncd in the body
				if len(trm0.args) > 3 && TraversePre(trm0.args[2], identWorkfn, trm0.args[3]) {
					inxident = trm0.args[3].(*Symbol).ident
				}
				// The following is a workaround for the fact that right now, using "index" in a chain op over a list creates a
				// separate index variable instead of using the one in go's for range stmt. If "this" isn't used in the loop,
				// then itervar.ident has gotten changed to "_" which means that if we put out the usual code, we'll end
				// up generating "for _, _ := range xxx" and the go compiler will barf. The workaround is to generate "for range xxx"
				if itervar.ident == "_" && inxident == "_" {
					fmt.Fprintf(bldr, "for range ")
				} else {
					fmt.Fprintf(bldr, "for %s, ", inxident)
					bldr.WriteString(itervar.ident)
					bldr.WriteString(" := range ")
				}
				qci.gencode0(iteree, bldr, false)
			}
			bldr.WriteByte(' ')
			qci.gencode0(trm0.args[2], bldr, false)
		} else if trm0.kind == Litform0 {
			qci.gencodeLitform(trm0, bldr)
		} else if trm0.kind == Multiret || trm0.kind == TypeAssertDecl {
			ident0 := trm0.args[0].(*Symbol).ident
			ident1 := trm0.args[1].(*Symbol).ident
			if ident0 != "_" || ident1 != "_" {
				bldr.WriteString(ident0)
				bldr.WriteString(", ")
				bldr.WriteString(ident1)
				bldr.WriteString(" := ")
			}
			qci.gencode0(trm0.args[2], bldr, false)
		} else if trm0.kind == Ifcase {
			// go's switch stmt might work but we need to check
			// for now we do a switch if every lhs (except possibly the default) is of the form X = Y
			// where X is const and Y varies
			lhs0 := trm0.args[0].(*TermTT).arg0
			var comprnd Term
			if lhs0.Tag() == EqExpr {
				comprnd = lhs0.(*TermTT).arg0
				for _, clz := range trm0.args[1:] {
					clz0 := clz.(*TermTT)
					if clz0.arg0 == TrueLiteral {
						continue
					}
					if !(clz0.arg0.Tag() == EqExpr && clz0.arg0.(*TermTT).arg0.Equal(comprnd)) {
						comprnd = nil
						break
					}
				}
			}
			if comprnd == nil {
				// here transform into chained ITE
				lastinx := len(trm0.args) - 1
				for i, clz := range trm0.args {
					clz0 := clz.(*TermTT)
					if clz0.arg0 != TrueLiteral {
						bldr.WriteString("if(")
						qci.gencode0(clz0.arg0, bldr, false)
						bldr.WriteString(") ")
					}
					if clz0.arg1.Tag() == Stmts {
						qci.writeStmts(clz0.arg1.(*TermL), false, bldr)
					} else {
						bldr.WriteString("{ ")
						qci.gencode0(trm0.args[1], bldr, false)
						bldr.WriteString(" }")
					}
					if i < lastinx {
						bldr.WriteString(" else ")
					} else {
						bldr.WriteString("\n")
					}
				}
			} else {
				// switch works
				bldr.WriteString("switch ")
				qci.gencode0(comprnd, bldr, false)
				bldr.WriteString(" {\n")
				for _, clz := range trm0.args {
					clz0 := clz.(*TermTT)
					if clz0.arg0 == TrueLiteral {
						bldr.WriteString("default:\n")
					} else {
						bldr.WriteString("case ")
						qci.gencode0(clz0.arg0.(*TermTT).arg1, bldr, false)
						bldr.WriteString(":\n")
					}
					qci.gencode0(clz0.arg1, bldr, false)
				}
				bldr.WriteString("}\n")
			}
		} else {
			inter := ""
			if trm0.kind == Symchain {
				inter = "."
			} else if trm0.kind == AndandExpr {
				inter = " && "
			} else if trm0.kind == OrorExpr {
				inter = " || "
			}
			for i, a := range trm0.args {
				prn := qci.parenthesize(a)
				if prn {
					bldr.WriteByte('(')
				}
				qci.gencode0(a, bldr, false)
				if prn {
					bldr.WriteByte(')')
				}
				if i < last {
					bldr.WriteString(inter)
				}
			}
		}
	case *TermTL:
		if trm0.kind == Funcall {
			qci.genFuncall(trm0, bldr)
		} else if trm0.kind == Typecase {
			// handle specific types with a type switch and structural ones with reflect. Note
			// that qci.namedTypes has added reflect to the modules list if we'll need it.
			//
			// A problem with Typecase is that the guarded symbol is not generated yet, but it must
			// be substd for gdref'd instances of the head expr (trm0.trm) in the body. And the gdrefs
			// are per type. So, I'm doing the substn here, despite the lateness of the hour.
			// A difficulty with changing it is there's no spot in the Term for the guarded symbol.
			// Only options I can think of are postfix to arglist or in plist item (ie hidden). Both ugly.
			// todo: consider moving it earlier.
			gdrefx := makeTermT(Gdref, trm0.trm, TypeNothing, Pos(-1), Pos(-1)).(*TermT)
			var guarded *Symbol
			skipped := -1
			switched := false
			dfltclz := -1
			for inx, clz := range trm0.args {
				casetyp := clz.(*TermTT).arg0.(*Type)
				if casetyp.family == TFTypred {
					if casetyp.v.(*Typred).ident == "entity" {
						dfltclz = inx
					} else if skipped >= 0 {
						panic("cannot (yet) handle typepred cases other than list")
					} else {
						skipped = inx
					}
					continue
				}
				if !switched {
					switched = true
					guarded = makeSymbol(gensym(), casetyp, nil)
					fmt.Fprintf(bldr, "switch %s := ", guarded.ident)
					qci.gencode0(trm0.trm, bldr, false)
					bldr.WriteString(".(type) {\n")
				}
				bldr.WriteString("case ")
				qci.writeType0(casetyp, false, bldr)
				bldr.WriteString(":\n")
				gdrefx.dtype = casetyp
				rhs := clz.(*TermTT).arg1
				rhs = runSubstn([]Term{gdrefx}, []Term{guarded}, rhs, false)
				qci.gencode0(rhs, bldr, false)
				bldr.WriteByte('\n')
			}
			// At this point, all the specific type cases are generated; various SVs (skipped, switched, dfltclz)
			// tell us whether there are structural cases and/or a default case. Note that if there are no specific
			// type cases, switched will still be false so we only generate a reflect-based if stmt. Also, if there's
			// an else (default) *and* structural cases, we need to move the d8m-based else into the reflect switch,
			// since the reflect switch will be inside the typeswitch default.
			if skipped >= 0 {
				if switched {
					bldr.WriteString("default:\n")
				}
				clz := trm0.args[skipped].(*TermTT)
				if clz.arg0.(*Type).v.(*Typred).ident == "list" {
					rflected := gensym()
					bldr.WriteString(rflected)
					bldr.WriteString(" := reflect.ValueOf(")
					qci.gencode0(trm0.trm, bldr, false)
					bldr.WriteString(")\nif ")
					bldr.WriteString(rflected)
					bldr.WriteString(".Kind() == reflect.Slice {\n")
					qci.gencodeGncList(rflected, trm0.trm, clz.arg1, bldr)
				} else {
					panic("cannot (yet) handle typepred cases other than list")
				}
				if dfltclz >= 0 {
					clz = trm0.args[dfltclz].(*TermTT)
					bldr.WriteString("} else {\n")
					qci.gencode0(clz.arg1, bldr, false)
					bldr.WriteString("}\n")
				} else {
					bldr.WriteString("}\n")
				}
			}
			if switched {
				bldr.WriteString("}\n")
			}
		} else {
			panic("unwritten")
		}
	default:
		panic("shouldn't happen")
	}
	//fmt.Println()
}

// This is for debugging. Look for types with generic in them.
func genericWorkfn(trm Term, s interface{}) int {
	typ := trm.Dtype()
	if typ != nil && typ.isGeneric() {
		fmt.Println(trm.String(), "is generic")
	}
	return 0
}

// Run the full backend optimzn suite on sym; effects arise via modding of sym binding and/or qci state.
// However, for convenience in returning errors detected in rewriting (e.g. non-manifest values), we return
// a Term. The normal return value is nil; the only other one will be ErrorTag.
// This method should not be called on type-valued symbols.
// The calls to optimize are driven by qci.externs which is mainly a list of the gbl decls we're going to make
// in the generated code. There are two extensions to this basic idea: (1) the "body" of the query is at index 0 in
// a symbol with ident="main" whose binding is a Stmts term, not a funi; (2) in order to get back back end ftns to run
// on every relevant bit of code, we add the funi's a/w iaaf's initzg gbl vars as bindings of symbols with ident="".
// These are codegen'd as their iaaf's, not in this separate manifestation. The smash semantics of backendFixups and the
// like ensure that this arrangement does what we want.
func (qci *QCInfo) optimize(sym *Symbol, gscope *Scope) Term {
	if sym.ident == "" || sym.binding == nil { // we've already seen this one
		return nil
	}
	bdg := sym.binding
	qci.optimizing = sym
	if bdg.Tag() == TypeTag {
		panic("should not happen")
	}
	lithook := false
	if sym.dtype.family == TFFtn {
		bdg = bdg.(*Funinst).body
	} else if sym.ident == "main" {
		if bdg.Tag() != Stmts {
			bdg = makeScopedTermL(Stmts, makeScope(), []Term{bdg}, bdg.Dtype(), Pos(-1), Pos(-1))
			bdg.(*TermL).scope.parent = gscope
		}
	} else {
		// after optimizing initializing expr, check if it needs to be an iaaf
		// Note that it may already be an iaaf, esp one due to lithook xfm
		// First, check if it's something simple, can punt if so.
		_, simple := bdg.(*TermB)
		if bdg.Tag() == SymbolTag {
			simple = true
		} else if bdg.Tag() == Litform0 {
			simple = true
			for _, x := range bdg.(*TermL).args {
				_, tb0 := x.(*TermB)
				if !tb0 {
					simple = false
					break
				}
			}
		}
		if simple {
			return nil
		}
		// here, not something simple; if already a iaaf, optimize that else wrap and optimize the initializer
		if bdg.Tag() == Funcall && bdg.(*TermTL).trm.Tag() == FuninstTag {
			lithook = true
			bdg = bdg.(*TermTL).trm.(*Funinst).body
		} else if bdg.Tag() != Stmts {
			bdg = makeScopedTermL(Stmts, makeScope(), []Term{bdg}, bdg.Dtype(), Pos(-1), Pos(-1))
			bdg.(*TermL).scope.parent = gscope
		}
	}
	// now we come to the main part of optimize: bdg is a stmtlist to be optimized.
	// First, apply rewrite rules to fixpt on the "source level" (aka uninlined) form.
	// This is an iterate to fixpt loop that only calls applyRewrites0.
	changed := true
	for repeats := 0; changed; repeats++ {
		if repeats > 50 {
			panic("infinite loop (1)")
		}
		if prOptimize {
			logger.Println("starting rewrites")
		}
		if prTiming {
			logger.Println("starting rewrite loop, repeat", repeats+1, "at", time.Now().Format(time.StampNano))
		}
		bdg, changed = qci.applyRewrites0(bdg)
		if bdg.Tag() == ErrorTag {
			return bdg.(*TermB)
		}
	}
	// Then do another iterate to fixpt loop that sequences inlining, cleanup, rewrites.
	// Usually, this runs all inlining on first pass but occasionally rewrite rules will introduce new inlining
	// opportunities. Since OOLXforms exits as soon as rule succeeds, we'll run this loop at least as many times as
	// there are rules that are going to fire.
	// The "infloop detection" threshold (100) is arbitrary. In fact, it used to be 50.
	changed = true
	bdg0 := bdg
	for repeats := 0; changed; repeats++ {
		if repeats > 100 {
			panic("infinite loop (2)")
		}
		if prTiming {
			logger.Println("starting main loop, repeat", repeats+1, "at", time.Now().Format(time.StampNano))
		}
		cui := makeCleanupInfo()
		if sym.binding != nil && sym.binding.Tag() == FuninstTag && sym.dtype.isMod() {
			// must ensure cui.symdict has refnc to mod rcvr
			funi := sym.binding.(*Funinst)
			rcvr := funi.funargs[0]
			cui.symdict[rcvr] = makeSymCleanup(0)
		}
		qci.seen = qci.seen[0:0] // clear seen before running
		cb4 := qci.inlineCounter
		if prOptimize {
			logger.Println("starting OOLXforms on", bdg0.String())
		}
		qci.recursiveInlines = qci.recursiveInlines[:0] // clear for each pass
		bdg1 := qci.OOLXforms(bdg0)
		changed = cb4 != qci.inlineCounter
		if prOptimize {
			logger.Println("after OOLXforms, bdg1=", bdg1.String())
		}
		if prTiming {
			logger.Println("cleanup for main, repeat", repeats+1, "at", time.Now().Format(time.StampNano))
		}
		bdg2 := cui.cleanup(bdg1)
		ch2 := true
		if prOptimize {
			logger.Println("starting rewrites")
		}
		if prTiming {
			logger.Println("rewrite for main, repeat", repeats+1, "at", time.Now().Format(time.StampNano))
		}
		for repeats2 := 0; ch2; repeats2++ {
			if repeats2 > 50 {
				panic("infinite loop (3)")
			}
			bdg2, ch2 = qci.applyRewrites0(bdg2)
			if bdg2.Tag() == ErrorTag {
				return bdg2.(*TermB)
			}
			if ch2 {
				changed = true
			} else {
				bdg0 = bdg2
			}
		}
	}
	if prOptimize {
		logger.Println("finished optimzn loop")
	}
	// Now bdg0 is a fully optimized stmtlist. Check where it came from and arrange to re-insert it, and
	// possibly to thunkify the code for gbl var initzn.
	stmtlist := bdg0.(*TermL)
	if sym.dtype.family == TFFtn {
		sym.binding.(*Funinst).body = stmtlist
	} else if sym.ident == "main" {
		sym.binding = bdg0
	} else if lithook {
		funi := sym.binding.(*TermTL).trm.(*Funinst)
		funi.body = stmtlist
		qci.addExtern(makeSymbol("", TypeNothing, funi))
	} else if len(stmtlist.args) == 1 {
		sym.binding = stmtlist.args[0]
	} else {
		thunktyp := makeType(TFFtn, []*Type{}, bdg0.Dtype(), true)
		thunk := makeFuninst(makeScope(), []*Symbol{}, thunktyp, stmtlist)
		sym.binding = makeFuncall(thunk, []Term{}, bdg0.Dtype(), nil)
	}
	return nil
}

// This debugging ftn will be commented in or out at diff times.
func prExterns(pre string, externs []*Symbol) {
	fmt.Println(pre)
	for _, s := range externs {
		fmt.Println(s.ident, ": ", s.binding.String())
	}
	fmt.Println("***********")

}

// for initial testing of stmapping, I'm going to put driver related stuff here
func queryCompile(trm Term, gscope *Scope) (contents string, errterm Term) {
	smi := makeSTMapInfo(gscope) // I may fold STMapInfo into QCInfo at some point...
	postmap := smi.visit(trm)
	if postmap.Tag() == ErrorTag {
		return "", postmap
	}
	//fmt.Println("postmap: ", postmap.(*TermTL).trm.(*Symbol).binding.String())
	var printWrap = func(trm Term) Term {
		tdt := trm.Dtype()
		if tdt != TypeNothing {
			var stgfied Term
			if tdt.family == TFString {
				stgfied = trm
			} else {
				stgfied = makeFuncall(tdt.stringifyMethod(nil), []Term{trm}, TypeString, nil)
			}
			trm = makeFuncall(biScope["println"][0], []Term{stgfied}, TypeNothing, nil)
		}
		return trm
	}
	if postmap.Tag() == Stmts {
		xtstmts := postmap.(*TermL)
		nstmts := len(xtstmts.args)
		// usually, wrap the retval of the query with println
		xtstmts.args[nstmts-1] = printWrap(xtstmts.args[nstmts-1])
		xtstmts.dtype = TypeNothing
	} else {
		postmap = printWrap(postmap)
	}
	qci := makeQCInfo(gscope.rwActives, smi)
	mainsym := makeSymbol("main", TypeNothing, postmap)
	qci.addExtern(mainsym)
	for extinx := 0; extinx < len(qci.externs); extinx++ {
		// catch and pass along errors from rewriting
		extsym := qci.externs[extinx]
		if prOptimize {
			logger.Println("optimizing item #", extinx, "ident=", extsym.ident, ":", extsym.binding.String())
		}
		if prTiming {
			logger.Println("optimizing", extinx, extsym.ident, "at", time.Now().Format(time.StampNano))
		}
		errterm = qci.optimize(extsym, gblScope)
		if errterm != nil {
			return
		}
		if prOptimize {
			logger.Println("post-optimize: ", qci.externs[extinx].binding.String())
		}
	}
	if prTiming {
		logger.Println("time after optimzn:", time.Now().Format(time.StampNano))
	}
	//prExterns("after fixups: ", qci.externs)
	qci.namedTypes(qci.externs[0].binding)
	for _, sym := range qci.externs[1:] {
		qci.namedTypes(sym.binding)
	}
	for gminx := 0; gminx < len(qci.gomethods); gminx++ {
		gmthd := smi.visit(qci.gomethods[gminx]).(*Symbol) // copy now, since it wasn't previously
		errterm = qci.optimize(gmthd, gblScope)
		if errterm != nil {
			return
		}
		qci.gomethods[gminx] = gmthd
	}
	if prTiming {
		logger.Println("time after gomethods:", time.Now().Format(time.StampNano))
	}
	qci.backendFixups()
	if prTiming {
		logger.Println("time after backendFixups:", time.Now().Format(time.StampNano))
	}
	qci.pointerize()
	if prTiming {
		logger.Println("time after ptrz:", time.Now().Format(time.StampNano))
	}
	qci.rename2go()
	if prTiming {
		logger.Println("time after rename2go:", time.Now().Format(time.StampNano))
	}
	contents = qci.gencode()
	if prTiming {
		logger.Println("time after codegen:", time.Now().Format(time.StampNano))
	}
	return
}
