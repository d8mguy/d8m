// Define Scopes generally, and also the "built in Scope" (which is different), plus helper types like SymXDict
// Define code to apply STMap items, run stmappings on terms, etc.

package main

import (
	"fmt"
)

// BIScopeMap is for the builtin scope, which is big enough to merit a map.
// The range is a slice to accomodate multibinding of BI symbols. Each Symbol
// associated with a given string has that string as its ident. The upscope indexes
// elements of the slice.
type BIScopeMap map[string][]*Symbol

var biScope = make(BIScopeMap, 200)

func (s BIScopeMap) Symbols(ident string) []*Symbol { return s[ident] }

// Add just does the obvious thing, handling expansion of each slice. The client
// must handle multibinding rules.
func (s BIScopeMap) Add(sym *Symbol) {
	s[sym.ident] = append(s[sym.ident], sym)
}

// SymX provides a key-value [Symbol, xcount] pair used in "small" scopes (which
// is almost all of them). They're stored in ident order of the Symbols (to
// allow binsearch); the xcount integer is a hack for multibinding, tells how many
// Symbols with the same identifier are "above" this one in *all* scopes. We make
// the slice type sortable, that comes in handy.
type SymX struct {
	sym    *Symbol
	xcount int
}

func (sx SymX) String() string {
	return fmt.Sprintf("%s : %d", sx.sym.ident, sx.xcount)
}

type SymXDict []SymX

// make this sortable according to go's sort pkg.
func (lst SymXDict) Less(i, j int) bool { return lst[i].sym.ident <= lst[j].sym.ident }
func (lst SymXDict) Swap(i, j int)      { lst[i], lst[j] = lst[j], lst[i] }
func (lst SymXDict) Len() int           { return len(lst) }

// Index uses binsearch to find and return the greatest i in d s.t. d[i].ident <= kv.
// Will return len(d) if the last elt of d has ident < kv.
func (d SymXDict) Index(kv string) int {
	if len(d) == 0 {
		return 0
	} else if d[len(d)-1].sym.ident < kv {
		return len(d)
	}
	mn, mx := 0, len(d)-1
	cur := mx / 2
	for mn <= mx {
		v := d[cur].sym.ident
		if v < kv {
			mn = cur + 1
		} else {
			mx = cur - 1
		}
		cur = mn + (mx-mn)/2
	}
	return cur
}

// IndexExact uses index then a == search; returns index if it finds elt == sym, else -1
func (d SymXDict) IndexExact(sym *Symbol) int {
	inx := d.Index(sym.ident)
	if inx < len(d) {
		for ; inx < len(d) && d[inx].sym.ident == sym.ident; inx++ {
			if d[inx].sym == sym {
				return inx
			}
		}
	}
	return -1
}

// Find combines index with a check for equality; returns nil if not found.
func (d SymXDict) Find(kv string) *Symbol {
	inx := d.Index(kv)
	if inx < len(d) && d[inx].sym.ident == kv {
		return d[inx].sym
	}
	return nil
}

// Add sym to the dict with given xcount. Assume caller checks and enforces redefn rules.
func (dp *SymXDict) Add(sym *Symbol, xcnt int) {
	inx := dp.Index(sym.ident) // inx is largest index <= sym.ident
	dlen := len(*dp)
	symx0 := SymX{sym, xcnt}
	// loop so as to put the new entry *after* any existing ones with same ident.
	for ; ; inx += 1 {
		if inx == dlen {
			*dp = append(*dp, symx0)
			break
		} else if (*dp)[inx].sym.ident != sym.ident {
			*dp = append(*dp, SymX{nil, 0})
			if inx < dlen {
				copy((*dp)[inx+1:dlen+1], (*dp)[inx:dlen])
			}
			(*dp)[inx] = symx0
			break
		}
	}
}

// Like Add but lower level: add at the exact index given; the SymX is already provided
func (dp *SymXDict) AddAt(x SymX, inx int) {
	dlen := len(*dp)
	if inx == dlen {
		*dp = append(*dp, x)
	} else {
		*dp = append(*dp, SymX{nil, 0})
		if inx < dlen {
			copy((*dp)[inx+1:dlen+1], (*dp)[inx:dlen])
		}
		(*dp)[inx] = x
	}
}

// Merge another SymXDict into the rcvr
func (dp *SymXDict) Merge(other SymXDict) {
	for _, sx := range other {
		// todo: fix the upcount; this code isn't always right
		dp.Add(sx.sym, sx.xcount)
	}
}

// Remove sym from dp. Panic if not present.
func (dp *SymXDict) Remove(sym *Symbol) {
	inx := dp.Index(sym.ident)
	ndp := len(*dp)
	for ; inx < ndp; inx++ {
		if (*dp)[inx].sym == sym {
			copy((*dp)[inx:ndp-1], (*dp)[inx+1:ndp])
			*dp = (*dp)[:ndp-1]
			return
		}
	}
	panic("not reached")
}

// Scope defines a collection of Symbols with (multi-)bindings and a link to parent
// that's used to define a scope tree.
type Scope struct {
	entries    SymXDict // this is binsearched, see defn of SymXDict
	parent     *Scope
	importPkgs []string // record path names of pkgs and modules for cleanup on scope exit
	importMdls []string
	stitems    []*Stitem    // nil except in scopes where stmap stmts occur
	stActives  []*applyItem // active applyST items
	rwActives  []Term       // rewrite stmts encountered in this scope
	plist      Plist
	topscope   bool // manually set to true for gbl scope and import scopes
}

func makeScope() *Scope {
	scp := Scope{make(SymXDict, 0, 3), nil, nil, nil, nil,
		nil, nil, zeroPlist(), false}
	return &scp
}

func (scp *Scope) String() string {
	cnt := len(scp.entries)
	rest := ""
	if cnt > 0 {
		rest = fmt.Sprint(",...", scp.entries[0].sym.ident)
	}
	return fmt.Sprintf("scope(%d%s)", cnt, rest)
}

// For debugging only: look for cycles in scopetree links and panic if found
func (self *Scope) findcycles() {
	var fc0 func(cur *Scope, seen *[]*Scope)
	fc0 = func(cur *Scope, seen *[]*Scope) {
		sb := *seen
		for _, s := range sb {
			if cur == s {
				panic("loop")
			}
		}
		if len(sb) == cap(sb) {
			if len(sb) > 100 {
				panic("too many")
			}
			sb0 := make([]*Scope, len(sb), 2*len(sb))
			copy(sb0, sb)
			sb = sb0
		}
		*seen = append(sb, cur)
		if cur.parent != nil {
			fc0(cur.parent, seen)
		}
	}
	myseen := make([]*Scope, 0, 10)
	fc0(self, &myseen)
}

// Copy self, optionally including the entries, else just copying the other stuff and allocating space for entries
func (self *Scope) Copy(entries bool) *Scope {
	dict2 := make(SymXDict, 0, len(self.entries))
	if entries {
		dict2 = dict2[:len(self.entries)]
		copy(dict2, self.entries)
	}
	return &Scope{dict2, self.parent, self.importPkgs, self.importMdls, self.stitems,
		self.stActives, self.rwActives, *self.plist.Copy(), self.topscope}
}

func (scp *Scope) addAssertions(trms []Term) {
	ret := scp.plist.Find(("assertions"))
	var ret0 []Term
	if ret != nil {
		ret0 = append(ret.([]Term), trms...)
	} else {
		ret0 = trms
	}
	scp.plist.Add("assertion", ret0)
}

func (scp *Scope) getAssertions() []Term {
	ret := scp.plist.Find(("assertions"))
	if ret != nil {
		return ret.([]Term)
	}
	return nil
}

// Clear GTS bindings of all types in Scope by calling Type.clearGTS
func (s *Scope) clearGTS() {
	seen := make([]*Type, 0, 10)
	for _, sx := range s.entries {
		sym := sx.sym
		if sym.dtype == nil {
			typ, isTyp := sym.binding.(*Type)
			if isTyp {
				typ.clearGTS(&seen)
			}
		}
	}
}

// Add given symbol to this Scope. The real work is done in the dict.
// The client should know the upcount because it looked up the identifier earlier
// for the purpose of checking the redefinition rules.
func (s *Scope) Add(sym *Symbol, upcount int) {
	s.entries.Add(sym, upcount)
	sym.homescope = s
}

// AddIfPossible checks if the proposed symbol can legally be added to the
// current scope tree. If so, it adds it (using entries.Add) and returns ""; else
// it returns a string explaining why this is wrong, appropriate for an errmsg.
// As a special case, if there is a symbol identical to sym in the scope, ignore it
// and return "". This handles the way mutually recursive ftns are defined.
// todo: implement the checking
func (s *Scope) AddIfPossible(sym *Symbol) (*Symbol, string) {
	inx := s.entries.Index(sym.ident)
	var existing *Symbol
	if inx < len(s.entries) && s.entries[inx].sym.ident == sym.ident {
		existing = s.entries[inx].sym
	}
	upcount := len(biScope[sym.ident])
	if existing == sym {
		return sym, ""
	} else if existing != nil {
		// This is one small part of what needs to be much more extensive checking.
		// It is for fwd type decls.
		if sym.dtype == nil && existing.dtype == nil && existing.binding.(*Type).family == TFUnknown {
			// Since the existing type might be refncd by others, we smash the existing one.
			existingType := existing.binding.(*Type)
			symType := sym.binding.(*Type)
			existingType.family = symType.family
			existingType.v = symType.v
			existingType.plist = symType.plist
			existingType.cachedString = symType.cachedString
			existingType.generator = symType.generator
			existingType.instInx = symType.instInx
			existingType.methods = symType.methods
			existingType.asserts = symType.asserts
			existingType.handlers = symType.handlers
			return sym, ""
		} else if existing.binding == nil && existing.dtype.Equal(sym.dtype) && sym.binding != nil {
			existing.binding = sym.binding
			return existing, ""
		}
		ntg := sym.binding.Tag()
		var xtg Termtag
		if existing.binding != nil {
			xtg = existing.binding.Tag()
		}
		var typish = func(tg Termtag) bool {
			return tg == TypeTag || tg == TypegenTag || tg == TypepredExpr
		}
		if typish(ntg) && typish(xtg) {
			return sym, "cannot multibind type-like symbols"
		} else if existing.dtype != nil && sym.dtype != nil && !(existing.dtype.family == TFFtn && sym.dtype.family == TFFtn) {
			return sym, "cannot redeclare " + existing.ident
		}
		upcount = s.entries[inx].xcount + 1
		for ; inx < len(s.entries) && s.entries[inx].sym.ident == sym.ident; inx++ {
			s.entries[inx].xcount = upcount
			upcount--
		}
	}
	s.entries.AddAt(SymX{sym, upcount}, inx)
	return sym, "" // wrong!
}

// Lookup searches up the scope tree looking for Symbols whose identifiers match
// ident. If none are found, return nil, else return the first one and the number
// of Symbols "above" this one in any accessible scope. (Ie any scope along the
// chain this one's on, including the BI scope.)
// Since types and typreds don't multibind, this rigamarole is not needed for them.
// Since types multibind with entities an issue arises when a type meaning is "above"
// an entity meaning. We handle this by requiring entity lookup to be prepared to
// find fewer entities than there are symbols.
// Since multibinding isn't all that common, we don't want to add overhead to normal
// lookup so Lookup always returns the first Symbol and if its upcount return value
// is >0 the client should call LookupN to get subsequent interpretations.
//
// Since some scopetrees don't include the global scope, we check if gblScope is last
// and if not, check it explicitly. BIScope is the same story except that it's never
// explicitly in a scopetree.
//
// Note that we can call this method just to see how many existing defns involve this
// identifier. If trm == nil then none, else upcount gives a basis for the upcount of
// an additional meaning, as long as it is added in front of the others (or is in
// a nested scope).
//
// The checking for sym.dtype == nil ensures that Lookup doesn't return type-valued
// Symbols, since this is how they are represented.
func (s *Scope) Lookup(ident string) (sym *Symbol, upcount int) {
	var check1 = func(entries SymXDict) *SymX {
		i := entries.Index(ident)
		if i < len(entries) && entries[i].sym.ident == ident && entries[i].sym.dtype != nil {
			return &entries[i]
		}
		return nil
	}
	isTop := false
	for curscope := s; curscope != nil; curscope = curscope.parent {
		isTop = curscope.topscope
		sx := check1(curscope.entries)
		if sx != nil {
			upcount = sx.xcount
			sym = sx.sym
			return
		}
	}
	// if last scope seen isn't topscope, check that explicitly
	if !isTop {
		sx := check1(gblScope.entries)
		if sx != nil {
			upcount = sx.xcount
			sym = sx.sym
			return
		}
	}
	syms := biScope[ident]
	if len(syms) > 0 {
		if syms[0].dtype == nil {
			if len(syms) > 1 {
				sym = syms[1]
				upcount = len(syms) - 2
			}
		} else {
			sym = syms[0]
			upcount = len(syms) - 1
		}
	}
	return
}

// Specialized ftn, added late in develt. Answer whether ident is the ident of a local vbl. Use the "funlitScope" plist item.
func (s *Scope) LookupLocal(ident string) bool {
	for curscope := s; curscope != nil; curscope = curscope.parent {
		e := curscope.entries
		i := e.Index(ident)
		if i < len(e) && e[i].sym.ident == ident {
			return true
		}
		if curscope.plist.Find("funlitScpe") != nil {
			break
		}
	}
	return false
}

// LookupN is the other part of the protocol for handling multi-scope multibinding.
// Lookup finds the first Symbol (if any) and the total number of Symbols in the scope
// tree that have ident as identifier. LookupN finds the N'th (counting from 0).
// The implication of this is that rather than passing back an updated Scope, we
// start at the leaf of the scope tree each time. This is O(N^2)!! Horrors! But it
// is also a lot easier to code, debug, and understand. And given the expected frequency
// of multiscope multibinding, it should have zero impact on actual performance.
//
// Note that since the client
// requests each symbol, it can handle mixtures of upscope types with entities.
func (s *Scope) LookupN(ident string, inx int) *Symbol {
	for curscope := s; curscope != nil; curscope = curscope.parent {
		e := curscope.entries
		i := e.Index(ident)
		if i < len(e) && e[i].sym.ident == ident {
			if inx == 0 {
				return e[i].sym
			}
			inx -= 1
			for i += 1; i < len(e) && e[i].sym.ident == ident; i += 1 {
				if inx == 0 {
					return e[i].sym
				}
				inx -= 1
			}
		}
	}
	syms := biScope[ident]
	if syms == nil || len(syms) <= inx {
		return nil
	}
	return syms[inx]
}

// tryAutocvt gets two Types, repg what we have and what we want. It looks for an autocvt ftn from one
// to the other and returns it else nil.
// tryAutocvt is here, instead of in TC, because it can't use std LookupFtn. Because infloop.
func tryAutocvt(have, wanted *Type, tci *TCInfo) *Symbol {
	if wanted.family == TFTypred {
		typred := wanted.v.(*Typred)
		if typred.ident == "enumerable" {
			// replace this wanted with genz list
			wanted = tci.scopes.lookupTypegen("list").binding.(*Typegen).dtype
		}
	}
	// the following hack ensures that an attempted autocvt doesn't leave detritus in GTS bindings but shows that
	// a better code design is needed.
	gncs := wanted.generics()
	// we want to clear the gncs not already bound
	for i := 0; i < len(gncs); i++ {
		if gncs[i].v.(*GTS).binding != nil {
			copy(gncs[i:], gncs[i+1:])
			gncs = gncs[:len(gncs)-1]
			i--
		}
	}
	defer func() {
		// delete any bindings that were created in the above calls to compat
		for _, gt := range gncs {
			gt.v.(*GTS).binding = nil
		}
	}()
	inx := 0
	acfn, upcnt := tci.scopes.Lookup("autocvt")
	for {
		if acfn == nil {
			break
		}
		acfntyp := acfn.dtype.v.(*Ftntype)
		seen := []*Type{}
		if have.compat(acfntyp.fmlargs[0], &seen) && wanted.compat(acfntyp.rettype, &seen) {
			break
		}
		if upcnt == inx {
			return nil
		}
		inx += 1
		acfn = tci.scopes.LookupN("autocvt", inx)
	}
	return acfn
}

// sym is function-valued; check whether it's generic with cached instances that match actuals.
// If so, return the appropriate instance, else nil.
func (sym *Symbol) cachedGeneric(actuals []Term) *Symbol {
	var sym0 *Symbol
	if len(actuals) == 0 { // shouldn't happen
		// fmt.Println("*** cachedGeneric called with no actuals ***")
		return nil
	}
	xstg := sym.plist.Find("cachedInstances") // look on cache first
	if xstg != nil {
		xstg0 := xstg.([]interface{})
		for _, elt := range xstg0 {
			elt0 := elt.([]Term)
			found := true
			for i, conc := range actuals {
				if !conc.Equal(elt0[i]) {
					found = false
					break
				}
			}
			if found {
				sym0 = elt0[len(elt0)-1].(*Symbol)
			}
		}
	}
	return sym0
}

// This exception for matchFun is slightly complicated.
func enumonlyHack(actual, wanted *Type) bool {
	seen := []*Type{}
	if wanted.family == TFList && actual.compat(TPEnumerable, &seen) {
		eltact := actual.Elttype()
		seen = []*Type{}
		return eltact.compat(wanted.v.(*Type), &seen)
	}
	return false
}

// Matchfun is where multibinding of functions happens. Since functions can be stored as independent symbols
// in symbol tables or as methods, this is called in several places. From symbol tables,
// it's called on every matching identifier, so it may be called on non-ftns.
// In other cases, it's called by looking up methods on the type of the first arg (aka the receiver).
// The arguments named args and cooked hold the actual function arguments before and after TC.
// If rettype == nil, it's unconstrained, else the rettype is checked after the args succeed.
// The unchecked function args are in 'args' while 'cooked' comes in either empty or with the rcvr arg filled in.
// On success it is returned with all the entries filled in with the checked versions.
// The return convention is: on success return the receiver or a grounded version of the receiver. On
// failure, return nil.
// Generic ftns have a "givenscope" on their plist. In such case, ground the ftn after matching.
// There is also a caching scheme for grounded symbols embedded in the "cachedInstances" plist item.
//
// When I designed this I didn't consider the possibility of needing to apply autocvt to the result to make
// the rettype work. Specifically, suppose sym matches on args, rettype is non-nil, and sym doesn't match on rettype
// but there's an autocvt ftn that would fix this. Then the spec requires the funcall to be wrapped with that autocvt
// ftn. That's damned inconvenient given that MatchFun returns the function Symbol and the caller creates the Funcall
// term. My workaround is to add a second retval, acfn. Most of the time it will be nil but the caller needs to check
// and wrap if non-nil.
func (sym *Symbol) MatchFun(args, cooked []Term, rettype *Type, tci *TCInfo) (matched *Symbol, acfn *Symbol) {
	if sym.dtype.family != TFFtn {
		return
	}
	gncs := sym.dtype.generics()
	defer func() {
		for _, gt := range gncs {
			gtv, isGTS := gt.v.(*GTS)
			if isGTS {
				gtv.binding = nil
			}
		}
	}()
	symtype := sym.dtype
	funtype := symtype.v.(*Ftntype)
	if itemExists(sym, "varany") {
		// A BI method (eg of log.Logger); just check that args TC uncnttd
		for i, a := range args[1:] {
			cooked[i+1] = a.Typecheck(nil, tci)
			if cooked[i+1].Tag() == ErrorTag {
				tci.innerError = cooked[i+1]
				return
			}
		}
		matched = sym
		return
	}
	nargs := len(args)
	nargsfml := len(funtype.fmlargs)
	if nargs != nargsfml {
		return
	}
	tci.considered = append(tci.considered, sym)
	givenscopeItem := sym.plist.Find("givenscope")
	var givenscope *Scope
	if givenscopeItem != nil {
		givenscope = givenscopeItem.(*Scope).Copy(true)
		// copy GTS deeper, in case of nested uses of the same GTS
		for i, sx := range givenscope.entries {
			if sx.sym.dtype == nil {
				b4 := sx.sym.binding.(*Type)
				gts := b4.v.(*GTS)
				aftr := makeType(TFGTS, makeGTS(gts.ident, gts.dtype))
				givenscope.entries[i].sym = makeSymbol(sx.sym.ident, nil, aftr)
				tsi := &typesubstInfo{b4, aftr, make([]*Type, 0, 2)}
				symtype = tsi.typeSubstn(symtype)
				funtype = symtype.v.(*Ftntype)
			}
		}
		givenscope.plist.Add("givenscope", true)
		tci.PushScope(givenscope)
		defer tci.PopScope()
	}
	// if sym has a funinst, check its fmlargs for enumonly tags
	var argsyms []*Symbol
	if sym.binding != nil && sym.binding.Tag() == FuninstTag {
		argsyms = sym.binding.(*Funinst).funargs
	}

	// todo: There's a fairly serious bug here with generics used as generics. Consider a
	// fn F with signature \(list(T), T)->X. Now suppose we use F in a cxt where there's
	// a GTS T and symbol xx:list(T). Suppose I write F(xx, 12). What happens is that xx matches
	// the first arg with T remaining unbound. Then 12 infers to integer which matches T, binding
	// it to integer. Nothing further checks this and the match succeeds, though it should not. I am
	// not sure whether the fix is in compat or in verifying that a GTS which is used but not bound
	// at one point in matchfun is not bound on any other usage.
	// Note that this bug manifests only when you make a coding mistake.
	// todo: another "bug" in code design is that if there's only one interpn of a ftnname we should complain
	// about failure to match on args. Otherwise, an errmsg will be generated but it'll be misleading. Note
	// that this is not something that MatchFun can figure out on its own, the caller needs to tell it.
	for i, a := range args {
		cntt := funtype.fmlargs[i]
		seen := []*Type{}
		eoPossible := argsyms != nil && argsyms[i].plist.Find("enumonly") != nil
		if i == 0 && cooked[0] != nil {
			rtyp := cooked[0].Dtype()
			if !(rtyp.compat(cntt, &seen) || (eoPossible && enumonlyHack(rtyp, cntt))) {
				acfn = tryAutocvt(rtyp, cntt, tci)
				if acfn != nil {
					cooked[0] = makeFuncall(acfn, []Term{cooked[0]}, cntt, nil)
					acfn = nil
				} else {
					tci.innerError = tci.Error(fmt.Sprintf("match of %s fails on arg %d (expect %s, get %s)",
						sym.ident, i, cntt.String(), rtyp.String()), sym)
					return
				}
			}
			continue
		} else {
			if eoPossible {
				tci.makeTentative()
			}
			cooked[i] = a.Typecheck(cntt, tci)
			if eoPossible {
				tci.unmakeTentative()
			}
			if eoPossible && cooked[i].Tag() == ErrorTag {
				tci.innerError = cooked[i]
				cooked[i] = a.Typecheck(TPEnumerable, tci)
				if cooked[i].Tag() != ErrorTag {
					et1 := cntt.Elttype()
					et2 := cooked[i].Dtype().Elttype()
					if et1 != nil && et2 != nil && !et2.compat(et1, &seen) {
						return
					}
				}
			}
			if cooked[i].Tag() == ErrorTag {
				// I'm not sure if this should be filtered; at some point it was, but it wasn't working.
				tci.innerError = cooked[i]
				return
			}
		}
	}
	// check rettype cntt
	seen := []*Type{}
	if rettype != nil && rettype != TypeNothing && !funtype.rettype.compat(rettype, &seen) {
		// rettype cntt check has failed but could still succeed if 3 conditions hold:
		// the formal is nilposs, the actual is guarded, and the main type of the formal is compat
		var nilpossGuard *Type
		if rettype.isNilposs() {
			maintyp := rettype.mainType()
			if !maintyp.compat(funtype.rettype, &seen) {
				return
			}
		} else if funtype.rettype.isNilposs() {
			maintyp := funtype.rettype.mainType()
			for _, gi := range tci.gditems {
				if gi.kind == 0 && gi.pos && gi.about.Tag() == Funcall && gi.about.(*TermTL).trm == sym {
					nilpossGuard = maintyp
					for i, a := range gi.about.(*TermTL).args {
						if !a.Equal(args[i]) {
							nilpossGuard = nil
							break
						}
					}
				} else if gi.about == cooked[0] && gi.kind == 1 {
					nilpossGuard = rettype
					break
				}
			}
			if nilpossGuard == nil || !nilpossGuard.compat(rettype, &seen) {
				return
			}
		} else if funtype.rettype.family == TFTypred && sym.binding.Plist().Find("expectedType") != nil {
			matched = sym
		} else {
			acfn = tryAutocvt(funtype.rettype, rettype, tci)
			if acfn != nil {
				matched = sym
			} else {
				return
			}
		}
	} else if funtype.rettype.family == TFExit {
		tci.exitstmt = true // signal exit to guarding code
	}
	if givenscope != nil {
		// we've matched a generic; create a grounded version and clean up the bindings
		ngiven := len(givenscope.entries)
		abst := make([]*Symbol, 0, ngiven)
		concrete := make([]Term, 0, ngiven)
		for i, sx := range givenscope.entries {
			if sx.sym.dtype == nil { // GTS?
				gts := sx.sym.binding.(*Type)
				bdg := gts.v.(*GTS).binding
				if bdg != nil {
					// tricky bit: pass in the original symbol, not the copy, since the symbol we concretize uses it
					origsym := givenscopeItem.(*Scope).entries[i].sym
					abst = append(abst, origsym)
					concrete = append(concrete, bdg.stripBoundGTS())
					gts.v.(*GTS).binding = nil
				}
			}
		}
		sym0 := sym.cachedGeneric(concrete)
		if sym0 == nil {
			// second check: if concrete and abst are same in everything, don't ground
			nodiffs := true
			for i, a := range abst {
				if a.binding != concrete[i] {
					nodiffs = false
					break
				}
			}
			if nodiffs {
				sym0 = sym
				//fmt.Println("*** got a nodiffs case ***")
			} else {
				//ok, gotta ground this ftn
				//fmt.Println("grounding", sym.ident, "with abst=", abst, "and conc=", concrete)
				czi := makeConczInfo(abst, concrete)
				czd := czi.concretize(sym)
				sym0 = czd.(*Symbol)
				sym.plist.Accum("cachedInstances", append(concrete, sym0))
			}
		}
		sym = sym0 // return the grounded one
	}
	matched = sym
	return
}

// This MatchFun is for funcalls with "computed functions", ie not symbols. These can't
// be generic, as genericity can only arise via definitions inside given stmts. (Nor can
// these functions be methods.) The code here is essentially a simplification of that in
// the Symbol method just above.
func (typ *Type) MatchFun(args, cooked []Term, rettype *Type, tci *TCInfo) bool {
	if typ.family != TFFtn {
		return false
	}
	funtype := typ.v.(*Ftntype)
	nargs := len(args)
	nargsfml := len(funtype.fmlargs)
	if nargs != nargsfml {
		return false
	}
	for i, a := range args {
		cooked[i] = a.Typecheck(funtype.fmlargs[i], tci)
		if cooked[i].Tag() == ErrorTag {
			return false
		}
	}
	var seen []*Type
	if !(rettype == nil || rettype == TypeNothing || funtype.rettype.compat(rettype, &seen)) {
		return false
	}
	return true
}

// LookupFtn is specialized to function multibinding; it uses Lookup and LookupN.
// There should be at most one ftn that matches; return it or nil. If returning nil,
// optionally return a string that describes a "near miss". This obviously subjective
// notion is intended to improve error msgs when multibdg fails. If there is no near
// miss, return "".
// Note that the result parameters are automatically initialized to the default values we want.
func (s *Scope) LookupFtn(ident string, args, cooked []Term, rettype *Type, tci *TCInfo) (matches, acfn *Symbol) {
	inx := 0
	cur, upcnt := s.Lookup(ident)
	for {
		if cur == nil {
			break
		}
		if cur.plist.Find("varany") != nil {
			matches = cur
			return
		}
		if inx < upcnt {
			tci.makeTentative()
		}
		// Clear the rcvr if a previous cntt caused it to TC as an error
		if len(cooked) > 0 && cooked[0] != nil && cooked[0].Tag() == ErrorTag {
			cooked[0] = nil
		}
		// check for the "mustCntt" hack: if mustCntt and no cntt, skip past this interpretation
		// This is terrible from the error msg pov.
		if cur.plist.Find("mustCntt") == nil || rettype != nil {
			// hack to make exit ignore rettype
			if cur.dtype.family == TFFtn && cur.dtype.v.(*Ftntype).rettype == TypeExit {
				rettype = nil
			}
			matches, acfn = cur.MatchFun(args, cooked, rettype, tci)
		}
		if inx < upcnt {
			tci.unmakeTentative()
		}
		if matches != nil {
			return
		}
		if upcnt == inx {
			break
		}
		inx += 1
		cur = s.LookupN(ident, inx)
	}
	return
}

// Special treatment for cvt because:
// (1) autocvt implies cvt
// (2) there's lots of multibinding and this both speeds it up and makes it easier to read
func (s *Scope) lookupCvt(argtyp, tgttyp *Type) *Symbol {
	var check = func(e SymXDict, ident string) int {
		i := e.Index(ident)
		for i < len(e) && e[i].sym.ident == ident {
			ftyp := e[i].sym.dtype.v.(*Ftntype)
			seen := []*Type{}
			if argtyp.compat(ftyp.fmlargs[0], &seen) && tgttyp.compat(ftyp.rettype, &seen) {
				return i
			}
			i++
		}
		return -1
	}
	for curscope := s; curscope != nil; curscope = curscope.parent {
		i := check(curscope.entries, "autocvt")
		if i >= 0 {
			return curscope.entries[i].sym
		}
		i = check(curscope.entries, "cvt")
		if i >= 0 {
			return curscope.entries[i].sym
		}
	}
	var check1 = func(syms []*Symbol) *Symbol {
		for _, sym := range syms {
			ftyp := sym.dtype.v.(*Ftntype)
			seen := []*Type{}
			if argtyp.compat(ftyp.fmlargs[0], &seen) && tgttyp.compat(ftyp.rettype, &seen) {
				return sym
			}
		}
		return nil
	}
	sym := check1(biScope["autocvt"])
	if sym != nil {
		return sym
	}
	return check1(biScope["cvt"])
}

// lookupTypeFn is generic for the scope tree+biScope seach for type, typred, and
// typegen using a thunk named filtfn. We can do it this way because these symbol
// flavors don't multibind relative to each other or themselves.
func (s *Scope) lookupTypeFn(ident string, filtfn func(*Symbol) bool) *Symbol {
	for curscope := s; curscope != nil; curscope = curscope.parent {
		e := curscope.entries
		i := e.Index(ident)
		if i < len(e) && e[i].sym.ident == ident {
			upcount := e[i].xcount
			trm := e[i].sym
			if filtfn(trm) {
				return trm
			}
			if upcount == 0 {
				return nil
			}
			for i += 1; i < len(e) && e[i].sym.ident == ident; i += 1 {
				trm := e[i].sym
				if filtfn(trm) {
					return trm
				}
			}
		}
	}
	syms := biScope[ident]
	for _, sym := range syms {
		if filtfn(sym) {
			return sym
		}
	}
	return nil
}

// Find a Typred meaning for ident in scope on its path to root. This will be a *Type
// with TFTypred family in the *binding*; its dtype will be nil.
func (s *Scope) lookupTypred(ident string) *Symbol {
	var checktprd = func(sym *Symbol) bool {
		if sym.dtype == nil {
			asTyp, isTyp := sym.binding.(*Type)
			if isTyp && asTyp.family == TFTypred {
				return true
			}
		}
		return false
	}
	return s.lookupTypeFn(ident, checktprd)
}

// Find a Type meaning for ident in scope on its path to root. This must not be
// TFTypred family, but its dtype will be nil.
func (s *Scope) lookupType(ident string) *Symbol {
	var checkty = func(sym *Symbol) bool {
		if sym.dtype == nil {
			// The option for sym.binding == nil is for forward decls of types, it's odd but this is the only way
			// a Symbol can arise with both dtype and binding nil.
			if sym.binding == nil || sym.binding.Tag() == TypeTag || sym.binding.Dtype() == Gztype {
				return true
			}
		}
		return false
	}
	return s.lookupTypeFn(ident, checkty)
}

// Find a Typegen meaning for ident; same idea as other type-like variants.
func (s *Scope) lookupTypegen(ident string) *Symbol {
	var checktg = func(sym *Symbol) bool {
		return sym.dtype == nil && sym.binding != nil && sym.binding.Tag() == TypegenTag
	}
	return s.lookupTypeFn(ident, checktg)
}

// Next, a bunch of STMap related code.

// Stitem is the "cooked" internal repn for search purposes, derived from the post-TC form.
type Stitem struct {
	source, dest *Type
	scope        *Scope       // filtered to hold only GTSs; unused in concretized stitems
	matchables   *symbolSlice // matchables here
	derived      []*Stitem    // cached instantiations of this stitem (UD stitems are usually generic)
	rself        *Symbol      // Exprs in funcallRhs have "self" == rself
	xmethods     Bitset       // I'th bit in set if i'th method is xltd
	funcallArgs  [][]Term     // Args for lhs of xltrule; if nil and corresp xmethods bit is set, direct xltrule
	funcallRhs   []Term       // if direct, this is the method symbol, else a general Term, usually a funcall
	attribs      []*Symbol    // Similar slots for attrib translating rules (which will be rare)
	attribRhs    []Term
	enumruleRhs  Term
	overflowLhs  []Term // for the rare case that xmethods doesn't suffice
	overflowRhs  []Term
}

// Create a matchstate for application of a rule.
func (sti *Stitem) makeSTMatchState() *STMatchState {
	return &STMatchState{sti.matchables, sti.rself, make([]Term, len(*sti.matchables)+1)}
}

func (sti *Stitem) String() string {
	return fmt.Sprintf("%s -> %s", sti.source.String(), sti.dest.String())
}

// Retrieve from sti's cache (aka "derived") a concrete stitem for srctyp and tgttype, or create,
// cache, and return a new one.
// When called, sti.scope will have GTS set (or mostly set??) to concretize.
func (sti *Stitem) concretize(srctyp, tgttyp *Type) *Stitem {
	// first, look in the cache
	for _, csti := range sti.derived {
		if tgttyp == csti.dest {
			return csti
		}
	}
	// nope, so create and enter a new one
	if tgttyp.generator == nil {
		// if tgttype isn't gnc, no need to concretize
		return sti
	}
	// Here for gncs; since tgttyp is concrete, we can find param value(s) in its cache
	gncinx := sti.dest.generator.inxCached(sti.dest)
	concinx := tgttyp.generator.inxCached(tgttyp)
	gncs := sti.dest.generator.insts[gncinx] // these have to be symbols
	concs := tgttyp.generator.insts[concinx]
	// filter real gncs from what's currently in gncs
	nargs := len(gncs) - 1
	gncs0 := make([]*Symbol, nargs)
	concs0 := make([]Term, nargs)
	// the GTS are stored in the cache as Type whereas we need Symbol here.
	// In the case where the type is already grounded, this requires setting up a fake symbol.
	// todo: is this a code design issue worth cleaning up?
	for i, s := range gncs[:nargs] {
		if s.Tag() == SymbolTag {
			// GES is a Symbol, use directly
			gncs0[i] = s.(*Symbol)
			concs0[i] = concs[i]
		} else {
			// the type should be in sti.scope, if not, it isn't gnc so copy in fake
			for _, sx := range sti.scope.entries {
				if sx.sym.dtype == nil && sx.sym.binding == s {
					gncs0[i] = sx.sym
					concs0[i] = concs[i]
					break
				}
			}
			if gncs0[i] == nil {
				gncs0[i] = makeSymbol("xxx", nil, gncs[i]) // fake
				concs0[i] = concs[i]
			}
		}
	}
	// Use these to concretize the xltrules.
	nmthds := len(sti.source.methods)
	cmthds := make([]*Symbol, nmthds)
	cfcargs := make([][]Term, nmthds)
	cfcrhs := make([]Term, nmthds)
	concz := makeConczInfo(gncs0, concs0)
	concz.tgen = tgttyp.generator
	// Seed with rself mapping
	nrself := concz.concretize(sti.rself)
	// first, method rules
	for i := 0; i < nmthds; i++ {
		if sti.xmethods.eltof(i) {
			cmthds[i] = srctyp.methods[i].sym
			fca0 := sti.funcallArgs[i]
			if fca0 != nil {
				cfcargs0 := make([]Term, len(fca0))
				for j, a := range fca0 {
					cfcargs0[j] = concz.concretize(a)
				}
				cfcargs[i] = cfcargs0
			}
			cfcrhs[i] = concz.concretize(sti.funcallRhs[i])
		}
	}
	// now attribs and enum
	var catrbs []*Symbol
	var catrbrhs []Term
	if sti.attribs != nil {
		for i, atrb := range sti.attribs {
			catrbs = append(catrbs, concz.concretize(atrb).(*Symbol))
			catrbrhs = append(catrbrhs, concz.concretize(sti.attribRhs[i]))
		}
	}
	var cEnumr Term
	if sti.enumruleRhs != nil {
		cEnumr = concz.concretize(sti.enumruleRhs)
	}
	cmtbles := make(symbolSlice, 0, len(*sti.matchables))
	for _, mbl := range *sti.matchables {
		inx := concz.origsyms.binsearch(mbl.ident)
		if inx >= 0 {
			for ; inx < len(concz.origsyms) && concz.origsyms[inx].ident == mbl.ident; inx++ {
				if concz.origsyms[inx] == mbl {
					cmtbles = append(cmtbles, concz.postsyms[inx])
					break
				}
			}
		}
	}
	// todo: fix scope to include any remaining generics
	csti := &Stitem{srctyp, tgttyp, makeScope(), &cmtbles, nil, nrself.(*Symbol),
		sti.xmethods, cfcargs, cfcrhs, catrbs, catrbrhs,
		cEnumr, sti.overflowLhs, sti.overflowRhs}
	sti.derived = append(sti.derived, csti)
	return csti
}

// Per item info for an active applyST stmt, as calculated by applyST2Stitem.
// Also has state info for use during stmapping.
type applyItem struct {
	srctype *Type // will be concrete in general
	what    Term
	wtag    Termtag
	sti     *Stitem
	cache   []*Stitem
}

// Make and return Stitem like ai.sti but with source == srctype
func (ai *applyItem) equalSourceStitem(srctype *Type) *Stitem {
	sti := ai.sti
	for _, csti := range ai.cache {
		if csti.source == srctype {
			return csti
		}
	}
	cmtbles := make(symbolSlice, len(*sti.matchables))
	tsi := &typesubstInfo{sti.source, srctype, make([]*Type, 0, 2)}
	for i, s := range *sti.matchables {
		t0 := tsi.typeSubstn(s.dtype)
		if t0 != s.dtype {
			cmtbles[i] = makeSymbol(s.ident, t0, nil)
		} else {
			cmtbles[i] = s
		}
	}
	// new funcallArgs are the old ones with new matchables substd for old
	// Since matchables is []*Symbol and subst wants []Term, we copy to entities with the right types
	fcargs := make([][]Term, len(sti.funcallArgs))
	fcrhs := make([]Term, len(sti.funcallRhs))
	b4args := make([]Term, len(cmtbles))
	aftrargs := make([]Term, len(cmtbles))
	for i, a := range cmtbles {
		b4args[i] = (*sti.matchables)[i]
		aftrargs[i] = a
	}
	substargs := &substInfo{b4args, aftrargs, nil, false, false}
	for i, fc := range sti.funcallArgs {
		rhs := sti.funcallRhs[i]
		if rhs != nil {
			fcrhs[i] = substargs.substn(rhs)
		}
		if len(fc) == 0 {
			continue
		}
		nfc := make([]Term, len(fc))
		for j, arg := range fc {
			nfc[j] = substargs.substn(arg)
		}
		fcargs[i] = nfc
	}
	csti := &Stitem{srctype, sti.dest, sti.scope, &cmtbles, nil, sti.rself,
		sti.xmethods, fcargs, fcrhs, sti.attribs, sti.attribRhs,
		sti.enumruleRhs, nil, nil}
	ai.cache = append(ai.cache, csti)
	return csti
}

// Matching an applyItem against a term during stmapping has its complications, which are collected here.
// Use this method for applyItems based on predicates (not symchains).
// If trm matches, return the stitem, which is normally ai.sti but see below. If it doesn't match, return nil.
// The applyItem is created by applyST2Stitem, which is called from applyST TC. (See next defn.) It may be
// very specific (eg if what is a concrete symchain) or not (eg if what is a predicate on uninstantiated typegen).
// Furthermore, the spec for matching stitems is different from TC -- when types are equal for TC purposes but the
// one in the applyItem has assertions, they must be manifestly true in the matching term else no match.
// Considering how stmapping works, it's most convenient to provide a new Stitem with either of these kinds of
// mismatch -- non-identical types due to assertions or due to genericity. The former we do in line, the latter with
// a call to the Stitem.concretize method.
func (ai *applyItem) matchTerm(trm Term, scope *Scope) *Stitem {
	// Start with a sanity check on ai. Probably not needed.
	if ai.wtag != NoTerm && ai.wtag != FuninstTag {
		panic("impossible")
	}
	// Next, check that the predicate is manifest (and true) on trm; the wtag check is due to how "true" is coded
	if !(ai.wtag == NoTerm || isManifestPredicate(scope, ai.what.(*Funinst), trm)) {
		return nil
	}
	// Ok, we have a sane, manifest predicate. Check that the type matches in the stmapping sense.
	trmtyp := trm.Dtype()
	typeMatch := trmtyp.Equal(ai.srctype)
	if typeMatch && ai.sti.dest.generator != nil {
		// assume no need to check assertions but need to concretize on one or more type params.
		// Note that this assumption causes incomplete behavior wrt the spec.
		// here, could still fail due to assertions; if success, definitely need to create stitem matching trmtyp
		return ai.sti.concretize(trmtyp, ai.sti.dest)
	}
	if typeMatch {
		sti0 := ai.sti
		if trmtyp != ai.srctype {
			failtype := sti0.source.satisfies(trmtyp)
			if failtype != nil {
				// we've gone to the trouble of remembering which type the assertion failed on but since we don't have a way
				// here to pass back an errmsg, we drop this info.
				// todo: fix this
				return nil
			}
			// at this point we know the types are not == but they do match.
			// Stmapping won't work until ai.srctype == trmtype, so make a new stitem that has this property.
			sti0 = ai.equalSourceStitem(trmtyp)
		} else if sti0.source != trmtyp {
			sti0 = ai.equalSourceStitem(trmtyp)
		}
		return sti0
	}
	return nil
}

// Called from TC for applyST "funcall"; create and return an applyItem for this, or nil if error.
// Just match source and target types to find the stitem. Note that arg "what" goes into the applyItem
// unless it's a uniformly true predicate. In that case, the applyItem's what slot is set to nil and wtag to NoTerm.
func applyST2Stitem(what Term, tgttyp *Type, scopetree *Scope) *applyItem {
	srctyp := what.Dtype()
	wtg := what.Tag()
	// Interpret a ftn-valued tgt as a predicate. This might be wrong.
	if wtg == SymbolTag && srctyp.family == TFFtn {
		what = what.(*Symbol).binding
		wtg := what.Tag()
		if what == nil || wtg != FuninstTag {
			return nil // this should really be checked in caller; error msg will be wrong
		}
	}
	if wtg == FuninstTag {
		// If it's uniformly true, we code what as nil and wtg as NoTerm
		body := what.(*Funinst).body
		if len(body.args) == 1 && body.args[0].Equal(TrueLiteral) {
			what = nil
			wtg = NoTerm
		}
	}
	if srctyp.family == TFFtn {
		srctyp = srctyp.v.(*Ftntype).fmlargs[0]
	}
	// look for an active rule whose source type is srctyp; concretize the rule if needed
	for scp := scopetree; scp != nil; scp = scp.parent {
		for _, sti := range scp.stitems {
			seen := []*Type{}
			if srctyp.generator != nil && srctyp.generator != sti.source.generator {
				continue
			}
			if !srctyp.compat(sti.source, &seen) {
				sti.scope.clearGTS()
				continue
			}
			assertionFail := sti.source.satisfies(srctyp)
			if assertionFail != nil {
				// todo: use the type in assertionFail to craft a decent error message
				sti.scope.clearGTS()
				continue
			}
			if tgttyp.generator != nil && tgttyp.generator != sti.dest.generator {
				sti.scope.clearGTS()
				continue
			}
			if !tgttyp.compat(sti.dest, &seen) {
				sti.scope.clearGTS()
				continue
			}
			sti0 := sti.concretize(srctyp, tgttyp)
			ai := &applyItem{srctyp, what, wtg, sti0, nil}
			sti.scope.clearGTS()
			return ai
		}
	}
	return nil
}

// A list of symdictEntry manages symbol mapping in STMapInfo.visit and friends. The sti slot is mainly
// intended for symbols that are explicitly mapped, using the corresponding form of the applyST directive.
// For a long time I used a map[*Symbol]symValue where symValue was a subset of symdictEntry but the map
// was difficult to work with in the debugger.
type symdictEntry struct {
	pre  *Symbol
	post Term // will almost always be *Symbol, but when Symchain root isn't Symbol, can be other
	sti  *Stitem
	next *symdictEntry // non-nil only if the map key needs to xlt in 2 diff ways (which can happen with ftns)
}

func (sde symdictEntry) String() string {
	return sde.pre.ident
}

// Symdict is a list of symdictEntry, see above for general ideas. This is binsearched on pre.ident; it's
// important to do a linear search on the string of entries having same pre.ident. The fact that we always
// do the full lg2 check in the index method means that it should always return the index of the first entry
// for which the condition is true.
type Symdict []symdictEntry

func (sd Symdict) index(ident string) int {
	if len(sd) == 0 || sd[len(sd)-1].pre.ident < ident {
		return -1
	}
	mn, mx := 0, len(sd)-1
	cur := mx / 2
	for mn <= mx {
		v := sd[cur].pre.ident
		if v < ident {
			mn = cur + 1
		} else {
			mx = cur - 1
		}
		cur = mn + (mx-mn)/2
	}
	return cur
}

// Look for a symdictEntry whose pre is == sym.
func (sd Symdict) find(sym *Symbol) *symdictEntry {
	inx := sd.index(sym.ident)
	if inx < 0 {
		return nil
	}
	for {
		if sd[inx].pre == sym {
			return &sd[inx]
		}
		inx++
		if inx == len(sd) || sd[inx].pre.ident != sym.ident {
			break
		}
	}
	return nil
}

// If there's an existing entry, add post and sti to it, else create a new entry
func (sd *Symdict) insert(sym *Symbol, post Term, sti *Stitem) *symdictEntry {
	inx := sd.index(sym.ident)
	var entry *symdictEntry
	nsd := *sd
	sdlen := len(*sd)
	if cap(*sd) == sdlen {
		nsd = make(Symdict, sdlen, sdlen*2)
		copy(nsd, *sd)
	}
	nsd = nsd[:sdlen+1]
	*sd = nsd
	if inx < 0 {
		entry = &nsd[sdlen]
		entry.pre = sym
	} else {
		copy(nsd[inx+1:sdlen+1], nsd[inx:sdlen])
		entry = &nsd[inx]
	}
	entry.pre = sym
	entry.post = post
	entry.sti = sti
	return entry
}

// Delete the entry for which sym is the pre
func (sd *Symdict) delete(sym *Symbol) {
	inx := sd.index(sym.ident)
	if inx >= 0 {
		nsd := *sd
		sdlen := len(nsd)
		copy(nsd[inx:sdlen-1], nsd[inx+1:sdlen])
		*sd = nsd[:sdlen-1]
	}
}

type infoELT struct {
	nm     string
	values []string
}

// This holds state info for the stmapping pass, which works in 2 possible modes. The usual one is a traversal
// of the entire query and its dependent ftns. The other is "method synthesis" mode which checks whether methods
// supplied with xltrules can translate other methods.
// Stmapping assumes that everything in the Term it gets can be smashed. New Symbols are created as needed for
// translation, and stored in symdict. The scopetree is updated while traversing so that applyItems can be
// added and deleted according to the language definition.
type STMapInfo struct {
	activeItems  []*applyItem // initially from gblscope but add+delete as scopes are traversed
	symdict      Symdict
	matchdicts   []*STMatchState // stack of matchstates for possibly nested xltrules under consideration
	scopetree    *Scope
	gblELTSlices []infoELT // hack for converting ELT to string
	rewriteStore *Scope    // here's the actual rewrite store
}

func makeSTMapInfo(gblscope *Scope) *STMapInfo {
	ais := make([]*applyItem, 0, len(gblscope.stActives))
	mds := make([]*STMatchState, 0, 3)
	ret := &STMapInfo{ais, make(Symdict, 0, 20), mds, gblscope, nil, makeScope()}
	ret.addApplyItems(gblscope)
	return ret
}

func (smi *STMapInfo) pushdict(mdict *STMatchState) {
	smi.matchdicts = append(smi.matchdicts, mdict)
}

func (smi *STMapInfo) popdict() {
	smi.matchdicts = smi.matchdicts[:len(smi.matchdicts)-1]
}

// Is sym a pattern symbol? If so, return its binding.
// We only look at the topmost STMatchState item on the stack
func (smi *STMapInfo) matchsym(sym *Symbol) Term {
	dictdepth := len(smi.matchdicts)
	if dictdepth == 0 {
		return nil
	}
	mst := smi.matchdicts[dictdepth-1]
	if sym == mst.selfvar {
		return mst.bindings[len(*mst.gesvars)]
	}
	// Note: for a long time this was s == sym; I weakened it to the current form to work around a problem with
	// rules that have a lhs GES of source type. To TC that needs to have target type on rhs but then the made up symbol
	// doesn't match in gesvars if using symbol equality. The sym.ident form is incorrect strictly speaking, but I decided
	// it was ok for the time being.
	for i, s := range *mst.gesvars {
		if s.ident == sym.ident {
			return mst.bindings[i]
		}
	}
	return nil
}

// ai is an active applyST item, add it to smi.
// if ai.wtag == SymbolTag add a partial entry to symdict that can be completed when the symbol is seen in a query
// the Symbol is used.
func (smi *STMapInfo) addApplyItems(scp *Scope) {
	for _, ai := range scp.stActives {
		if ai.wtag == SymbolTag {
			aisym := ai.what.(*Symbol)
			smi.symdict.insert(aisym, nil, ai.sti)
		} else {
			smi.activeItems = append(smi.activeItems, ai)
		}
	}
}

// This is just a helper for removeApplyItem. List deletion is ugly in go.
func (smi *STMapInfo) rmvAI(ai *applyItem) {
	inx := -1
	for i, aielt := range smi.activeItems {
		if ai == aielt {
			inx = i
			break
		}
	}
	if inx < 0 {
		panic("impossible")
	}
	last := len(smi.activeItems) - 1
	if inx < last {
		copy(smi.activeItems[inx:last], smi.activeItems[inx+1:last+1])
	}
	smi.activeItems = smi.activeItems[:last]
}

// Called when leaving a scope that added applyItems. So this assumes the ai was added.
func (smi *STMapInfo) removeApplyItems(scp *Scope) {
	for _, ai := range scp.stActives {
		if ai.wtag == SymbolTag {
			aisym := ai.what.(*Symbol)
			sv := smi.symdict.find(aisym)
			if sv.post == nil {
				smi.symdict.delete(aisym)
			}
		} else {
			smi.rmvAI(ai)
		}
	}
}

// funi is the non-trivial predicate a/w an applyST; trm is the term proposed as satisfying it.
// assume funi.body != nil.
func isManifestPredicate(scp *Scope, funi *Funinst, trm Term) bool {
	substn := []Term{funi.funargs[0], trm}
	var reqmts []Term
	if len(funi.body.args) > 1 {
		return false
	}
	done := 0
	satisfied := 0
	stmt := funi.body.args[0]
	if stmt.Tag() == AndandExpr {
		reqmts = stmt.(*TermL).args
		done = 1<<len(reqmts) - 1
	} else {
		reqmts = []Term{stmt}
		done = 1
	}
	for ; scp != nil; scp = scp.parent {
		for _, asrt := range scp.getAssertions() {
			for i, reqd := range reqmts {
				if eqUnderSubstn(asrt, reqd, substn) {
					satisfied |= 1 << i
					break
				}
			}
			if satisfied == done {
				return true
			}
		}
		if satisfied == done {
			return true
		}
	}
	return false
}

// Apply sti to trm. Should be called only if isActionpt returned ai on trm;
// Can return an error Term (if no matching xltrule) or replacement for trm.
func (smi *STMapInfo) apply(trm Term, sti *Stitem) Term {
	switch trm.Tag() {
	case SymbolTag:
		sym := trm.(*Symbol)
		symv := smi.symdict.find(sym)
		if symv == nil {
			symv = smi.symdict.insert(sym, nil, nil)
		}
		pstyp := sti.dest
		if symv.post == nil {
			var psbdg Term
			if sym.binding != nil {
				psbdg = smi.apply(sym.binding, sti)
				if psbdg.Tag() == ErrorTag {
					return psbdg
				}
				if !psbdg.Dtype().Equal(pstyp) {
					return genError(fmt.Sprint("cannot map", sym.ident, "to ", pstyp.String()), trm)
				}
			}
			sym0 := makeSymbol(sym.ident, pstyp, psbdg)
			sym0.plist = *sym.plist.Copy()
			symv = smi.symdict.find(sym) // symdict maybe modded, must re-acquire the entry
			symv.post = sym0
			symv.sti = sti
		}
		return symv.post
	case Funcall: // isActionpt already checked that ftn is a Symbol
		var asDirect = func(fnsym *Symbol, args []Term, rettype *Type, origterm *TermTL) Term {
			argsx := make([]Term, len(args))
			argsx[0] = smi.apply(args[0], sti)
			if argsx[0].Tag() == ErrorTag {
				return argsx[0]
			}
			for i := 1; i < len(args); i++ {
				argsx[i] = smi.visit(args[i])
				if argsx[i].Tag() == ErrorTag {
					return argsx[i]
				}
			}
			return makeFuncall(fnsym, argsx, rettype, origterm)
		}
		trm0 := trm.(*TermTL)
		fnsym := trm0.trm.(*Symbol)
		minx := sti.source.methodIndex(fnsym.ident)
		nmthds := len(sti.source.methods)
		found := false
		if minx >= 0 {
			var mthd *Symbol
			for ; minx < nmthds; minx++ {
				mthd = sti.source.methods[minx].sym
				if mthd == fnsym {
					found = true
					break
				}
				if mthd.ident != fnsym.ident {
					break
				}
			}
			if found && len(mthd.dtype.v.(*Ftntype).fmlargs) == len(trm0.args) {
				//fmt.Println("xltg", fnsym.ident)
				if !sti.xmethods.eltof(minx) {
					// here, attempt to synth a rhs version of ai.srctype.methods[minx]. if successful,
					// set xmethods and put the new "method" into funcallRhs where it'll be picked up
					srcmthd := sti.source.methods[minx].sym
					mthdbdg0 := srcmthd.binding.(*Funinst)
					mthdtypeL := srcmthd.dtype.v.(*Ftntype)
					argsr := make([]*Type, len(mthdtypeL.fmlargs))
					copy(argsr, mthdtypeL.fmlargs)
					argsr[0] = sti.rself.dtype
					for i, ty := range argsr[1:] {
						// Note: this should be a full traverse, not just a top level compare
						if ty == sti.source {
							argsr[i+1] = sti.dest
						}
					}
					rettr := mthdtypeL.rettype
					if rettr.Equal(mthdtypeL.fmlargs[0]) {
						rettr = sti.rself.dtype
					}
					mthdtypeR := makeType(TFFtn, argsr, rettr, mthdtypeL.pure)
					smi.symdict.insert(mthdbdg0.funargs[0], makeSymbol("self", argsr[0], nil), sti)
					changedIndices := make([]*Stitem, len(trm0.args)+1)
					changedIndices[0] = sti // only self arg changes
					changedIndices[len(trm0.args)] = sti
					mthdbdg1 := smi.rewriteFtn(mthdbdg0, mthdtypeR, changedIndices)
					if mthdbdg1.Tag() == ErrorTag {
						return mthdbdg1
					}
					// todo: fix obscure bug here if tgttype already has mthd named srcmthd.ident...
					sti.funcallRhs[minx] = makeSymbol(srcmthd.ident, mthdtypeR, mthdbdg1)
				}
				// it's a method in xmethods; check args
				xltruleLeft := sti.funcallArgs[minx]
				if xltruleLeft == nil {
					// it's direct, the tgt method is in funcallRhs; visit args and assemble
					rettype := trm0.dtype
					if rettype == sti.source {
						rettype = sti.dest
					}
					return asDirect(smi.visit(sti.funcallRhs[minx]).(*Symbol), trm0.args, rettype, trm0)
				} else {
					mstate := sti.makeSTMatchState()
					// process actuals both as to matching lhs (which binds GES to actuals) and wrt visiting or applying
					// skip the first arg in the actual, since it's self; offset the args in xltruleLeft
					for i := 1; i < len(trm0.args); i++ {
						a := trm0.args[i]
						if !mstate.msmatch(xltruleLeft[i-1], a) {
							errmsg := fmt.Sprintf("match failure in xltrule %s on %s", mthd.ident, trm0)
							return genError(errmsg, trm0)
						}
						a0 := smi.visit(a)
						if a0.Tag() == ErrorTag {
							return a0
						}
						for bdginx := 0; bdginx < len(mstate.bindings); bdginx++ {
							if mstate.bindings[bdginx] == a {
								mstate.bindings[bdginx] = a0
								break
							}
						}
					}
					// if we get here, the args all match, so create and return the output
					self0 := smi.apply(trm0.args[0], sti)
					//fmt.Println("in", fnsym.ident, "xltg self arg b4:", trm0.args[0].String(), ":", trm0.args[0].Dtype().String(), "; aftr: ",
					//	self0.String(), ":", self0.Dtype().String())
					smi.pushdict(mstate)
					mstate.bindings[len(*mstate.gesvars)] = self0
					// rationale for visiting rhs0.trm is to make a copy; this might not be needed
					rhs0 := sti.funcallRhs[minx].(*TermTL)
					trmx := smi.visit(rhs0.trm)
					argsx := make([]Term, len(rhs0.args))
					argsx[0] = self0
					for i, a := range rhs0.args[1:] {
						argsx[i+1] = smi.visit(a)
					}
					rhsx := makeFuncall(trmx, argsx, rhs0.trm.Dtype().v.(*Ftntype).rettype, nil)
					smi.popdict()
					return rhsx
				}
			}
		}
		// if we get here, it's not a method. Maybe it's a ftn we can translate
		maybeGnc, _ := smi.scopetree.Lookup(fnsym.ident)
		// todo: above assumes single symbol; make multibinding work
		if maybeGnc != nil {
			gscope := maybeGnc.plist.Find("givenscope")
			if gscope != nil {
				gscope0 := gscope.(*Scope)
				abst := make([]*Symbol, len(gscope0.entries))
				for i, sx := range gscope0.entries {
					abst[i] = sx.sym
				}
				// I'm going to punt here: only work for single GTS that's assumed to match S, now substg T
				if len(abst) == 1 && abst[0].dtype == nil && abst[0].binding.(*Type).family == TFGTS {
					conc := []Term{sti.dest}
					sym0 := maybeGnc.cachedGeneric(conc)
					if sym0 == nil {
						czi := makeConczInfo(abst, conc)
						czd := czi.concretize(maybeGnc)
						sym0 = czd.(*Symbol)
						maybeGnc.plist.Accum("cachedInstances", append(conc, sym0))
					}
					// at this point, we've got the ftn symbol; create the appropriate funcall
					// We need to change the rcvr and copy the other actuals, same as direct xltrule
					rettype := trm0.dtype
					if rettype == sti.source {
						rettype = sti.dest
					}
					return asDirect(sym0, trm0.args, rettype, trm0)
				}
			} else {
				// assume only the rettype changes
				// todo: fix this
				otyp := fnsym.dtype.v.(*Ftntype)
				ntyp := makeType(TFFtn, otyp.fmlargs, sti.dest, otyp.pure)
				nargs := len(otyp.fmlargs)
				changes := make([]*Stitem, nargs+1)
				changes[nargs] = sti
				nfn := smi.rewriteFtn(fnsym, ntyp, changes)
				return makeFuncall(nfn, trm0.args, sti.dest, trm)
			}
		}
		return genError("can't translate call to "+fnsym.ident, trm0)
	case EachStmt:
		// post-TC each stmt has 3 args: itervar, collection, body; here the collection is an actionpt
		trm0 := trm.(*TermL)
		itervar := trm0.args[0].(*Symbol)
		coll := trm0.args[1]
		rcoll := smi.apply(coll, sti)
		if rcoll.Tag() == ErrorTag {
			return rcoll
		}
		etyp := rcoll.Dtype().Elttype() // cannot be nil as enum'ble was checked in TC
		// now etyp is the new type of itervar
		xitervar := makeSymbol(itervar.ident, etyp, nil)
		smi.symdict.insert(itervar, xitervar, sti)
		nbody := smi.visit(trm0.args[2])
		escope := makeScope()
		escope.Add(itervar, 0)
		escope.parent = nbody.(*TermL).scope.parent
		nbody.(*TermL).scope.parent = escope
		return makeScopedTermL(EachStmt, escope, []Term{xitervar, rcoll, nbody}, TypeNothing, trm0.first, trm0.final)
	case Symchain:
		trm0 := trm.(*TermL)
		root := trm0.args[0]
		xroot := smi.apply(root, sti)
		if xroot.Tag() == ErrorTag {
			return xroot
		}
		xltd := false
		var xrhs Term
		for i, atrb := range sti.attribs {
			if atrb == trm0.args[1] {
				// following causes xltd lhs self to subst for rhs "self" symbol when visiting rhs
				smi.symdict.insert(sti.rself, xroot, nil)
				xrhs = smi.apply(sti.attribRhs[i], sti)
				smi.symdict.delete(sti.rself)
				if xrhs.Tag() == ErrorTag {
					return xrhs
				}
				xltd = true
				break
			}
		}
		if xltd {
			if len(trm0.args) == 2 {
				return xrhs
			} else {
				// here, we need to check the xltd result fits into the chain
				panic("not implemented")
			}
		}
	case Gdref: // apply a mapcast
		trm0 := trm.(*TermT)
		// this handles mapcast symbols but I'm not sure it covers all cases correctly.
		if trm0.arg0.Tag() == SymbolTag {
			sym := trm0.arg0.(*Symbol)
			matchbdg := smi.matchsym(sym)
			if matchbdg != nil {
				return matchbdg // smi.visit(matchbdg)
			}
		}
		return smi.apply(trm0.arg0, sti)
	case ZeroValue:
		return makeTermT(ZeroValue, nil, sti.dest, trm.First(), trm.Final())
	case Stmts: // can be actionpt by virtue of dtype; visit and check
		// ignore return stmts for now
		trm0 := trm.(*TermL)
		body0 := make([]Term, len(trm0.args))
		for i, s := range trm0.args {
			if i == len(trm0.args)-1 {
				body0[i] = smi.apply(s, sti)
			} else {
				body0[i] = smi.visit(s)
			}
			if body0[i].Tag() == ErrorTag {
				return body0[i]
			}
		}
		lclscope := makeScope()
		return makeScopedTermL(Stmts, lclscope, body0, sti.dest, trm0.first, trm0.final)
	}
	return genError(fmt.Sprint("cannot map ", trm.String(), " according to rule ", sti.String()), trm)
}

// Check all the conditions that make trm an actionpt.
// If one obtains, return the *Stitem that makes it so, else nil.
func (smi *STMapInfo) isActionpt(trm Term) *Stitem {
	// try type-based first
	// Note: there is probably a missing case here for types exported from modules that have a type based applyST
	// todo: take care of this
	for _, ai := range smi.activeItems {
		sti0 := ai.matchTerm(trm, smi.scopetree)
		if sti0 != nil {
			return sti0
		}
	}
	// then symbols and others
	trmtg := trm.Tag()
	if trmtg == SymbolTag {
		trm0 := trm.(*Symbol)
		sv := smi.symdict.find(trm0)
		if sv != nil {
			return sv.sti
		}
		qisym0 := trm0.plist.Find("qisym")
		if qisym0 != nil {
			for _, ai := range qisym0.(*QIVal).scope.stActives {
				if ai.wtag == SymbolTag && ai.what == trm0 {
					return ai.sti
				}
			}
		}
	} else if trmtg == Funcall {
		trm0 := trm.(*TermTL)
		if trm0.trm.Tag() == SymbolTag && len(trm0.args) > 0 {
			rcvr := trm0.args[0]
			ai := smi.isActionpt(rcvr)
			rcvrtag := rcvr.Tag()
			if (rcvrtag == SymbolTag || rcvrtag == Symchain) && ai != nil {
				rtyp := rcvr.Dtype()
				ftnsym := trm0.trm.(*Symbol)
				msym := rtyp.methodLookup(ftnsym.ident)
				if msym != nil && msym == ftnsym {
					return ai
				}
			}
		}
	} else if trmtg == Gdref {
		trmai := trm.Plist().Find("applyitem")
		if trmai != nil {
			return trmai.(*applyItem).sti
		}
	} else if trmtg == Symchain {
		root := trm.(*TermL).args[0]
		if root.Tag() == SymbolTag {
			return smi.isActionpt(root)
		}
	} else if trmtg == EachStmt {
		return smi.isActionpt(trm.(*TermL).args[1])
	}
	return nil
}

// Change the dtype of trm to ntyp.
// I'm not sure this is a good idea but I'm trying it out.
// Probably ntyp will only ever be TypeNothing
func smashDtype(trm Term, ntyp *Type) {
	switch trm0 := trm.(type) {
	default:
		panic("shouldn't happen")
	case *Symbol:
		// do nothing here, symbol will be deleted via other machinery and it's dtype shouldn't be smashed.
	case *TermB:
		// Tricky bit: dtypes of TermB's cannot be smashed but may be same as ntyp
		otyp := trm0.Dtype()
		if !(otyp.Equal(ntyp) || ntyp == TypeNothing) {
			panic("shouldn't happen")
		}
	case *TermT:
		trm0.dtype = ntyp
	case *TermTT:
		trm0.dtype = ntyp
	case *TermL:
		trm0.dtype = ntyp
		if trm0.kind == IfStmt && len(trm0.args) == 3 {
			smashDtype(trm0.args[1], ntyp)
			smashDtype(trm0.args[2], ntyp)
		} else if trm0.kind == Stmts && len(trm0.args) > 0 {
			laststmt := trm0.args[len(trm0.args)-1]
			if laststmt.Tag() != ReturnStmt {
				smashDtype(laststmt, ntyp)
			}
		}
	case *TermTL:
		trm0.dtype = ntyp
	}
}

// trm will be a ftn-valued Symbol or Funinst. Attempt to rewrite it to
// have newtyp, which will be a ftn type. Return the new Term or an error Term.
// If only argtypes are changing, there's a possibility that we only need to rewrite
// according to the enumonly rule, in which case we don't actually have to rewrite the ftn,
// just the enumonly arg (or rarely, args).
// The sti arg can be nil, in which case we'll look for a xltn
func (smi *STMapInfo) rewriteFtn(trm Term, newtyp *Type, changeItems []*Stitem) Term {
	var funi *Funinst
	if trm.Tag() == SymbolTag {
		// This is kinda ugly, I need to come up with something more elegant
		funi = trm.(*Symbol).binding.(*Funinst)
	} else {
		funi = trm.(*Funinst)
	}
	if funi.body == nil {
		return genError("cannot rewrite built in ftn", trm)
	}
	ftypnew := newtyp.v.(*Ftntype)
	// here, we're changing only args: put them into symdict with xltns then visit the body
	fmlargsx := make([]*Symbol, len(funi.funargs))
	for i, fa := range funi.funargs {
		symv := smi.symdict.find(fa)
		var psym *Symbol
		if symv != nil {
			psym = symv.post.(*Symbol)
		} else {
			psym = makeSymbol(fa.ident, ftypnew.fmlargs[i], nil)
			smi.symdict.insert(fa, psym, changeItems[i])
		}
		fmlargsx[i] = psym
	}
	if !ftypnew.rettype.Equal(funi.dtype.v.(*Ftntype).rettype) {
		stmts := funi.body.args
		retsym := stmts[len(stmts)-1]
		if retsym.Tag() == SymbolTag {
			rs0 := retsym.(*Symbol)
			if rs0 != funi.funargs[0] {
				rs1 := makeSymbol(rs0.ident, ftypnew.rettype, nil)
				smi.symdict.insert(rs0, rs1, changeItems[len(funi.funargs)])
			}
		}
	}

	body0 := smi.visit(funi.body)
	if body0.Tag() == ErrorTag {
		return body0
	}
	nscope := makeScope()
	for _, s := range fmlargsx {
		nscope.Add(s, 0)
	}
	nscope.parent = funi.scope.parent
	body0.(*TermL).scope.parent = nscope
	nfuni := makeFuninst(nscope, fmlargsx, newtyp, body0.(*TermL))
	nfuni.isLocalfn = funi.isLocalfn
	if trm.Tag() == SymbolTag {
		trm0 := trm.(*Symbol)
		nsym := makeSymbol(trm0.ident, newtyp, nfuni)
		nsym.plist = *trm0.plist.Copy()
		//fmt.Println("symdict [rw ftn sym] copies ftn sym ", trm0.ident, ":", trm0.dtype.String())
		smi.symdict.insert(trm0, nsym, nil)
		return nsym
	}
	return nfuni
}

// If we know a default value for typ, return it as a Term, else return nil.
func defaultValue(typ *Type, seen *[]*Type) (retval Term) {

	switch typ.family {
	case TFInt, TFByte:
		retval = makeIntlit(0)
	case TFFloat:
		retval = makeFloatTerm("0.0", Pos(-1))
	case TFString:
		retval = makeStringTerm("", Pos(-1), false)
	case TFList:
		retval = makeTermL(Litform0, []Term{}, typ, Pos(-1), Pos(-1))
	case TFBool:
		retval = FalseLiteral
	case TFOrtype:
		if typ.isNilposs() {
			return nilEntity
		}
	case TFSpace:
		if typ == biScope["bits64"][0].binding {
			retval = makeIntlit(0)
		}
	case TFTuple:
		// was zerovalue. Avoid infloops on recursive types
		for _, st := range *seen {
			if st == typ {
				return nilEntity
			}
		}
		*seen = append(*seen, typ)
		attribs := typ.v.(*Tupletype).attribs
		attvals := make([]Term, len(attribs))
		for i, a := range attribs {
			attvals[i] = defaultValue(a.dtype, seen)
		}
		retval = makeTermL(Litform0, attvals, typ, Pos(-1), Pos(-1))
	case TFFtn:
		retval = nilEntity // is this the right thing?
	}
	return
}

// Another visit helper analyzes a lithook funcall, rewriting to a litform0 if there are only asgnmts to attribs
// and no calls to methods of self. If there are in addition some local defs and intermediate calcs, leave as a fn.
func (smi *STMapInfo) lithookXfm(trm *TermTL) Term {
	// this is used for the rewrite to litform0 case.
	var assembleValues = func(bodyargs, attribVals []Term, attribs []*Symbol, asgns []int) {
		for i, a := range attribs {
			filled := false
			for _, ainx := range asgns {
				aa := bodyargs[ainx].(*TermTT).arg0.(*TermL).args[1].(*Symbol) // get the attrib
				if aa.ident == a.ident {
					attribVals[i] = smi.visit(bodyargs[ainx].(*TermTT).arg1)
					filled = true
					break
				}
			}
			if !filled {
				// no explicit value for this attrib; do I know the default value?
				seen := make([]*Type, 2)
				tmp := defaultValue(a.dtype, &seen)
				if tmp == nil {
					panic("don't know how to fill slot for " + attribs[i].ident)
				}
				attribVals[i] = tmp
			}
		}
	}
	fnsym := trm.trm.(*Symbol)
	if fnsym.binding == nil {
		return trm // this can happen with Gomap
	}
	lhfuni := fnsym.binding.(*Funinst)
	selfsym := lhfuni.funargs[0]
	nstmts := len(lhfuni.body.args) - 1 // the last stmt returns self, cut it off
	// go through the body and collect assignments into asgns
	asgns := make([]int, 0, 2)
	lhsLocalRef := func(trm Term) bool {
		return trm.Tag() == Symchain && trm.(*TermL).args[0].(*Symbol) == selfsym && len(trm.(*TermL).args) == 2
	}
	for i, a := range lhfuni.body.args[:nstmts] {
		if a.Tag() == AsgnStmt {
			a0 := a.(*TermTT).arg0
			a1 := a.(*TermTT).arg1
			if lhsLocalRef(a0) && !TraversePre(a1, identWorkfn, selfsym) {
				asgns = append(asgns, i)
			}
		}
	}
	attribs := selfsym.dtype.v.(*Tupletype).attribs
	attribValues := make([]Term, len(attribs))
	if len(asgns) == nstmts {
		for i, fa := range lhfuni.funargs[1:] {
			//fmt.Println("symdict [lithook xfm] copies funarg ", fa.ident, ":", fa.dtype.String())
			smi.symdict.insert(fa, smi.visit(trm.args[i+1]), nil)
		}
		// here for Litform0
		// for now, assume there are no order cntts among the asgnmt stmts
		// todo: add code to detect and accomodate order cntts
		// todo: unresolved issue if one of the args uses self, eg to access a method
		assembleValues(lhfuni.body.args, attribValues, attribs, asgns)
		// at this point, attribValues is a proper arg to a Litform0
		return makeTermL(Litform0, attribValues, selfsym.dtype, Pos(-1), Pos(-1))
	}
	return trm
}

// A visit helper used to extract from a term the stitem that causes it to change.
// Should only be called on a term that did translate.
// I'm not super confident about best way to do this; I'm trying the first non-nil return from
// isActionpt.
func (smi *STMapInfo) findStitem(trm Term) (retval *Stitem) {
	if trm == nil {
		return
	}
	ap := smi.isActionpt(trm)
	if ap != nil {
		retval = ap
	} else {
		// note: symbols has to be actionpt else fail; literals fail automatically.
		// hence, no cases for *Symbol or *TermB.
		// I am pretty sure *Funinst can't happen either
		switch trm0 := trm.(type) {
		case *TermT:
			retval = smi.findStitem(trm0.arg0)
		case *TermTT:
			retval = smi.findStitem(trm0.arg0)
			if retval == nil {
				retval = smi.findStitem(trm0.arg1)
			}
		case *TermL:
			if trm0.kind == Symchain {
				retval = smi.findStitem(trm0.args[0])
			} else if trm0.kind == Stmts {
				// can only be from final stmt (??)
				retval = smi.findStitem(trm0.args[len(trm0.args)-1])
			} else {
				for _, x := range trm0.args {
					retval = smi.findStitem(x)
					if retval != nil {
						break
					}
				}
			}
		case *TermTL:
			for _, x := range trm0.args {
				retval = smi.findStitem(x)
				if retval != nil {
					return
				}
			}
			retval = smi.findStitem(trm0.trm)
		}
	}
	return
}

func flushWhenCompiling(trm Term) bool {
	tg := trm.Tag()
	return tg == AssertStmt || tg == ImportStmt || tg == ImportPkgStmt || tg == OncondStmt || callingNamed(trm, "applyST")
}

// This is the main function for traversing the term we're stmapping.
// If the incoming term is an actionpt, handle it. If not, use the tag to guide recursive calls to visit on the parts.
// Return the resulting Term, which will generally be a possibly modified copy of the incoming one.
func (smi *STMapInfo) visit(trm Term) Term {
	if trm == nil {
		return nil
	}
	//fmt.Println("visiting ", trm.String())
	sti := smi.isActionpt(trm)
	if sti != nil {
		// In theory xltg at an actionpt can return an actionpt of a diff rule. Hence the loop.
		// The loopcount thing is purely to prevent infloops. In principle it's not needed because they shouldn't happen.
		loopcount := 0
		for ; loopcount < 50; loopcount++ {
			tmp := smi.apply(trm, sti)
			if tmp.Tag() == ErrorTag {
				return tmp
			}
			prevtyp := trm.Dtype()
			newtyp := tmp.Dtype()
			if prevtyp == TypeNothing && newtyp != TypeNothing {
				smashDtype(tmp, TypeNothing)
				newtyp = TypeNothing
			}
			if newtyp.Equal(prevtyp) {
				return tmp
			}
			sti = smi.isActionpt(tmp)
			if sti == nil {
				return tmp
			}
			trm = tmp
		}
		if loopcount == 50 {
			panic("got a problem here")
		}
		return nil
	} else {
		// here for std visit actions
		tg := trm.Tag()
		switch tg {
		case Funcall:
			trm0 := trm.(*TermTL)
			// first, check for the special case of a "funcall" to applyST
			if callingNamed(trm0, "applyST") {
				return trm0 // we're done with this stmt
			}
			// next, lithook gets special treatment at this time (given that it's not going to be stmapped)
			if callingNamed(trm0, "lithook") {
				trm1 := smi.lithookXfm(trm0)
				if trm1.Tag() == Litform0 {
					return trm1
				}
				// if not changed, nothing has been done
			}
			// also check for the case that an ELT is cvtd to string
			if callingNamed(trm0, "autocvt") && trm0.dtype == TypeString && trm0.args[0].Dtype().family == TFInt {
				argt := trm0.args[0].Dtype()
				elt := argt.plist.Find("ELT")
				if elt != nil && argt.plist.Find("infoELT") == nil {
					gnsym := gensym()
					smi.gblELTSlices = append(smi.gblELTSlices, infoELT{gnsym, elt.([]string)})
					argt.plist.Add("infoELT", gnsym)
				}
			}
			// otherwise, process the args, the ftn, and either attempt to rewrite the ftn or just make a copy
			changedIndices := make([]*Stitem, len(trm0.args)+1)
			argsx := make([]Term, len(trm0.args))
			ftypesx := make([]*Type, len(trm0.args))
			rettype := trm0.dtype
			changecount := 0
			for i, a := range trm0.args {
				tmp := smi.visit(a)
				typt := tmp.Dtype()
				if !typt.Equal(a.Dtype()) {
					changedIndices[i] = smi.findStitem(a)
					changecount++
				}
				ftypesx[i] = typt
				argsx[i] = tmp
			}
			xftn := trm0.trm
			var xfuntyp *Type
			sameTypes := changecount == 0 && rettype.Equal(trm0.dtype)
			specialFtns := trm0.trm.Tag() == SymbolTag && trm0.trm.(*Symbol).binding == nil
			if !(sameTypes || specialFtns) {
				xfuntyp = makeType(TFFtn, ftypesx, rettype, xftn.Dtype().v.(*Ftntype).pure)
				xftn = smi.rewriteFtn(xftn, xfuntyp, changedIndices)
			} else {
				xftn = smi.visit(xftn)
			}
			if xftn.Tag() == ErrorTag {
				return xftn
			}
			return makeFuncall(xftn, argsx, rettype, trm0)
		case SymbolTag: // we can have a distinct reqdtype but this is not an actionpt
			origsym := trm.(*Symbol)
			matchbdg := smi.matchsym(origsym)
			if matchbdg != nil {
				return matchbdg
			}
			// ignore type-valued defns for now
			if origsym.dtype != nil {
				ntyp := origsym.dtype
				sv := smi.symdict.find(origsym)
				if sv == nil { // not previously seen
					nsym := makeSymbol(origsym.ident, ntyp, nil)
					nsym.plist = *origsym.plist.Copy()
					// do this now, else recursive functions infloop
					//fmt.Println("symdict [visit symbol] gets copy of ", origsym.ident, ":", origsym.dtype.String())
					smi.symdict.insert(origsym, nsym, nil)
					var nbdg Term
					if origsym.binding != nil {
						// todo: enter funarg symbols into scope??
						nbdg = smi.visit(origsym.binding)
						if nbdg.Tag() == ErrorTag {
							return nbdg
						}
					}
					nsym.binding = nbdg
					return nsym
				} else {
					// here, we've seen origsym one or more times and may have mapped it >1 time
					cursv := sv
					for ; cursv != nil; cursv = cursv.next {
						seen := []*Type{}
						if cursv.post.Dtype().compat(ntyp, &seen) {
							//fmt.Println("symdict [visit symbol] returns copy of ", origsym.ident, ":", origsym.dtype.String())
							return cursv.post
						}
					}
					// we haven't got a postsym whose type matches what we need. If it's a ftn try rewriting, else give up
					if ntyp.family == TFFtn {
						//fmt.Println("symdict [visit symbol] copies ftn ", origsym.ident, ":", origsym.dtype.String())
						tmp := smi.visit(origsym.binding)
						if tmp != nil && tmp.Tag() == ErrorTag {
							return tmp
						}
						nsym := makeSymbol(origsym.ident, ntyp, tmp)
						nsym.plist = *origsym.plist.Copy()
						sv1 := &symdictEntry{origsym, nsym, nil, nil}
						sv1.next = sv.next
						sv.next = sv1
						return nsym
					} else {
						errmsg := fmt.Sprintf("cannot find or create symbol %s with type %s", origsym.ident, ntyp.String())
						return genError(errmsg, trm)
					}
				}
			}
		case Valdecl, Vardecl, Fwddecl:
			trm0 := trm.(*TermT)
			nsym := trm0.arg0.(*Symbol)
			//  check if we've already copied this symbol in the Fwddecl stmt; don't do it again
			if !(trm0.kind == Valdecl && nsym.plist.Find("fwddecld") != nil) {
				trm1 := smi.visit(nsym)
				if trm1.Tag() == ErrorTag {
					return trm1
				}
				nsym = trm1.(*Symbol)
				// ensure a default binding
				if nsym.binding == nil {
					nsym.binding = makeTermT(ZeroValue, nil, nsym.dtype, Pos(-1), Pos(-1))
				}
				// Note that upcount might be wrong here, but at the moment, it's not used properly anyway
				smi.scopetree.Add(nsym, 0)
			} else {
				sv := smi.symdict.find(nsym)
				nsym = sv.post.(*Symbol)
			}
			trm = makeTermT(trm0.kind, nsym, trm0.dtype, trm0.first, trm0.final)
		case Symchain:
			trm0 := trm.(*TermL)
			root := trm0.args[0]
			root0 := smi.visit(root)
			if root.Dtype().Equal(root0.Dtype()) {
				nargs := make([]Term, len(trm0.args))
				nargs[0] = root0
				for i := 1; i < len(trm0.args); i++ {
					nargs[i] = trm0.args[i]
				}
				return makeTermL(Symchain, nargs, trm0.dtype, trm0.first, trm0.final)
			} else {
				errmsg := fmt.Sprintf("cannot use attribute %s when stmap changed root type", trm0.args[1].String())
				return genError(errmsg, trm)
			}
		case FuninstTag:
			trm0 := trm.(*Funinst)
			if trm0.body == nil {
				// This is either an obscure thing that'll be asserted manifest or (much more likely) a
				// flavor of BI ftn (not labeled as "biFtn") that's going to be handled manually by codegen.
				// Even so, make a copy in case plist might be used in backend.
				nfuni := makeFuninst(trm0.scope, trm0.funargs, trm0.dtype, nil)
				nfuni.isLocalfn = trm0.isLocalfn
				nfuni.plist = *trm0.plist.Copy()
				return nfuni
			}
			// Ok, we have a std UD ftn.
			// copy funargs and push trm0.scope onto scopetree stack before visiting the body
			fmlargsx := make([]*Symbol, len(trm0.funargs))
			nscope := makeScope()
			for i, fa := range trm0.funargs {
				nsym := makeSymbol(fa.ident, fa.dtype, nil)
				nsym.plist = *fa.plist.Copy()
				//fmt.Println("symdict [funinst args] creates ", nsym.ident, ":", nsym.dtype.String())
				smi.symdict.insert(fa, nsym, nil)
				fmlargsx[i] = nsym
				nscope.Add(nsym, 0)
			}
			nscope.parent = smi.scopetree
			smi.scopetree = nscope
			nbody := smi.visit(trm0.body)
			if nbody.Tag() == ErrorTag {
				return nbody
			}
			// pop trm0.scope
			smi.scopetree = smi.scopetree.parent
			nbody0 := nbody.(*TermL)
			nfuni := makeFuninst(nscope, fmlargsx, trm0.dtype, nbody0)
			nfuni.isLocalfn = trm0.isLocalfn
			nfuni.plist = trm0.plist
			return nfuni
		case Stmts:
			trm0 := trm.(*TermL)
			var nscope *Scope
			if trm0.scope != nil {
				smi.addApplyItems(trm0.scope)
				nscope = trm0.scope.Copy(false)
				nscope.parent = smi.scopetree
				smi.scopetree = nscope
			}
			argsx := make([]Term, 0, len(trm0.args))
			for _, a := range trm0.args {
				tmp := smi.visit(a)
				if tmp.Tag() == ErrorTag {
					return tmp
				}
				if !flushWhenCompiling(tmp) {
					argsx = append(argsx, tmp)
				}
			}
			if trm0.scope != nil {
				smi.removeApplyItems(trm0.scope)
				smi.scopetree = smi.scopetree.parent
			}
			lastinx := len(argsx) - 1
			var rettype *Type
			if lastinx >= 0 {
				rettype = argsx[lastinx].Dtype()
				if trm0.dtype == TypeNothing {
					rettype = TypeNothing
				}
			}
			trm1 := makeScopedTermL(Stmts, nscope, argsx, rettype, trm0.first, trm0.final)
			trm = trm1
		case GivenStmt:
			// just the body. BTW, this should be rare in a query...
			trm0 := trm.(*TermTL)
			tmp := smi.visit(trm0.trm)
			trm = makeTermTL(GivenStmt, tmp, trm0.args, TypeNothing, trm0.first, trm0.final)
		default:
			// This case handles recursive visits for tags that don't get special treatment.
			// This part is based on concrete type.
			switch trm0 := trm.(type) {
			default:
				return trm
			case *TermT:
				if trm0.kind == AssertStmt {
					return trm // don't copy since this is about to be flushed
				}
				a0x := trm0.arg0
				if a0x != nil {
					a0x = smi.visit(a0x)
					if a0x.Tag() == ErrorTag {
						return a0x
					}
				}
				trm = makeTermT(trm0.kind, a0x, trm0.dtype, trm0.first, trm0.final)
			case *TermTT:
				a0x := trm0.arg0
				if a0x != nil {
					a0x = smi.visit(a0x)
					if a0x.Tag() == ErrorTag {
						return a0x
					}
				}
				a1x := trm0.arg1
				if a1x != nil {
					a1x = smi.visit(a1x)
					if a1x.Tag() == ErrorTag {
						return a1x
					}
				}
				trm = makeTermTT(trm0.kind, a0x, a1x, trm0.dtype, trm0.first, trm0.final)
			case *TermL:
				// Among others, this handles EachStmt, which has a scope; no binding since it's just the itervar
				var nscope *Scope
				if trm0.scope != nil {
					nscope = makeScope()
					for _, lsx := range trm0.scope.entries {
						nsym := makeSymbol(lsx.sym.ident, lsx.sym.dtype, nil)
						nscope.Add(nsym, lsx.xcount)
						//fmt.Println("symdict [TermL ", trm0.kind, "] copies scope entry ", nsym.ident, ":", nsym.dtype.String())
						smi.symdict.insert(lsx.sym, nsym, nil)
					}
					nscope.parent = smi.scopetree
					smi.scopetree = nscope
				}
				argsx := make([]Term, len(trm0.args))
				for i, a := range trm0.args {
					argsx[i] = smi.visit(a)
					if argsx[i].Tag() == ErrorTag {
						return argsx[i]
					}
				}
				if trm0.scope != nil {
					smi.scopetree = smi.scopetree.parent
				}
				trm = makeScopedTermL(trm0.kind, nscope, argsx, trm0.dtype, trm0.first, trm0.final)
			case *TermTL:
				argsx := make([]Term, len(trm0.args))
				for i, a := range trm0.args {
					argsx[i] = smi.visit(a)
					if argsx[i].Tag() == ErrorTag {
						return argsx[i]
					}
				}
				trmx := smi.visit(trm0.trm)
				if trmx.Tag() == ErrorTag {
					return trmx
				}
				trm = makeTermTL(trm0.kind, trmx, argsx, trm0.dtype, trm0.first, trm0.final)
			}
		}
		return trm
	}
}

// matching for rewrite rule engine and xltrules. The STMapInfo stores one per level of nested funcalls;
// these can arise from diff stitems.
type STMatchState struct {
	gesvars  *symbolSlice // direct from STI which derives from given stmt; can be nil
	selfvar  *Symbol      // also from STI
	bindings []Term       // room for len(gesvars)+1 bindings, selfvar binding is at end
}

// Returns true and updates mst if gtrm matches ptrm exactly. P and G stand for pattern and ground resp. Pattern is
// from the xltrule and will typically be a symbol but isn't required to be. Ground is an actual arg to a funcall,
// often the method corresp to lhs method. So it will commonly be a symbol too. When
func (mst *STMatchState) msmatch(ptrm, gtrm Term) bool {
	ptg := ptrm.Tag()
	gtg := gtrm.Tag()
	if ptg == SymbolTag {
		psym := ptrm.(*Symbol)
		psyminx := mst.gesvars.findsym(psym)
		if ptrm == mst.selfvar {
			psyminx = len(*mst.gesvars) // set to index of binding for self
		}
		if psyminx >= 0 {
			if mst.bindings[psyminx] != nil {
				return gtrm.Equal(mst.bindings[psyminx])
			}
			mst.bindings[psyminx] = gtrm
			return true
		} else {
			return ptrm.Equal(gtrm)
		}
	}
	// here, ptrm is not a Symbol; match is if same tags and matching parts
	if ptg != gtg {
		return false
	}
	switch pt0 := ptrm.(type) {
	case *TermT:
		gt0 := gtrm.(*TermT)
		if pt0.arg0 == nil {
			return gt0.arg0 == nil
		}
		return mst.msmatch(pt0.arg0, gt0.arg0)
	case *TermTT:
		gt0 := gtrm.(*TermTT)
		a0 := false
		if pt0.arg0 == nil {
			a0 = gt0.arg0 == nil
		} else {
			a0 = mst.msmatch(pt0.arg0, gt0.arg0)
		}
		if a0 {
			if pt0.arg1 == nil {
				return gt0.arg1 == nil
			} else {
				return mst.msmatch(pt0.arg1, gt0.arg1)
			}
		}
	case *TermL:
		gt0 := gtrm.(*TermL)
		for i, a := range pt0.args {
			if !mst.msmatch(a, gt0.args[i]) {
				return false
			}
		}
		return true
	case *TermTL:
		gt0 := gtrm.(*TermTL)
		for i, a := range pt0.args {
			if !mst.msmatch(a, gt0.args[i]) {
				return false
			}
		}
		return mst.msmatch(pt0.trm, gt0.trm)
	case *Funinst:
		panic("write me")
	default:
		return pt0.Equal(gtrm)
	}
	return false
}
