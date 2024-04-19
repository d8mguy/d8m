// Symbolic operations, starting with simplify and manifest

package main

import (
	"fmt"
)

// TraversePre is a preorder traversal ftn. Its workfn returns an integer interpd as (1) if -1,
// a directive to bail (return immediately); (2) if >= 0 a bitset specifying
// that 0, 1, or 2 of the parts of the term should *not* be traversed. (So 0 means traverse all.)
// Return true if bailing early.
func TraversePre(trm Term, workfn func(trm Term, stvar interface{}) int, state interface{}) bool {
	if trm == nil {
		return false
	}
	descend := workfn(trm, state)
	if descend < 0 {
		return true
	}
	switch trm0 := trm.(type) {
	case *Symbol: // 0 or 1 cntl binding
		if descend == 0 && trm0.binding != nil {
			if TraversePre(trm0.binding, workfn, state) {
				return true
			}
		}
	case *Funinst: // 0 or 1 cntl body
		if descend == 0 && trm0.plist.Find("biFtn") == nil && trm0.body != nil {
			if TraversePre(trm0.body, workfn, state) {
				return true
			}
		}
	case *TermT: // 0 or 1 cntl arg0
		if descend == 0 && trm0.arg0 != nil {
			if TraversePre(trm0.arg0, workfn, state) {
				return true
			}
		}
	case *TermL: // 0 or 1 cntl args
		if descend == 0 {
			for _, a := range trm0.args {
				if a != nil {
					if TraversePre(a, workfn, state) {
						return true
					}
				}
			}
		}
	case *TermTT: // 0-3 cntl arg0, arg1
		if (descend&1) == 0 && trm0.arg0 != nil {
			if TraversePre(trm0.arg0, workfn, state) {
				return true
			}
		}
		if (descend&2) == 0 && trm0.arg1 != nil {
			if TraversePre(trm0.arg1, workfn, state) {
				return true
			}
		}
	case *TermTL: // 0 or 1 cntl trm; 0 or 2 cntl args
		if (descend&1) == 0 && trm0.trm != nil {
			if TraversePre(trm0.trm, workfn, state) {
				return true
			}
		}
		if (descend & 2) == 0 {
			for _, a := range trm0.args {
				if a != nil {
					if TraversePre(a, workfn, state) {
						return true
					}
				}
			}
		}
	}
	return false
}

// A struct to manage the current state of "defined onconds". These are oncondition stmts whose conditions
// consist of 1 or more defined(...) clauses. These are manifest and are intended to simplify dependency
// ordering for rules and assertions about relns among types, ftns, assertions, rwrules, and so on.

// Here, a single defined-oncond
type defCondElement struct {
	idents  []string // the idents in a list of && clauses
	cntts   []*Type  // pll array, nil corresp to "type" at source level
	defined Bitset   // track which idents are defined, trigger when all are
	body    Term     // what should be run when this defCondElement triggers
}

// Here, global level info
type defCondState struct {
	defConds []*defCondElement
}

var defConds = defCondState{nil}

// Specialized for the defined predicate; search for ident in the scope tree plus all active modules
// Return true if the search finds any type or typegen with the given identifier.
func dfcSearchTypes(ident string, scp *Scope) bool {
	inscope := scp.lookupType(ident) != nil || scp.lookupTypegen(ident) != nil
	if !inscope {
		for _, qiv := range QIScope {
			if qiv.activeCount > 0 && qiv.isModule && qiv.mname != "" {
				inscope = qiv.scope.lookupType(ident) != nil || qiv.scope.lookupTypegen(ident) != nil
				if inscope {
					break
				}
			}
		}
	}
	return inscope
}

func dfcSearchEntities(ident string, cntt *Type, scp *Scope) bool {
	sym, _ := scp.Lookup(ident)
	if sym == nil {
		for _, qiv := range QIScope {
			if qiv.activeCount > 0 && qiv.isModule && qiv.mname != "" {
				sym, _ = qiv.scope.Lookup(ident)
				if sym != nil {
					break
				}
			}
		}
	}
	return sym != nil
}

// Called from TC when a manifest oncond based on defined(...) is encountered.
// Cond should be the typechecked condition of a defined-oncond, IOW consisting of 1 or more &&-linked calls to defined.
// Body will be the not typechecked body of the oncond stmt. It's going to be checked when all the symbols required by
// the defined clauses are actually defined. In order to set up the state, we search among existing defns. If all the
// required symbols are already defined, we return true instead of posting a new elt to dfcs. Otherwise we post that elt
// and return false.
func (dfcs *defCondState) addElement(cond, body Term, scp *Scope) bool {
	var args []Term
	if cond.Tag() == AndandExpr {
		args = cond.(*TermL).args
	} else {
		args = []Term{cond}
	}
	bset := makeBitset(len(args))
	idents := make([]string, len(args))
	cntts := make([]*Type, len(args))
	for i, cnd := range args {
		cnd0 := cnd.(*TermTL) // TC ensures this is a call to defined
		idents[i] = cnd0.args[0].(*TermB).value
		cntt := cnd0.args[1]
		var defined bool
		if cntt != nil {
			cntts[i] = cntt.(*Type)
			defined = dfcSearchTypes(idents[i], scp)
		} else {
			defined = dfcSearchEntities(idents[i], cntts[i], scp)
		}
		if defined {
			bset = bset.add(i)
		}
	}
	allbits := Bitset((1 << len(args)) - 1)
	if bset == allbits {
		return true
	}
	dfcelt := defCondElement{idents, cntts, bset, body}
	dfcs.defConds = append(dfcs.defConds, &dfcelt)
	return false
}

// Called whenever a new defn is run. Go through the existing defined-onconds, update any of them,
// and return a list of the bodies that have become enabled as a result.
func (dfcs *defCondState) update4Defn(sym *Symbol) []Term {
	var rslt []Term
	if sym.dtype == nil {
		for i := 0; i < len(dfcs.defConds); {
			dfe := dfcs.defConds[i]
			found := -1
			for j, id := range dfe.idents {
				if id == sym.ident && dfe.cntts[j] == nil {
					dfe.defined = dfe.defined.add(j)
					allbits := Bitset((1 << len(dfe.idents)) - 1)
					if dfe.defined == allbits {
						rslt = append(rslt, dfe.body)
						found = j
						break
					}
				}
			}
			if found >= 0 {
				copy(dfcs.defConds[i:], dfcs.defConds[i+1:])
				dfcs.defConds = dfcs.defConds[:len(dfcs.defConds)-1]
			} else {
				i++
			}
		}
	}
	return rslt
}

// Manifest should only be called on simplified Terms. Since TermB holds all the basic type literals, it's easy to tell.
func Manifest(trm Term) bool {
	_, istb := trm.(*TermB)
	if istb {
		return true
	}
	if callingNamed(trm, "defined") {
		return true
	}
	// todo: add the behavior that asserted exprs are manifest
	return false
}

func emptyStmtlist(trm Term) bool {
	if trm.Tag() == Stmts {
		trm0 := trm.(*TermL)
		return len(trm0.args) == 0
	}
	return false
}

// Symbeval is for ad hoc symbolic evaluation of funcalls on BI ftns. It's driven by a plist item in the ftn symbol.
// It returns the result of symbolic evaluation if that's possible, else the original term.
func symbEval(fcall *TermTL) (ret Term) {
	var derefConstSymbol = func(trm Term) (ret Term) {
		ret = trm
		if trm.Tag() == SymbolTag {
			sym := trm.(*Symbol)
			if sym.binding != nil && sym.plist.Find("var") == nil {
				ret = sym.binding
			}
		}
		return
	}
	ret = fcall
	// I've added typeAs and elttype as type-related QC-time fns. It would be better, I think
	// to do something more like the "symbeval" tagging approach
	if callingNamed(fcall, "typeAs") {
		// evaluate when the type arg is computable
		typarg := fcall.args[1]
		var typargVal *Type
		if typarg.Tag() == TypeTag {
			typargVal = typarg.(*Type)
		} else if typarg.Tag() == Funcall {
			tmp := symbEval(typarg.(*TermTL))
			if tmp.Tag() == TypeTag {
				typargVal = tmp.(*Type)
			}
		}
		if typargVal != nil {
			ent := fcall.args[0]
			if ent.Tag() == SymbolTag {
				ret = makeTermT(Gdref, ent, typargVal, fcall.first, fcall.final)
			} else {
				smashDtype(ent, typargVal)
				ret = ent
			}
		}
	} else if callingNamed(fcall, "elttype") {
		etarg := fcall.args[0]
		var conctyp *Type
		if etarg.Tag() == TypeTag {
			conctyp = etarg.(*Type)
		} else if etarg.Tag() == SymbolTag {
			et0 := etarg.(*Symbol).binding.(*Type)
			if et0.family != TFTypred {
				conctyp = et0
			}
		}
		if conctyp != nil {
			if conctyp.family != TFList { // maybe TFGTS, etc
				return
			}
			return conctyp.v.(*Type)
		}
	}
	switch fcall.trm.Plist().Find("symbeval") {
	case "listlen":
		// can assume arg is a list
		arg := derefConstSymbol(fcall.args[0])
		if arg.Tag() == Litform0 {
			ret = makeIntlit(len(arg.(*TermL).args))
		} else if arg.Tag() == Stringlit {
			ret = makeIntlit(len(arg.(*TermB).value))
		}
	}
	return
}

// Simplify maps terms to terms; simplify(X) returns X unless is constant or always designates
// something else. So simplify of "if(true) A else B" is A; simplify of "2*5" is 10; and so on.
// When simplify returns a constant (like 10) the expr is manifest. IOW trm is manifest if simplify(trm)
// is a TermB. Simplify assumes it's being called in a post-order traversal of its argument.
// If called during a traversal in which simplify is always called in post-order, there's no need to
// recursively call parts again. That's what Simplify does. If that assumption doesn't hold, use SimplifyAll.
// A corollary is that Simplify should only be called on typechecked terms, not directly coming from the parser.
func Simplify(trm Term) Term {
	switch trm0 := trm.(type) {
	case *Symbol:
		if trm0.binding != nil && trm0.plist.Find("var") == nil && Manifest(trm0.binding) {
			return trm0.binding
		}
	case *TermT:
		return trm0.Simplify()
	case *TermTT:
		return trm0.Simplify()
	case *TermL:
		return trm0.Simplify()
	case *TermTL:
		return trm0.Simplify()
	}
	return trm
}

// As mentioned earlier, Simplify should be applied in post-order during typechecking (or any similar traversal).
func (t *TermL) Simplify() (retval Term) {
	retval = t
	changed := false
	switch t.kind {
	case Symchain: // if root is litform...
		if t.args[0].Tag() == Litform0 {
			root := Simplify(t.args[0]).(*TermL)
			atrb := t.args[1].(*Symbol)
			atrbs := root.dtype.v.(*Tupletype).attribs
			inx := -1
			for i, a := range atrbs {
				if a == atrb {
					inx = i
					break
				}
			}
			if inx < 0 {
				panic("impossible")
			}
			retval = root.args[inx]
		} else {
			root := Simplify(t.args[0])
			if root != t.args[0] {
				var argsx []Term
				// flatten nested symchains
				if root.Tag() == Symchain {
					rootargs := root.(*TermL).args
					nra := len(rootargs)
					argsx = make([]Term, nra+len(t.args)-1)
					copy(argsx, rootargs)
					copy(argsx[nra:], t.args[1:])
				} else {
					argsx = make([]Term, len(t.args))
					copy(argsx, t.args)
					argsx[0] = root
				}
				retval = makeTermL(Symchain, argsx, t.dtype, t.first, t.final)
			}
		}
	case Stmts:
		// using {} to rep no stmt, flush that out
		argsx := make([]Term, 0, len(t.args))
		for i := 0; i < len(t.args); i++ {
			a := t.args[i]
			tmp := Simplify(a)
			if tmp != a {
				changed = true
			}
			if tmp.Tag() == IfStmt {
				// delete if expr with empty branch(es)
				tmp0 := tmp.(*TermL)
				if !anyModcall(tmp0.args[0]) && emptyStmtlist(tmp0.args[1]) && (len(tmp0.args) == 2 || emptyStmtlist(tmp0.args[2])) {
					changed = true
					continue
				}
			}
			// note: nested stmts in stmts
			if tmp.Tag() == Stmts {
				// take care about injecting existing identifiers into this stmtlist
				// Note that this entire issue should be handled at a higher level of abstraction, since it's done in numerous places
				for _, s := range tmp.(*TermL).args {
					if s.Tag() == Valdecl || s.Tag() == Vardecl {
						sym := s.(*TermT).arg0.(*Symbol)
						ident0 := identifierOutOfScope(t.scope, nil, sym.ident, sym)
						sym.ident = ident0
						t.scope.Add(sym, 0)
					}
				}
				argsx = append(argsx, tmp.(*TermL).args...)
			} else {
				argsx = append(argsx, tmp)
			}
		}
		if changed {
			retval = makeScopedTermL(t.kind, t.scope, argsx, t.dtype, t.first, t.final)
		}
	case AndandExpr, OrorExpr:
		changed = false
		extinguished := false
		clzs0 := make([]Term, 0, len(t.args))
		for _, clz := range t.args {
			sclz := Simplify(clz)
			if sclz == TrueLiteral {
				if t.kind == OrorExpr {
					extinguished = true
					break
				}
				changed = true
			} else if sclz == FalseLiteral {
				if t.kind == AndandExpr {
					extinguished = true
					break
				}
				changed = true
			} else {
				clzs0 = append(clzs0, sclz)
			}
		}
		// extinguished means a clause eval'd so as to determine the value of the whole thing
		if extinguished {
			if t.kind == AndandExpr {
				return FalseLiteral
			} else {
				return TrueLiteral
			}
		} else if len(clzs0) == 0 {
			// !extinguished but no clauses appended mean they all eval'd to the non-determining for the opr, ie
			// false for ||, true for &&. Again in this case, we know the outcome.
			if t.kind == AndandExpr {
				return TrueLiteral
			} else {
				return FalseLiteral
			}
		}
		if changed {
			retval = makeTermL(t.kind, clzs0, TypeBool, t.first, t.final)
		}
	case Ifcase:
		postclauses := make([]Term, 0, len(t.args))
		for i, clz := range t.args {
			clz0 := Simplify(clz).(*TermTT) // always arrow opr
			if !(clz0.arg0 == FalseLiteral) {
				postclauses = append(postclauses, clz0)
			}
			if clz0.arg0 == TrueLiteral {
				if len(postclauses) == 1 {
					return clz0.arg1
				} else if i < len(t.args) {
					changed = true
				}
				break
			} else if clz != clz0 {
				changed = true
			}
		}
		if changed {
			retval = makeTermL(Ifcase, postclauses, t.dtype, t.first, t.final)
		}
	case IfStmt:
		condx := Simplify(t.args[0])
		changed = condx != t.args[0]
		if condx == TrueLiteral {
			retval = t.args[1]
			if retval.Tag() == Stmts && len(retval.(*TermL).args) == 1 {
				retval = retval.(*TermL).args[0]
			}
		} else if condx == FalseLiteral {
			if len(t.args) == 3 {
				retval = t.args[2]
				if retval.Tag() == Stmts && len(retval.(*TermL).args) == 1 {
					retval = retval.(*TermL).args[0]
				}
			} else {
				// this is a no-op
				return makeScopedTermL(Stmts, makeScope(), []Term{}, TypeNothing, t.first, t.final)
			}
		} else {
			tpart := Simplify(t.args[1])
			var epart Term
			if len(t.args) == 3 {
				epart = Simplify(t.args[2])
				if epart.Tag() == Stmts && len(epart.(*TermL).args) == 0 {
					return makeTermL(IfStmt, []Term{condx, tpart}, t.dtype, t.first, t.final)
				}
			}
			if changed || tpart != t.args[1] || (epart != nil && epart != t.args[2]) {
				args := make([]Term, len(t.args))
				args[0] = condx
				args[1] = tpart
				if len(t.args) == 3 {
					args[2] = epart
				}
				retval = makeTermL(IfStmt, args, t.dtype, t.first, t.final)
			}
		}
	default:
		if t.kind == EachStmt {
			body := t.args[2].(*TermL)
			if len(body.args) == 0 {
				return body // this eliminates the each stmt, since it can't do anything
			}
			// Optimzn can sometimes create manifest empty lists or singleton lists
			// saving singletons for later...
			enumeree := t.args[1]
			//if enumeree.Tag() == Litform0 && enumeree.Dtype().family == TFList && len(enumeree.(*TermL).args) == 0 {
			//	fmt.Println("eliminating ", t.String())
			//	return makeScopedTermL(Stmts, makeScope(), []Term{}, TypeNothing, Pos(-1), Pos(-1))
			//}
			if enumeree.Dtype().family == TFList {
				elen := symbEval(makeFuncall(biScope["__len"][0], []Term{enumeree}, TypeInt, nil))
				if elen.Tag() == Intlit && elen.(*TermB).value == "0" {
					//fmt.Println("eliminating ", t.String())
					return makeScopedTermL(Stmts, makeScope(), []Term{}, TypeNothing, Pos(-1), Pos(-1))
				}
				// also, inline+simplifn can make gnc itervars specific
				if t.args[0].Dtype().family == TFTypred {
					t.args[0].(*Symbol).dtype = enumeree.Dtype().Elttype()
				}
			}
		}
		argsx := make([]Term, len(t.args))
		for i, a := range t.args {
			argsx[i] = Simplify(a)
			if argsx[i] != a {
				changed = true
			}
		}
		if changed {
			retval = makeScopedTermL(t.kind, t.scope, argsx, t.dtype, t.first, t.final)
		}
	}
	return
}

func (t *TermT) Simplify() (retval Term) {
	retval = t
	switch t.kind {
	case Valdecl, Vardecl:
		decld := t.arg0.(*Symbol)
		if decld.binding != nil && decld.binding.Tag() != TypeTag {
			decld.binding = Simplify(decld.binding)
		}
	case Lognot:
		if t.arg0 == TrueLiteral {
			retval = FalseLiteral
		} else if t.arg0 == FalseLiteral {
			retval = TrueLiteral
		} else if t.arg0.Tag() == Lognot {
			retval = Simplify(t.arg0.(*TermT).arg0)
		}
	case Negate:
		if Manifest(t.arg0) {
			a0x := t.arg0.(*TermB)
			// can be int or float
			strg := a0x.value
			if strg[0] == '-' {
				strg = strg[1:]
			} else {
				strg = "-" + strg
			}
			a0x.value = strg
			if a0x.kind == Intlit {
				val := a0x.plist.Find("intval").(int)
				a0x.plist.Add("intval", -val)
			} else {
				val := a0x.plist.Find("floatval").(float64)
				a0x.plist.Add("floatval", -val)
			}
			retval = a0x
		}
	case Gdref:
		typ := t.arg0.Dtype()
		tg := t.arg0.Tag()
		if tg == Intlit && t.dtype == TypeByte {
			return
		}
		if tg != SymbolTag && typ.family != TFOrtype {
			// this is obscure but rwrules can propagate multiple copies of the same term making smashing dangerous
			retval = t.arg0.Copy()
			// Can't really smash a termb's type, so exempt it. (Yes, this code is getting ugly.)
			_, istb := retval.(*TermB)
			if !istb {
				smashDtype(retval, t.dtype)
			}
		} else if t.dtype.family == TFTypred && typ.family != TFTypred {
			retval = t.arg0
		}
	case Range:
		// do nothing
	default:
		if t.arg0 != nil {
			a0 := Simplify(t.arg0)
			if a0 != t.arg0 {
				retval = makeTermT(t.kind, a0, t.dtype, t.first, t.final)
			}
		}
	}
	return
}

// post-TC there isn't much TermTT left, just equality. Pure exprs that simplify to the same thing are
// equal; also, manifest exprs can be eval'd completely.
func (t *TermTT) Simplify() (retval Term) {
	retval = t
	if t.kind == EqExpr || t.kind == EqeqExpr {
		if t.arg0.Equal(t.arg1) && isPure(t.arg0) {
			return TrueLiteral
		} else if Manifest(t.arg0) && Manifest(t.arg1) {
			retval = FalseLiteral
			if t.arg0.(*TermB).value == t.arg1.(*TermB).value {
				retval = TrueLiteral
			}
			return
		}
		// This is a tiny subset of the xfm ITE(C,T,E)=T --> C; ITE(C,T,E)=E --> !C when T and E are mutex.
		// The mutex condition is hard to evaluate in general, but easy for the nil/notnil case.
		if t.arg0.Tag() == IfStmt && t.arg1.Equal(nilEntity) {
			a0 := t.arg0.(*TermL)
			scond := Simplify(a0.args[0])
			if a0.args[1].Equal(nilEntity) {
				return scond
			} else if a0.args[2].Equal(nilEntity) {
				return Simplify(makeTermT(Lognot, scond, TypeBool, a0.first, a0.final))
			}
		}
	}
	a0 := t.arg0
	if t.kind != AsgnStmt {
		a0 = Simplify(a0)
	}
	a1 := Simplify(t.arg1)
	if a0 != t.arg0 || a1 != t.arg1 {
		retval = makeTermTT(t.kind, a0, a1, t.dtype, t.first, t.final)
	}
	return
}

// special simplification for counting range literals. Return an intlit if successful else the arg.
func tryRangeCount(trm Term) Term {
	if trm.Tag() == Range {
		t := trm.(*TermT)
		rnglit := t.arg0.(*TermL)
		r0 := rnglit.args[0]
		r1 := rnglit.args[1]
		r2 := rnglit.args[2]
		r3 := rnglit.args[3]
		manif := Manifest(r0) && Manifest(r1) && Manifest(r2) && Manifest(r3)
		if manif {
			if r0.Dtype() == TypeInt {
				rangeCount := r1.Plist().Find("intval").(int) - r0.Plist().Find("intval").(int)
				if r3.(*TermB).value == "true" {
					rangeCount += 1
				}
				if r2.(*TermB).value != "1" {
					rangeCount /= r2.Plist().Find("intval").(int)
				}
				return makeIntlit(rangeCount)
			} else {
				rangeCountF := r1.Plist().Find("floatval").(float64) - r0.Plist().Find("floatval").(float64)
				if r3.(*TermB).value == "true" {
					rangeCountF += 1.0
				}
				if r2.(*TermB).value != "1.0" {
					rangeCountF /= r2.Plist().Find("floatval").(float64)
				}
				return makeIntlit(int(rangeCountF))
			}
		}
	}
	return trm
}

// Special simplification for selecting from range literals. Selcall should be select(R, X, Y) where R is a Range term.
// No need for X and Y manifest; the resulting Range is R.low+X ... R.low+Y.
func tryRangeSelect(selcall *TermTL) Term {
	low := Simplify(selcall.args[1]) // todo: should check this is pure (no mod call therein)
	high := Simplify(selcall.args[2])
	rngarg := selcall.args[0]
	var rng *TermL
	if rngarg.Tag() == Range {
		rng = rngarg.(*TermT).arg0.(*TermL)
	} else {
		rng = rngarg.(*Symbol).binding.(*TermT).arg0.(*TermL)
	}
	r0 := rng.args[0]
	plus := r0.Dtype().methods.Find("+")
	xlow := makeFuncall(plus, []Term{r0, low}, r0.Dtype(), nil)
	xhigh := makeFuncall(plus, []Term{r0, high}, r0.Dtype(), nil)
	rnglit := makeTermL(Litform0, []Term{xlow, xhigh, rng.args[2], FalseLiteral}, rng.dtype, Pos(-1), Pos(-1))
	return makeTermT(Range, rnglit, selcall.args[0].Dtype(), Pos(-1), Pos(-1))
}

// TermTL is where the action is in manifest eval, mostly because oprs are mostly remapped to funcalls.
// I'm doing arith and logical oprs, and possibly some string oprs.
func (t *TermTL) Simplify() (retval Term) {
	retval = t
	if t.kind == Funcall {
		if t.trm.Tag() == SymbolTag {
			sym := t.trm.(*Symbol)
			fnnames := []string{"*", "+", "-", "/", "<", "<=", ">", ">="} // these are sorted
			inx := binsearchStrings(fnnames, sym.ident)
			if inx >= 0 && fnnames[inx] == sym.ident && len(t.args) == 2 {
				// known opr, next check if the arg types are known
				at0 := t.args[0].Dtype()
				at1 := t.args[1].Dtype()
				if (at0 == TypeInt || at0 == TypeFloat) && at0.Equal(at1) {
					m0 := Simplify(t.args[0])
					m1 := Simplify(t.args[1])
					if Manifest(m0) && Manifest(m1) {
						if at0 == TypeInt {
							v0 := m0.(*TermB).plist.Find("intval").(int)
							v1 := m1.(*TermB).plist.Find("intval").(int)
							if inx >= 4 {
								retval = TrueLiteral
								if v0 == v1 {
									if inx == 4 || inx == 6 {
										retval = FalseLiteral
									}
								} else if v0 < v1 {
									if inx >= 6 {
										retval = FalseLiteral
									}
								} else if inx < 6 {
									retval = FalseLiteral
								}
							} else {
								var r int
								switch inx {
								case 0:
									r = v0 * v1
								case 1:
									r = v0 + v1
								case 2:
									r = v0 - v1
								case 3:
									r = v0 / v1
								}
								retv := makeTermB(Intlit, fmt.Sprint(r), Pos(-1))
								retv.plist.Add("intval", r)
								retval = retv
							}
						} else { // has to be float
							v0 := m0.(*TermB).plist.Find("floatval").(float64)
							v1 := m1.(*TermB).plist.Find("floatval").(float64)
							if inx >= 4 {
								retval = TrueLiteral
								if v0 == v1 {
									if inx == 4 || inx == 6 {
										retval = FalseLiteral
									}
								} else if v0 < v1 {
									if inx >= 6 {
										retval = FalseLiteral
									}
								} else if inx < 6 {
									retval = FalseLiteral
								}
							} else {
								var r float64
								switch inx {
								case 0:
									r = v0 * v1
								case 1:
									r = v0 + v1
								case 2:
									r = v0 - v1
								case 3:
									r = v0 / v1
								}
								retv := makeTermB(Floatlit, fmt.Sprint(r), Pos(-1))
								retv.plist.Add("floatval", r)
								retval = retv
							}
						}
						return
					}
				} else if inx == 1 && at0 == TypeString && at1 == TypeString {
					m0 := Simplify(t.args[0])
					m1 := Simplify(t.args[1])
					if Manifest(m0) && Manifest(m1) {
						retval = makeStringTerm(m0.(*TermB).value+m1.(*TermB).value, Pos(-1), false)
						return
					}
				}
			} else {
				tmp := symbEval(t)
				if tmp != t {
					return tmp
				}
			}
			// There are count cases we can simplify
			if sym.ident == "count" && len(t.args) == 1 && t.dtype == TypeInt {
				ctg := t.args[0].Tag()
				if ctg == Litform0 && t.args[0].Dtype().family == TFList {
					return makeIntlit(len(t.args[0].(*TermL).args))
				} else {
					rcnt := tryRangeCount(t.args[0])
					if rcnt.Tag() == Intlit {
						return rcnt
					}
				}
			}
			// Note that inliner rewrites select calls to slicex but same same.
			var rangeSelCond = func(sym *Symbol, trm *TermTL) bool {
				if sym.ident == "select" || sym.ident == "__slicex" {
					if len(trm.args) == 3 && trm.dtype.family == TFList {
						lstarg := trm.args[0]
						ltg := lstarg.Tag()
						return ltg == Range || (ltg == SymbolTag && lstarg.(*Symbol).binding != nil && lstarg.(*Symbol).binding.Tag() == Range)
					}
				}
				return false
			}
			if rangeSelCond(sym, t) {
				return tryRangeSelect(t)
			}
		}
		// nothing to do to this ftn but check the args
		changed := false
		argsx := make([]Term, len(t.args))
		for i, a := range t.args {
			argsx[i] = Simplify(a)
			if argsx[i] != a {
				changed = true
			}
		}
		if changed {
			retval = makeFuncall(t.trm, argsx, t.dtype, t)
		}
	} else if t.kind == Litform {
		panic("this should not be in post-TC terms")
	} else if t.kind == Typecase {
		tctyp := t.trm.Dtype()
		if tctyp != TPEntity {
			clznum := -1
			for inx, clz := range t.args {
				seen := make([]*Type, 0, 2)
				if tctyp.compat(clz.(*TermTT).arg0.(*Type), &seen) {
					clznum = inx
					break
				}
			}
			if clznum >= 0 {
				if prOptimize {
					logger.Println("simplifying a typecase to the ", t.args[clznum].(*TermTT).arg0.(*Type).String(), " case")
				}
				retval = Simplify(t.args[clznum].(*TermTT).arg1)
			} else if prOptimize {
				logger.Println("NOT simplifying a typecase because didn't find clause compat with", tctyp.String())
			}
		} else if prOptimize {
			logger.Println("NOT simplifying a typecase because trm type is entity")
		}
	}
	return
}

// stub
// The intended functionality here is to determine if the two terms are equal when
// occurrences of substns[0] in the first are replaced by substns[1] in the second.
// A more aggressive/advanced checker will check logical equivalence under more extensive
// sets of axioms, such as commassoc of arith ops, etc.
// Returning false means that all the assertions that end up in clauses are disabled.
func eqUnderSubstn(assertedInType, queried Term, substns []Term) bool {
	return false
}

// do the assertions (reqmts) in aot imply those in other?
// This handles type-type assertions. The caller needs to provide the substitutions aka "free variables"
// which are the self symbols a/w each type.
func (aot *AssertsOfType) implies(other *AssertsOfType, selfs []Term) bool {
	if aot == nil {
		return true
	}
	if other == nil {
		return false
	}
	for i, nm := range other.binfNames {
		found := false
		cmpr := isComparisonOp(nm)
		for j, nm0 := range aot.binfNames {
			if nm == nm0 {
				found = true
				v0 := other.binfValues[i]
				v1 := aot.binfValues[j]
				// in the comparison case, aot.Add has nmlzd integer cases to use only >= and <=
				if cmpr && v0.Dtype() == TypeInt {
					v0i := v0.Plist().Find("intval").(int)
					v1i := v1.Plist().Find("intval").(int)
					if nm == "<=" {
						if v0i > v1i {
							return false
						}
					} else {
						if v0i < v1i {
							return false
						}
					}
				} else if !(Manifest(v0) && Manifest(v1) && v0.Equal(v1)) {
					return false
				}
				break
			}
		}
		if !found {
			return false
		}
	}
	for _, clz := range other.clauses {
		found := false
		for _, oclz := range aot.clauses {
			if eqUnderSubstn(clz, oclz, selfs) {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}

// To correctly match stitem against a proposed type S in applyST (provided either by the type of a Symbol or the
// argtype or body of a predicate), we need to check that S satisfies any assertions made about (and hence required by)
// stitem.source. If some of these conditions are made in the body of the applyST predicate, the effect is to delay eval
// of those until particular terms are considered for translation. If they're made in the type (of the funarg to applyST
// predicate or of the Symbol to which applyST applies) then they're eval'd when the applyST happens in order to decide
// whether the given stitem can match.
//
// When we call this ftn, we'll know that S is compat with stitem.source, which means they're structurally identical.
// So we need to traverse their structures in pll and at each point of structure, examine any assertions in the stitem.source
// structure with the corresponding type's assertions, by calling implies, the function defined above. We don't care about
// methods, and since function signatures don't care about assertions, we don't need to care about these either.
//
// The traversal is done with TraverseTypePre, with the pll one done by imitating same in a stack in the state struct. As
// with any user of TraverseTypePre, recursive type detection/infloop prevention is the responsibility of the workfn and
// state struct as well.
type asrtCheckSV struct {
	pllParts []*Type
	seen     []*Type
	errorAt  *Type
}

// Pop assumes asck.pllParts is non-empty
func (asck *asrtCheckSV) Pop() *Type {
	m1 := len(asck.pllParts) - 1
	ret := asck.pllParts[m1]
	asck.pllParts = asck.pllParts[:m1]
	return ret
}

// Push next visited part on pllParts
func (asck *asrtCheckSV) Push(part *Type) {
	asck.pllParts = append(asck.pllParts, part)
}

// The workfn is designed to traverse another type in pll with the one passed in; this other type
// is initially placed in the asrtCheckSV.pllParts stack. On each call here, we have 2 new pll subparts of
// the original types and we check that the asserts of one support those of the other. We also do recursion
// detection using the seen slot. Return values are -1, 0, 1 to obey the workfn prcl for TraverseTypePre;
// -1 means don't visit components; 1 means exit.
func asrtCheckWorkfn(typ *Type, sv interface{}) int {
	acsv := sv.(*asrtCheckSV)
	for i := 0; i < len(acsv.seen); i += 1 {
		if typ == acsv.seen[i] {
			return -1
		}
	}
	plltype := acsv.Pop()
	substns := []Term{plltype.selfsym, typ.selfsym}
	if typ.asserts != nil && (plltype.asserts == nil || !plltype.asserts.implies(typ.asserts, substns)) {
		acsv.errorAt = typ
		return 1
	}
	switch typ.family {
	case TFList:
		acsv.seen = append(acsv.seen, typ)
		acsv.Push(plltype.v.(*Type))
	case TFSpace:
		acsv.seen = append(acsv.seen, typ)
		acsv.Push(plltype.v.(*Spacetype).elttype)
	case TFTuple:
		acsv.seen = append(acsv.seen, typ)
		atrbs := plltype.v.(*Tupletype).attribs
		for i := 1; i <= len(atrbs); i++ {
			acsv.Push(atrbs[len(atrbs)-i].dtype)
		}
	case TFFtn:
		return -1
	}
	return 0
}

// Satisfies applies the parallel traversal machinery via TraverseTypePre and asrtCheckWorkfn defined above
// to decide if other satisfies the assertions in typ. In this case, a true value from TraverseTypePre means
// that the search failed and the component at which it failed is recorded in the errorAt slot of the state.
// Thus, returning a non-nil *Type from satisfies means that it fails; nil means success.
func (typ *Type) satisfies(other *Type) *Type {
	if typ == other {
		return nil
	}
	acsv := &asrtCheckSV{[]*Type{other}, make([]*Type, 0, 4), nil}
	ret := typ.TraverseTypePre(asrtCheckWorkfn, acsv)
	if ret {
		return acsv.errorAt
	}
	return nil
}
