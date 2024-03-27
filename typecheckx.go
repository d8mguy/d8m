// Typechecker for the more "exotic" parts of the language: special functions, STMap stmt, xprftns, and the like

package main

import "fmt"

// Typecheck STMap statements. Parser ensures first 2 elts are source and target, which should be types.
// Then the optional direct stmt followed by any number of xltrules, which it ensures are Arrow exprs or
// else enumerable "call"
func (t *TermL) tcStmap(tci *TCInfo) Term {
	// TC src type
	srctype := TC4Type(t.args[0], nil, tci)
	if srctype.Tag() == ErrorTag {
		return srctype
	}
	// same for tgt type
	tci.cxt.Push(GomapAllowed)
	desttype := TC4Type(t.args[1], nil, tci)
	tci.cxt.Pop()
	if desttype.Tag() == ErrorTag {
		return desttype
	}
	// source and target decls check out; create scopes and symbols to be used later
	srctype0 := srctype.(*Type)
	desttype0 := desttype.(*Type)
	stypsym := makeSymbol("source", nil, srctype0)
	ttypsym := makeSymbol("target", nil, desttype0)
	sselfsym := makeSymbol("self", srctype0, nil)
	sselfsym.plist.Add("var", true)
	tselfsym := makeSymbol("self", desttype0, nil)
	tselfsym.plist.Add("var", true)
	srcscope := makeScope()
	srcscope.Add(stypsym, 0)
	srcscope.Add(sselfsym, 0)
	tgtscope := makeScope()
	tgtscope.Add(ttypsym, 0)
	tgtscope.Add(tselfsym, 0)
	resultinx := 2 // where to write results
	// We're going to create an stitem and put it in the nearest enclosing scope other than a given scope.
	givenscope := tci.cxt.FindItem("givenscope")
	destscope := tci.scopes
	var gs0 *Scope
	if givenscope == nil {
		gs0 = makeScope()
	} else {
		destscope = destscope.parent // don't add to the givenscope but its parent
		gs0 = givenscope.(*Scope)
	}
	// first, check for overlaps. [First approximation: equality]
	for curscope := destscope; curscope != nil; curscope = curscope.parent {
		for _, sti := range curscope.stitems {
			if srctype0.Equal(sti.source) && desttype0.Equal(sti.dest) {
				errmsg := fmt.Sprintf("There's already an [%s -> %s] STMap", srctype0.String(), desttype0.String())
				return tci.Error(errmsg, t)
			}
		}
	}
	nmethods := len(srctype0.methods)
	fcargs := make([][]Term, nmethods)
	fcrhsides := make([]Term, nmethods)
	// the following is slightly wrong: should really note which GES occur in xltrules as matchables
	// The code here could mistake a GES bound as a space dim as being a matchable
	// todo: fix this
	mbles := make(symbolSlice, 0, len(gs0.entries))
	gs1 := makeScope()
	for _, sx := range gs0.entries {
		if sx.sym.dtype == nil {
			gs1.Add(sx.sym, 0)
		} else {
			mbles.insert(mbles.binsearch(sx.sym.ident), sx.sym)
			// ensure that matchables of type source get treated as being of type target in rhs of xltrules
			if sx.sym.dtype.Equal(srctype) {
				tgtscope.Add(makeSymbol(sx.sym.ident, desttype0, nil), 0)
			}
		}
	}
	sti := &Stitem{srctype0, desttype0, gs1, &mbles, nil, tselfsym, 0,
		fcargs, fcrhsides, nil, nil, nil, nil, nil}
	destscope.stitems = append(destscope.stitems, sti)

	firstXltruleIndex := 2
	// next, handle optional direct stmt
	if len(t.args) > 2 && t.args[2].Tag() == DirectStmt {
		firstXltruleIndex = 3
		// Note added while rewriting this: the direct stmt processing deserves to be reworked but in my judgeent,
		// it's not important enough to spend the time on.
		// Each elt of direct stmt is either Ident or Symdecl tag
		// Because direct rules have a simple form, we can (and do) check that they'll work in stapply.
		// Then we remember that in a plist entry. Later, we may do some checking on UD xltrules too.
		for _, dx := range t.args[2].(*TermL).args {
			var srcinx, tgtinx int
			if dx.Tag() == Ident {
				// local helper ftn: check src or dest methodness; error if returned string != ""
				var dirtest = func(dxname string, typ *Type, srcdest string) (int, string) {
					inx := typ.methodIndex(dxname)
					if inx < 0 {
						return -1, fmt.Sprintf("in direct stmt, %s is not a method of %s type", dxname, srcdest)
					} else if inx < len(typ.methods)-1 && typ.methods[inx+1].sym.ident == dxname {
						return -1, fmt.Sprintf("in direct stmt, %s is ambiguous by itself", dxname)
					}
					return inx, ""
				}
				// unqualified form
				dxname := dx.(*Token).val
				var msg string
				srcinx, msg = dirtest(dxname, srctype0, "source")
				if msg != "" {
					return tci.Error(msg, t)
				}
				tgtinx, msg = dirtest(dxname, desttype0, "target")
				if msg != "" {
					return tci.Error(msg, t)
				}
			} else {
				// qualified form: parser ensures that in this case tag is Symdecl; type slot better be a ftntype
				dx0 := dx.(*TermL)
				dxname := dx0.args[0].(*Token).val
				tci.PushScope(srcscope)
				dxtyp := TC4Type(dx0.args[1], nil, tci)
				tci.PopScope()
				if dxtyp.Tag() == ErrorTag {
					return dxtyp
				}
				// There needs to be src and dest methods that match the ftn type
				// dirtest2 returns the index in typ.methods of the matching one, or -1
				var dirtest2 = func(dxname string, typ, mthdtyp *Type) int {
					inx := typ.methodIndex(dxname)
					if inx < 0 {
						return inx
					}
					for ; typ.methods[inx].sym.ident == dxname; inx++ {
						seen := []*Type{}
						if typ.methods[inx].sym.dtype.compat(mthdtyp, &seen) {
							return inx
						}
					}
					return -1
				}
				srcinx = dirtest2(dxname, srctype0, dxtyp.(*Type))
				if srcinx < 0 {
					return tci.Error(fmt.Sprintf("can't find a match for %s in source type", dx0.String()), t)
				}
				tgtinx = dirtest2(dxname, desttype0, dxtyp.(*Type))
				if tgtinx < 0 {
					return tci.Error(fmt.Sprintf("can't find a match for %s in target type", dx0.String()), t)
				}
			}
			sti.xmethods = sti.xmethods.add(srcinx)
			// by leaving funcallArgs nil, we signal to the runtime engine that this is a direct rule
			sti.funcallRhs[srcinx] = desttype0.methods[tgtinx].sym

			resultinx += 1
		}
	}
	// now process xltrules, if any
	for _, xlr := range t.args[firstXltruleIndex:] {
		// parser ensures that each xlr is an Arrow Term (hence TermTT). Lhs special cases "funcall" to enumerable.
		xlr0 := xlr.(*TermTT)
		// Process lhs then rhs, linking in scope that binds "self" to the appropriate type
		var lhs Term
		enumble := false
		if callingNamed(xlr0.arg0, "enumerable") {
			fc0 := xlr0.arg0.(*TermTL)
			lhs = fc0.args[0]
			if !(tokenNamed(lhs, "self") && len(fc0.args) == 1) {
				return tci.Error("in xltrule, arg to enumerable must be 'self'", fc0)
			}
			enumble = true
		} else {
			tci.PushScope(srcscope)
			lhs = xlr0.arg0.Typecheck(nil, tci)
			tci.PopScope()
			if lhs.Tag() == ErrorTag {
				return lhs
			}
		}
		tci.PushScope(tgtscope)
		rhs := xlr0.arg1.Typecheck(nil, tci)
		tci.PopScope()
		if rhs.Tag() == ErrorTag {
			return rhs
		}
		// for now, we only check the lhs and rhs TC, not that they're legit form for rules
		// (must contain implicit/explicit refnc to self and a method). Possibly we'll do that later.
		rtyp := rhs.Dtype()
		// Both parts are TC'd, check form and fill in appropriate slots
		if enumble { // cntt is that srctype.Elttype equals rtyp.Elttype
			srcelt := srctype0.Elttype()
			seen := []*Type{}
			if !srcelt.compat(rtyp.Elttype(), &seen) {
				return tci.Error("in enumerable rule, resulting elttype must be compatible with lhs", t)
			}
			if sti.enumruleRhs != nil {
				return tci.Error("multiple enumerable rules not allowed", t)
			}
			sti.enumruleRhs = rhs
		} else {
			ltyp := lhs.Dtype()
			wanted := ltyp
			if ltyp.Equal(srctype0) {
				wanted = desttype0
			}
			if !(wanted == TypeNothing || wanted.Equal(rtyp)) {
				errmsg := fmt.Sprintf("in xltrule, expected rhs type %s but got %s", wanted.String(), rtyp.String())
				return tci.Error(errmsg, t)
			}
			ltg := lhs.Tag()
			if ltg == Funcall {
				lhs0 := lhs.(*TermTL)
				if lhs0.trm.Tag() != SymbolTag || sti.source.methodIndexByMethod(lhs0.trm.(*Symbol)) < 0 {
					return tci.Error("bad form for xltrule", lhs)
				}
				mthdsym := lhs0.trm.(*Symbol)
				mthdinx := sti.source.methodIndexByMethod(mthdsym)
				rcvr, issym := lhs0.args[0].(*Symbol)
				if issym && rcvr.ident == "self" {
					// here for "std form" method xltn, other cases cause error at bottom
					if sti.xmethods.eltof(mthdinx) {
						sti.overflowLhs = append(sti.overflowLhs, lhs)
						sti.overflowRhs = append(sti.overflowRhs, rhs)
					} else {
						sti.funcallArgs[mthdinx] = lhs0.args[1:]
						sti.funcallRhs[mthdinx] = rhs
						sti.xmethods = sti.xmethods.add(mthdinx)
					}
				} else {
					return tci.Error("xltrule must reference 'self' as receiver", lhs)
				}
			} else if ltg == Symchain {
				lhs0 := lhs.(*TermL)
				root, issym := lhs0.args[0].(*Symbol)
				if issym && root.ident == "self" && len(lhs0.args) == 2 {
					atrb := lhs0.args[1].(*Symbol)
					sti.attribs = append(sti.attribs, atrb)
					sti.attribRhs = append(sti.attribRhs, rhs)
				} else {
					return tci.Error("can't make sense of attribute rule", t)
				}
			} else {
				return tci.Error("bad form for xltrule", lhs)
			}
		}
	}
	return t
}

// specialFtnMap identifies function names that are "reserved" for special treatment
// These must be written as standard funcalls, are never multibound, etc.
// This map codes both whether an identifier designates such a "function" and if so,
// how many args it has. Go dictates that any string not in the map returns 0. Since no
// actual pseudo-ftn has 0 args, we use that to filter. Any number > 0 means that #args; -1 means variable.
var specialFtnMap = map[string]int{
	"always":       2,
	"applyST":      2,
	"cast":         2,
	"typeAs":       2,
	"channelMake":  2,
	"changed":      1,
	"connected":    1,
	"compile":      -1,
	"load":         1,
	"setwd":        1,
	"cvt":          2,
	"dimension":    2,
	"xp":           1,
	"defined":      2,
	"enumerable":   1,
	"Gocall":       1,
	"Gomethod":     1,
	"purified":     1,
	"rcvrLinked":   1,
	"noInline":     1,
	"alwaysInline": 1,
	"rewrite":      -1,
	"stringify":    1,
	// below here only active in xprftns
	"matchquery":          1,
	"pattern":             1,
	"contains":            -1,
	"match":               -1,
	"matches":             -1,
	"matching":            3,
	"matchcount":          -1,
	"mksym":               -1,
	"mktype":              -1,
	"mkterm":              -1,
	"mkstmts":             1,
	"parseRW":             1,
	"lookup":              1,
	"lookupType":          1,
	"rewriteStoreGet":     1,
	"rewriteStoreSet":     -1,
	"rewriteStoreDeclare": 3,
	"defineGlobal":        1,
}

// tables to drive TC for mktype and mkterm. Lifted direct from ruby version of the compiler, with labels changed to
// other types as appropriate.
type TTIcode int

const (
	ttiTrm = iota
	ttiTrmO
	ttiTrms  // seq of xprterms
	ttiLstrm // explicit list of xprterms
	ttiSym
	ttiSyms
	ttiTyp
	ttiStmts
	ttiStmtsO // O is for optional
)

var mkTermTagInfo = map[string][]TTIcode{
	"symchain":    {ttiSyms},
	"gdref":       {ttiTrm, ttiTyp},
	"litform":     {ttiTyp, ttiTrms},
	"val":         {ttiSym},
	"var":         {ttiSym},
	"funcall":     {ttiTrm, ttiLstrm},
	"stmts":       {ttiStmts},
	"if":          {ttiTrm, ttiStmts, ttiStmtsO},
	"ifcase":      {ttiTrms},
	"oncondition": {ttiTrm, ttiStmts},
	"arrow":       {ttiTrm, ttiTrm},
	"eq":          {ttiTrm, ttiTrm},
	"eqeq":        {ttiTrm, ttiTrm},
	"lognot":      {ttiTrm},
	"irange":      {ttiTrm, ttiTrm, ttiTrm, ttiTrm},
	"frange":      {ttiTrm, ttiTrm, ttiTrm, ttiTrm},
	"asgn":        {ttiTrm, ttiTrm},
	"each":        {ttiSym, ttiTrm, ttiStmts},
	"break":       {},
	"continue":    {},
	"return":      {ttiTrmO},
	"typeterm":    {ttiTyp, ttiTrm},
}

// Called exclusively from tcSpecialXprftns to handle just mktype and mkterm.
func tcMkTypeTerm(t *TermTL, ftnname string, tci *TCInfo) Term {
	// first arg must be label lit
	lbllit, isLbl := t.args[0].(*TermB)
	if !isLbl || lbllit.kind != Labellit {
		return tci.Error("label expected", t)
	}
	ftnsym, _ := tci.scopes.Lookup(ftnname) // guaranteed to work
	if ftnname == "mktype" {
		// following args depend on value of the first; only checked now if type families with args
		if lbllit.value == "list" {
			if len(t.args) != 2 {
				return tci.Error("wrong number of arguments", t)
			}
			t.args[1] = TC4Type(t.args[1], nil, tci)
			if t.args[1].Tag() == ErrorTag {
				return t.args[1]
			}
		} else if lbllit.value == "tuple" || lbllit.value == "situated" {
			if len(t.args) != 2 {
				return tci.Error("wrong number of arguments", t)
			}
			t.args[1] = t.args[1].Typecheck(Gzsymlist, tci)
			if t.args[1].Tag() == ErrorTag {
				return t.args[1]
			}
		} else if lbllit.value == "function" {
			if len(t.args) != 4 {
				return tci.Error("wrong number of arguments", t)
			}
			fmls := t.args[1].Typecheck(Gztypelist, tci)
			rettyp := TC4Type(t.args[2], nil, tci)
			purwd := t.args[3].Typecheck(TypeLabel, tci)
			if len(tci.errors) != 0 {
				return tci.errors[0]
			}
			t.args[1] = fmls
			t.args[2] = rettyp
			t.args[3] = purwd
		} else if lbllit.value == "space" {
			if len(t.args) != 3 {
				return tci.Error("wrong number of arguments", t)
			}
			t.args[1] = TC4Type(t.args[1], nil, tci)
			t.args[2] = t.args[2].Typecheck(TypeInt, tci)
			if t.args[1].Tag() == ErrorTag {
				return t.args[1]
			}
		} else if lbllit.value == "Gomap" {
			panic("unwritten")
		} else if lbllit.value == "Gochannel" {
			t.args[1] = TC4Type(t.args[1], nil, tci)
			if t.args[1].Tag() == ErrorTag {
				return t.args[1]
			}
		} else {
			// here, can be typegen name; treat all args as xprterm
			panic("unwritten")
		}
		t.trm = ftnsym
		t.args[0] = lbllit
		t.dtype = ftnsym.dtype.v.(*Ftntype).rettype
	} else {
		// here for mkterm, use table driven approach
		ttis := mkTermTagInfo[lbllit.value]
		if ttis == nil {
			return tci.Error(fmt.Sprintf("%s is not a known tag", lbllit.value), t)
		}
		argsx := make([]Term, len(t.args))
		argsx[0] = lbllit
		arginx := 1
		for _, tti := range ttis {
			if arginx >= len(t.args) {
				if tti == ttiStmtsO || tti == ttiTrmO {
					break
				}
				return tci.Error("not enough arguments", t)
			}
			a := t.args[arginx]
			switch tti {
			case ttiTrm, ttiTrmO:
				argsx[arginx] = a.Typecheck(Gzterm, tci)
				if argsx[arginx].Tag() == ErrorTag {
					return argsx[arginx]
				}
			case ttiTrms: // must be last in ttis
				for ; arginx < len(t.args); arginx++ {
					argsx[arginx] = t.args[arginx].Typecheck(Gztermlist, tci)
				}
			case ttiLstrm:
				argsx[arginx] = a.Typecheck(TPList, tci)
			case ttiSym:
				argsx[arginx] = a.Typecheck(Gzsym, tci)
			case ttiSyms: // must be last in ttis
				for ; arginx < len(t.args); arginx++ {
					argsx[arginx] = t.args[arginx].Typecheck(Gzsym, tci)
				}
			case ttiTyp:
				argsx[arginx] = TC4Type(a, nil, tci)
			case ttiStmts, ttiStmtsO:
				argsx[arginx] = a.Typecheck(Gzstmts, tci)
			}
			arginx += 1
		}
		if len(tci.errors) > 0 && tci.tentative == 0 {
			return tci.errors[0]
		}
		if lbllit.value == "each" && len(t.args) == 5 {
			// handle the optional inx arg outside the table scheme
			tmp := t.args[4].Typecheck(Gzsym, tci)
			if tmp.Tag() == ErrorTag {
				return tmp
			}
			argsx[arginx] = tmp
		}
		t.trm = ftnsym
		t.args = argsx
		t.dtype = ftnsym.dtype.v.(*Ftntype).rettype
	}
	return t
}

// Called exclusively from specialFuncall to process the xprftn BI's. The rcvr is a funcall,
// ftnname is the ident of the ftn symbol. Returns an error on failure. On success, return
// a smashed rcvr or a newly synthd funcall, usually decorated with a compiled pattern entity.
// This will only be called if tci.inXprftn; these symbols are unreserved outside that context.
func (t *TermTL) tcSpecialXprftns(ftnname string, cntt *Type, tci *TCInfo) Term {
	// typecheck a pattern term and put it into proper form. The ptnterm arg is the pattern, not the match call.
	var checkPattern = func(ptnterm Term) Term {
		ret := ptnterm.Typecheck(nil, tci)
		if ret.Tag() == ErrorTag {
			return ret
		}
		if ret.Tag() == Stringlit {
			parsed := parseString(ret.(*TermB).value)
			tci.cxt.Push(SymbolPattern) // special hack to loosen some TC restrns
			ret = parsed.Typecheck(nil, tci)
			tci.cxt.Pop()
		} else if ret.Tag() == SymbolTag && ret.Dtype() == Gzptn {
			ret = ret.(*Symbol).binding
			if !callingNamed(ret, "pattern") {
				panic("internal")
			}
			ret = ret.(*TermTL).args[0]
		}
		return ret
	}
	ftnsym, _ := tci.scopes.Lookup(ftnname) // guaranteed to work
	funtyp := ftnsym.dtype.v.(*Ftntype)
	seen := []*Type{}
	// special case mksym; probably isn't the right soln
	if cntt != nil && ftnname != "mksym" && !funtyp.rettype.compat(cntt, &seen) {
		return tci.ErrorExpect(cntt, funtyp.rettype, t)
	}
	var ptnterm Term
	if ftnname == "mksym" {
		ident := t.args[0].Typecheck(nil, tci)
		if ident.Tag() == ErrorTag {
			return ident
		}
		idtyp := ident.Dtype()
		if idtyp.family != TFLabel && idtyp.family != TFString {
			return tci.Error("expected label or string", t.args[0])
		}
		if t.args[1].Tag() == Labellit && t.args[1].(*TermB).value == "type" {
			if len(t.args) < 3 {
				return tci.Error("type-valued mksym is missing the value", t)
			}
			bdg := TC4Type(t.args[2], nil, tci)
			if bdg.Tag() == ErrorTag {
				return bdg
			}
			t.args[2] = bdg
		} else {
			var typ Term
			typarg := t.args[1]
			// special handling here: a type-valued matchable is not handled as a std type
			if typarg.Tag() == Ident || callingNamed(typarg, "mktype") {
				tci.makeTentative()
				typ = typarg.Typecheck(nil, tci)
				tci.unmakeTentative()
				if typ.Tag() == ErrorTag {
					typ = nil
				}
				//} else if callingNamed(typarg, "type") {
				// should not eval at this time
			}
			if typ == nil {
				typ = TC4Type(t.args[1], nil, tci)
			}
			if typ.Tag() == ErrorTag {
				return typ
			}
			t.args[1] = typ
			if len(t.args) == 3 {
				bdg := t.args[2].Typecheck(nil, tci)
				if bdg.Tag() == ErrorTag {
					return bdg
				}
				t.args[2] = bdg
			}
		}
		t.trm = ftnsym
		t.args[0] = ident
		t.dtype = Gzsym
		return t
	} else if ftnname == "mktype" || ftnname == "mkterm" {
		return tcMkTypeTerm(t, ftnname, tci)
	} else if ftnname == "mkstmts" {
		a0 := t.args[0].Typecheck(funtyp.fmlargs[0], tci)
		if a0.Tag() == ErrorTag {
			return a0
		}
		t.trm = ftnsym
		t.args[0] = a0
		t.dtype = funtyp.rettype
		return t
	} else if ftnname == "lookup" || ftnname == "lookupType" {
		a0 := t.args[0].Typecheck(funtyp.fmlargs[0], tci)
		if a0.Tag() == ErrorTag {
			return a0
		}
		t.trm = ftnsym
		t.args[0] = a0
		t.dtype = funtyp.rettype
		return t
	} else if ftnname == "parseRW" {
		// arg must be a manifest string
		arg := t.args[0]
		if arg.Tag() == Ident {
			sym := arg.Typecheck(nil, tci)
			if sym.Tag() == ErrorTag {
				return sym
			}
			sym0 := sym.(*Symbol)
			if sym0.binding != nil {
				arg = sym0.binding
			}
		}
		if arg.Tag() != Stringlit {
			return tci.Error("parseRW requires a manifest string", t)
		}
		fnsym, _ := tci.scopes.Lookup(ftnname)
		// if this parse call is in the runtime part of the xprftn, process it then
		if tci.mqseen {
			return makeFuncall(fnsym, []Term{arg}, Gzterm, t)
		}
		// otherwise, process now
		strg := arg.(*TermB).value
		parsetrm := parseString(strg)
		// todo: stop ignoring stx errors
		tci.cxt.Push(SymbolPattern)
		parsed := parsetrm.Typecheck(nil, tci)
		tci.cxt.Pop()
		if parsed.Tag() == ErrorTag {
			return parsed
		}
		return makeFuncall(fnsym, []Term{parsed}, Gzterm, t)
	} else if ftnname == "matchquery" && t.args[0].Tag() == AndandExpr {
		// here just for the case where a pattern is followed by conjoined conditions
		// At coding time, && is parsed to a list of clauses unless actively prevented by parens
		// designed to change the associativity. At some point, I need to ensure that such exprs
		// get listified (since && is associative) but for now, I'm ignoring it.
		aax := t.args[0].(*TermL)
		ptnterm = checkPattern(aax.args[0])
		// see below for TC of other && stuff
	} else if ftnname == "rewriteStoreGet" {
		// don't check but assume good
		a0 := t.args[0].Typecheck(nil, tci)
		if a0.Tag() == ErrorTag {
			return a0
		}
		t.trm = ftnsym
		t.args[0] = a0
		t.dtype = a0.Dtype()
		return t
	} else if ftnname == "rewriteStoreSet" { // 1 or 2 args
		a0 := t.args[0].Typecheck(nil, tci)
		if a0.Tag() == ErrorTag {
			return a0
		}
		if len(t.args) == 2 {
			a1 := t.args[1].Typecheck(nil, tci)
			if a1.Tag() == ErrorTag {
				return a1
			}
			t.args[1] = a1
		}
		t.trm = ftnsym
		t.args[0] = a0
		t.dtype = TypeNothing
		return t
	} else if ftnname == "rewriteStoreDeclare" {
		// args: symbol, type, expr
		if t.args[0].Tag() != Ident {
			return tci.Error("symbol expected", t.args[0])
		}
		typ := TC4Type(t.args[1], nil, tci)
		if typ.Tag() == ErrorTag {
			return tci.Error("type expected", t.args[1])
		}
		bdg := t.args[2].Typecheck(typ.(*Type), tci)
		if bdg.Tag() == ErrorTag {
			return bdg
		}
		rwsym := makeSymbol(t.args[0].(*Token).val, typ.(*Type), bdg)
		rwsym.plist.Add("var", true)
		tci.rwstoreScope.Add(rwsym, 0)
		return makeTermT(RwsDeclStmt, rwsym, TypeNothing, t.first, t.final)
	} else if ftnname == "defineGlobal" {
		a0 := t.args[0].Typecheck(Gzsym, tci)
		if a0.Tag() == ErrorTag {
			return a0
		}
		t.trm = ftnsym
		t.args[0] = a0
		t.dtype = TypeNothing
		return t
	}
	// what's left is all the pattern variants; all have a pattern as first arg except possibly matchquery
	// which is more complicated. If the previously declared ptnterm isn't nil, TC that arg. Which
	// can be symbol, string lit, list lit, list-valued expr, or xprftn call. If symbol,
	// can be ptn valued or a xprterm+xprsym
	if ptnterm == nil {
		ptnterm = checkPattern(t.args[0])
	}
	if ptnterm.Tag() == ErrorTag {
		return ptnterm
	}
	fnsym, _ := tci.scopes.Lookup(ftnname)
	switch ftnname {
	case "matchquery": // pattern with optional && clauses
		var conjoined Term
		if t.args[0].Tag() == AndandExpr {
			// the first clause has already been TC'd as a pattern; now TC the others
			aax := t.args[0].(*TermL)
			if len(aax.args) == 2 {
				conjoined = aax.args[1]
			} else {
				conjoined = makeTermL(AndandExpr, aax.args[1:], nil, aax.first, aax.final)
			}
			conjoined = conjoined.Typecheck(TypeBool, tci)
			if conjoined.Tag() == ErrorTag {
				return conjoined
			}
		}
		// TC'd term is a funcall on matchquery Symbol with pattern and conjoined (if non-nil) as args;
		// the funcall is decorated with the compiled pattern
		var ret Term
		args := []Term{ptnterm}
		if conjoined != nil {
			args = append(args, conjoined)
		}
		ret = makeFuncall(fnsym, args, TypeNothing, nil)
		cp := compilePattern(ptnterm, ret, tci.scopes.entries, false, tci.mqseen)
		if cp.Tag() == ErrorTag {
			// compilePattern doesn't record errors in tci, so do that now.
			tci.errors = append(tci.errors, cp)
		}
		tci.mqseen = true
		return cp
	case "pattern": // pattern: must be defining a symbol, don't compile yet
		pt0, isTL := ptnterm.(*TermTL)
		// wrap unless already wrapped
		if !(isTL && pt0.kind == Funcall && symbolNamed(pt0.trm, "pattern")) {
			ptnterm = makeFuncall(fnsym, []Term{ptnterm}, Gzptn, nil)
		}
		// don't compile now, it'll be compiled when used
		return ptnterm
	case "contains", "match", "matches", "matchcount":
		// each of these is pattern then 1 or more xprterms; return type is in ftn symbol (bool, list(matchinfo), integer)
		// TC'd repn is funcall of corresp ftn symbol, first arg is pattern and vbl# of args corresp to terms given.
		// This call has to be for a PEM iff #xprterms is 0. Also, match has at most 1 xprterm.
		if ftnname == "match" && len(t.args) > 2 {
			return tci.Error("match cannot be applied to multiple terms", t)
		}
		argsx := make([]Term, len(t.args))
		argsx[0] = ptnterm
		for i, a := range t.args[1:] {
			tmp := a.Typecheck(Gzterm, tci)
			if tmp.Tag() == ErrorTag {
				return tmp
			}
			argsx[i+1] = tmp
		}
		rettyp := fnsym.dtype.v.(*Ftntype).rettype
		var ret Term = makeFuncall(fnsym, argsx, rettyp, nil)
		cp := compilePattern(ptnterm, ret, tci.scopes.entries, ftnname == "matches", tci.mqseen)
		if cp.Tag() == ErrorTag {
			tci.errors = append(tci.errors, cp)
		}
		return cp
	case "matching":
		// pattern then two booleans
		searching := t.args[1].Typecheck(TypeBool, tci)
		if searching.Tag() == ErrorTag {
			return searching
		}
		if !Manifest(Simplify(searching)) {
			return tci.Error("search param to matching must be manifest", t)
		}
		binding := t.args[2].Typecheck(TypeBool, tci)
		if binding.Tag() == ErrorTag {
			return binding
		}
		if !Manifest(Simplify(binding)) {
			return tci.Error("binding param to matching must be manifest", t)
		}
		t.trm = fnsym
		t.args[0] = ptnterm
		t.args[1] = searching
		t.args[2] = binding
		t.dtype = TypeNothing
		cp := compilePattern(ptnterm, t, tci.scopes.entries, false, tci.mqseen)
		if cp.Tag() == ErrorTag {
			tci.errors = append(tci.errors, cp)
		}
		return cp
	default:
		panic("cannot happen")
	}
}

// This is called, as the name suggests, for funcalls that need special typechecking.
// It will ordinarily return a TC'd term or error, but for the specially TC'd xpr ftns, it checks
// if we're in "xpr mode" and return nil if not. So nil means "go back to std TC".
func (t *TermTL) specialFuncall(ftnname string, argcount int, cntt *Type, tci *TCInfo) Term {
	errmsg := fmt.Sprintf("%s is not implemented", ftnname)
	if argcount > 0 && len(t.args) != argcount {
		fmtstrg := "%s requires %d arguments"
		if argcount == 1 {
			fmtstrg = "%s requires one argument"
		}
		return tci.Error(fmt.Sprintf(fmtstrg, ftnname, argcount), t)
	}
	switch ftnname {
	case "always":
		if t.args[0].Tag() != TildeExpr || t.args[0].(*TermTT).arg0.Tag() != Ident {
			return tci.Error("arguments to 'always' have the wrong form", t)
		}
		// always(v~X, P(v)) has t.args[0] as the tilde expr so rhs is X; nvtoken is v.
		rhs := t.args[0].(*TermTT).arg1.Typecheck(nil, tci)
		if rhs.Tag() == ErrorTag {
			return rhs
		}
		rhsTyp := rhs.Dtype()
		nvtoken := t.args[0].(*TermTT).arg0.(*Token)
		// declare "val v = X" and add it to a local scope that's active when we eval P
		newvar := makeSymbol(nvtoken.val, rhs.Dtype(), rhs)
		localscope := makeScope()
		localscope.Add(newvar, 0)
		tci.PushScope(localscope)
		defer tci.PopScope()
		// The spec defines always as equiv to an IAAF containing assert; the guarding behavior of assert
		// is the key idea. We don't need all that internally -- just check the value, define the vbl, check
		// the predicate with guarding, and wrap or smash the dtype as needed.
		tci.EnterGuardingCxt()
		defer tci.ExitGuardingCxt(IfStmt)
		cond := t.args[1].Typecheck(TypeBool, tci)
		if cond.Tag() == ErrorTag {
			return cond
		}
		// The fallowing somewhat ugly hack looks for certain nilpossible list methods and a nonnil cond, replacing them
		// with special methods that assume non-nullity. This is not nearly as good as a design that can figure out how to
		// fix the code when non-nullity is asserted, but that's harder. The good news about this hack (and the reason it's here)
		// is that it covers a good fraction (I believe) of cases that'll arise in practice.
		if cond.Tag() == Lognot {
			condx := cond.(*TermT).arg0
			condxtg := condx.Tag()
			if (condxtg == EqExpr || condxtg == EqeqExpr) && condx.(*TermTT).arg1 == nilEntity {
				if rhs.Tag() == Funcall && rhs.(*TermTL).trm.Tag() == SymbolTag {
					calling := rhs.(*TermTL).trm.(*Symbol)
					rcvttype := calling.dtype.v.(*Ftntype).fmlargs[0]
					if rcvttype.family == TFList {
						found := false
						for _, s := range []string{"filt1", "head", "index", "last", "popb", "popf", "tail"} {
							if s == calling.ident {
								found = true
								break
							}
						}
						if found {
							nonnilsym := rcvttype.methods.Find("__" + calling.ident)
							if nonnilsym == nil {
								panic("can't happen")
							}
							rhs0 := rhs.(*TermTL)
							rhs0.trm = nonnilsym
							rhs0.dtype = rhsTyp.mainType()
							return rhs
						}
					}
				}
			}
		}
		// if P(v) involves a guard, it should now be active
		gdi := tci.findTagGuard(newvar)
		if gdi != nil { // wrap rhs in a gdref or smash its dtype
			ort := newvar.dtype.v.(*Ortype).variants[gdi.kind-2].dtype
			rhs = makeTermT(Gdref, rhs, ort, Pos(-1), Pos(-1))
		} else if rhsTyp.isNilposs() {
			guarded := tci.tryGuardfixNP(newvar)
			if guarded != newvar {
				smashDtype(rhs, rhsTyp.mainType())
			}
		}
		return rhs
	case "applyST":
		if cntt != nil && cntt != TypeNothing {
			return tci.ErrorExpect(cntt, TypeNothing, t)
		}
		// arg 0 is a type, with Gomap allowed; it'll be the target type
		tci.cxt.Push(GomapAllowed)
		tgttype := TC4Type(t.args[0], nil, tci)
		tci.cxt.Pop()
		if tgttype.Tag() == ErrorTag {
			return tgttype
		}
		// arg 1 is an "appropriate predicate" or a symchain designating an entity to translate.
		// Appropriate means 1 arg of type S, s.t. there's a STMap item from S to T.
		tg1 := t.args[1].Tag()
		if tg1 == FunLit || tg1 == Dot || tg1 == Ident {
			tgt := t.args[1].Typecheck(nil, tci)
			if tgt.Tag() == ErrorTag {
				return tgt
			}
			t.args[0] = tgttype
			tgtt := tgt.Tag()
			if tgtt == Symchain {
				return tci.Error("applyST for entity attributes is not implemented yet", t)
			}
			xsym := tgtt == SymbolTag
			if tgtt == FuninstTag || (xsym && tgt.Dtype().family == TFFtn) {
				tgtty := tgt.Dtype().v.(*Ftntype)
				if tgtty.rettype != TypeBool {
					return tci.Error("function to applyST must be a predicate", t)
				}
				if len(tgtty.fmlargs) != 1 {
					return tci.Error("function to applyST must have a single argument", t)
				}
			} else if !xsym {
				return tci.Error("cannot be the enabling condition for applyST", t)
			}
			t.args[1] = tgt
			t.trm = biScope[ftnname][0]
			t.dtype = TypeNothing
			aplyi := applyST2Stitem(tgt, tgttype.(*Type), tci.scopes)
			if aplyi == nil {
				errmsg := fmt.Sprintf("cannot find STMap item matching %s and %s", tgt.Dtype().String(), tgttype.String())
				return tci.Error(errmsg, t)
			}
			tci.scopes.stActives = append(tci.scopes.stActives, aplyi)
			return t
		} else {
			return tci.Error("cannot be an appropriate argument to applyST", t)
		}
	case "changed":
		panic("unwritten")
	case "channelMake":
		chtype := TC4Type(t.args[0], nil, tci)
		if chtype.Tag() == ErrorTag {
			return chtype
		}
		chbuff := t.args[1].Typecheck(TypeInt, tci)
		if chbuff.Tag() == ErrorTag {
			return chbuff
		}
		if chtype.Tag() == SymbolTag {
			chtype = chtype.(*Symbol).binding
		}
		if !(chtype.Tag() == TypeTag && chtype.(*Type).family == TFGochan) {
			return tci.Error("channel type required", t)
		}
		return makeFuncall(biScope["channelMake"][0], []Term{chtype, chbuff}, chtype.(*Type), nil)
	case "connected":
		ctype := TC4Type(t.args[0], nil, tci)
		if ctype.Tag() == ErrorTag {
			return ctype
		}
		ctype.Plist().Add("connected", true)
		return biScope["__EAP"][0]
	case "compile":
		argsx := make([]Term, len(t.args))
		for i, a := range t.args {
			tci.allowTilde = i > 0
			tmp := a.Typecheck(nil, tci)
			if tmp.Tag() == ErrorTag {
				tci.allowTilde = false
				return tmp
			}
			argsx[i] = tmp
		}
		tci.allowTilde = false
		return makeFuncall(biScope["compile"][0], argsx, TypeNothing, nil)
	case "load", "setwd":
		fname := t.args[0].Typecheck(TypeString, tci)
		if fname.Tag() == ErrorTag {
			return fname
		}
		return makeFuncall(biScope[ftnname][0], []Term{fname}, TypeNothing, nil)
	case "cvt", "cast", "typeAs":
		tgttype := TC4Type(t.args[1], nil, tci)
		if tgttype.Tag() == ErrorTag {
			return tgttype
		}
		if tgttype.Tag() == TypeTag { // main branch here
			tgttype0 := tgttype.(*Type)
			if ftnname == "cvt" {
				seen := []*Type{}
				if cntt != nil && !tgttype0.compat(cntt, &seen) {
					return tci.ErrorExpect(cntt, tgttype0, t)
				}
				a0 := t.args[0].Typecheck(nil, tci)
				if a0.Tag() == ErrorTag {
					return a0
				}
				// For cvt I've cooked up a special ftn that runs all the checks quickly; see comments at defn
				cvsym := tci.scopes.lookupCvt(a0.Dtype(), tgttype0)
				if cvsym != nil {
					return makeFuncall(cvsym, []Term{a0}, tgttype0, t)
				}
				return tci.Error(fmt.Sprintf("no cvt function to %s", tgttype.String()), t)
			} else if ftnname == "cast" {
				// The following is a bit of a kluge. Cast has a few uses and they're somewhat misellaneous. One
				// is to disambiguate multibound function symbols. Another is to enforce ortypes in condl exprs or
				// funargs when more specific types are inferable. A third is to invoke stmapping on specific exprs.
				// TC'g all of these without backtracking is a challenge but I always prefer that if possible. So here,
				// I'm going to assume that any cast to a ftn type is a multibound disambig. Otherwise, check without cntts
				// and see if ortype or stmapping works via local checks.
				if tgttype0.family == TFFtn {
					return t.args[0].Typecheck(tgttype0, tci)
				}
				xpr := t.args[0].Typecheck(nil, tci)
				xprt := xpr.Tag()
				if xprt == ErrorTag {
					return xpr
				}
				// first arg typechecks uncnttd; see if we can cast it, if not try stmapping
				seen := []*Type{}
				xprty := xpr.Dtype()
				if cntt != nil && !tgttype0.compat(cntt, &seen) {
					return tci.Error("cast type incompatible with constraint", t)
				}
				seen = []*Type{}
				// first, check if cast type is compat with inferred; this includes ortype genzns
				if tgttype0.compat(xprty, &seen) {
					if xprty.Equal(tgttype0) {
						// Equal types can have diffs for CG purposes, like pkg imported types
						xpr.Plist().CopyInto(&tgttype0.plist)
					} else {
						if xpr.Tag() == SymbolTag {
							xpr = makeTermT(Gdref, xpr, tgttype0, t.args[0].First(), t.args[0].Final())
						} else {
							smashDtype(xpr, tgttype0)
						}
					}
					return xpr
				}
				// no, so is the wanted type obtainable via stmapping?
				// Note that xpr may not have the form of an applyST arg but it'll never be used as such --
				// we only care about the stitem.
				aplyi := applyST2Stitem(xpr, tgttype0, tci.scopes)
				if aplyi == nil {
					errmsg := fmt.Sprintf("cannot cast %s to %s", xprty.String(), tgttype0.String())
					return tci.Error(errmsg, t)
				}
				xpr = makeTermT(Gdref, xpr, tgttype0, xpr.First(), xpr.Final())
				xpr.Plist().Add("applyitem", aplyi)
				return xpr
			} else {
				// must be typeAs with a specific type; eval here
				xpr := t.args[0].Typecheck(nil, tci)
				if xpr.Tag() == ErrorTag {
					return xpr
				}
				if xpr.Tag() == SymbolTag {
					return makeTermT(Gdref, xpr, derefGTS(tgttype0), t.first, t.final)
				} else {
					smashDtype(xpr, derefGTS(tgttype0))
					return xpr
				}
			}
		} else if ftnname == "typeAs" {
			// typeAs should get a computed type
			xpr := t.args[0].Typecheck(nil, tci)
			if xpr.Tag() == ErrorTag {
				return xpr
			}
			return makeFuncall(biScope["typeAs"][0], []Term{xpr, tgttype}, TPEntity, t)
			/*		} else if callingNamed(tgttype, "elttype") {
					// special case computed elttype, otherwise it's an error
					xpr := t.args[0].Typecheck(nil, tci)
					if xpr.Tag() == ErrorTag {
						return xpr
					}
					return makeTermT(Gdref, xpr, tgttype, t.first, t.final)
			*/
		} else {
			return tci.Error(tgttype.String()+" cannot be a type", t)
		}
	case "defined":
		defsym := biScope[ftnname][0]
		if t.args[0].Tag() != Ident {
			return tci.Error("identifier expected", t)
		}
		symstrg := makeStringTerm(t.args[0].(*Token).val, t.args[0].First(), false)
		// we're going to encode the presence of "type" by leaving typ nil, sort of like the dtype of type-valued symbols.
		var typ *Type
		if !tokenNamed(t.args[1], "type") {
			typx := TC4Type(t.args[1], nil, tci)
			if typx.Tag() == ErrorTag {
				return typx
			}
			typ = typx.(*Type)
		}
		return makeFuncall(defsym, []Term{symstrg, typ}, TypeBool, t)
	case "dimension":
		// 2 symbols, both become defined
		if t.args[0].Tag() != Ident || t.args[1].Tag() != Ident {
			return tci.Error("dimension statement must have 2 symbols as arguments", t)
		}
		dim := t.args[0].(*Token).val
		baseunit := t.args[1].(*Token).val
		// special treatment for duration: it's predefined but without a baseunit
		qtytyp := tci.scopes.lookupType(dim)
		if qtytyp != nil {
			if !(dim == "duration" && qtytyp.plist.Find("baseunit") == nil) {
				return tci.Error(fmt.Sprintf("%s is already defined as a type", dim), t)
			} else {
				qtytyp.plist.Add("baseunit", baseunit)
			}
		} else {
			qtytyp = makeSymbol(dim, nil, makeType(TFBaseQty, dim))
			qtytyp.plist.Add("baseunit", baseunit)
			tci.scopes.Add(qtytyp, 0)
		}
		tci.scopes.Add(makeSymbol(baseunit, qtytyp.binding.(*Type), makeTermB(Floatlit, "1.0", Pos(-1))), 0)
		t.dtype = TypeNothing
		return TrueLiteral
	case "enumerable": // in assert, in extend, arg is an attrib name
		if tci.cxt.Find(AssertStmt, 0, NoTerm) < 0 {
			return tci.Error("enumerable must be in assert", t)
		}
		xtndinx := tci.cxt.Find(ExtendExpr, 0, NoTerm)
		if xtndinx < 0 {
			return tci.Error("enumerable may only be asserted in extend expression", t)
		}
		basetype := (*tci.cxt)[xtndinx].plist.Find("basetype").(*Type)
		var attribsym *Symbol
		if t.args[0].Tag() == Ident {
			ident := t.args[0].(*Token).val
			attribsym = basetype.attributeLookup(ident)
		}
		if attribsym == nil {
			return tci.Error("enumerable predicate's argument must be an attribute name", t)
		}
		basetype.plist.Add("enumerable", attribsym)
		return biScope["__EAP"][0] // get special Symbol for internal use
	case "Gocall":
		if t.args[0].Tag() != Funcall {
			return tci.Error("Gocall requires a single function call argument", t)
		}
		if cntt != nil && cntt != TypeNothing {
			return tci.ErrorExpect(cntt, TypeNothing, t)
		}
		targ := t.args[0].(*TermTL)
		tmp := targ.Typecheck(nil, tci)
		if tmp.Tag() == ErrorTag {
			return tmp
		}
		t.args[0] = tmp
		tmp0 := tmp.(*TermTL)
		fnterm := tmp0.trm
		// loop through possibly nested symbols to get the funi
		for fnterm.Tag() != FuninstTag {
			if fnterm == nil || fnterm.Tag() != SymbolTag {
				return tci.Error("Unhandled case in Gocall", t)
			}
			fnterm = fnterm.(*Symbol).binding
		}
		fnterm.(*Funinst).plist.Add("noInline", true) // tag function to prevent inlining
		t.trm = biScope[ftnname][0]
		t.dtype = TypeNothing
		return t
	case "Gomethod":
		if t.args[0].Tag() != Ident {
			return tci.Error("Gomethod argument must be an identifier", t)
		}
		if tci.cxt.Find(AssertStmt, 0, NoTerm) < 0 {
			return tci.Error("Gomethod must be asserted", t)
		}
		xxinx := tci.cxt.Find(ExtendExpr, 0, NoTerm)
		if xxinx < 0 {
			return tci.Error("Gomethod must occur in a type definition", t)
		}
		gmident := t.args[0].(*Token).val
		xtndType := (*tci.cxt)[xxinx].plist.Find("basetype").(*Type)
		mthd := xtndType.methods.Find(gmident)
		if mthd == nil {
			return tci.Error("no such method", t)
		}
		mthd.plist.Add("Gomethod", true)
		return biScope["__EAP"][0]
	case "purified", "noInline", "alwaysInline":
		if tci.cxt.Find(AssertStmt, 0, NoTerm) < 0 {
			errmsg := fmt.Sprintf("%(...) must be asserted", ftnname)
			return tci.Error(errmsg, t)
		}
		if t.args[0].Tag() != Ident {
			return tci.Error("symbol expected", t)
		}
		var fn Term
		xtndInx := tci.cxt.Find(ExtendExpr, 0, NoTerm)
		if xtndInx >= 0 {
			xtndType := (*tci.cxt)[xtndInx].plist.Find("basetype").(*Type)
			fn = xtndType.methods.Find(t.args[0].(*Token).val)
		}
		if fn == nil {
			fn = t.args[0].Typecheck(TPFunction, tci)
			if fn.Tag() == ErrorTag {
				return fn
			}
		}
		if ftnname == "purified" {
			if !fn.Dtype().isMod() {
				return tci.Error(fn.String()+"doesn't need to be purified", t)
			}
			fn.Plist().Add("purified", true)
		} else if fn.Tag() == SymbolTag {
			fnsym := fn.(*Symbol)
			if fnsym.binding == nil {
				return tci.Error("cannot declare this of unbound function", t)
			}
			funi := fnsym.binding.(*Funinst)
			funi.Plist().Add(ftnname, true)
		} else {
			return tci.Error("cannot assert "+ftnname+" of non-symbol", t)
		}
		return biScope["__EAP"][0]
	case "rcvrLinked":
		if tci.cxt.Find(AssertStmt, 0, NoTerm) < 0 {
			return tci.Error("rcvrLinked(...) must be asserted", t)
		}
		flinx := tci.cxt.Find(FunLit, 0, NoTerm)
		if flinx < 0 {
			return tci.Error("rcvrLinked(...) must be in function", t)
		}
		fn := t.args[0].Typecheck(nil, tci)
		if fn.Tag() == ErrorTag {
			return fn
		}
		if fn.Tag() != SymbolTag || fn.(*Symbol).binding != nil {
			return tci.Error("rcvrLinked applies only to function arguments", t)
		}
		// todo: check the remaining conditions, which aren't currently recorded in tci.cxt
		fn.Plist().Add("rcvrLinked", true)
		return biScope["__EAP"][0]
	case "rewrite":
		// first arg can be symbol designating xprftn or funcall on same; 2nd arg optional, uncnttd.
		t.dtype = TypeNothing
		if len(t.args) == 2 {
			xpr := t.args[1].Typecheck(nil, tci)
			if xpr.Tag() == ErrorTag {
				return xpr
			}
			t.args[1] = xpr
			t.dtype = xpr.Dtype()
		}
		rulex := t.args[0].Typecheck(nil, tci)
		if rulex.Tag() == ErrorTag {
			return rulex
		}
		tag0 := rulex.Tag()
		sym := rulex
		if tag0 == Funcall {
			sym = rulex.(*TermTL).trm
		}
		if (tag0 != SymbolTag && tag0 != Funcall) || sym.Tag() != SymbolTag {
			return tci.Error("rewrite must reference or call an xprftn", t)
		}
		xfunc := sym.(*Symbol).binding.(*Funinst)
		if xfunc.plist.Find("MQP") == nil {
			return tci.Error(sym.(*Symbol).ident+"is not an xprftn", rulex)
		}
		if len(xfunc.scope.entries) > 0 && (tag0 != Funcall || len(rulex.(*TermTL).args) != len(xfunc.scope.entries)) {
			return tci.Error("this xprftn needs arguments", t.args[0])
		}
		t.args[0] = rulex
		t.trm = biScope[ftnname][0]
		if tci.scopes.topscope {
			tci.scopes.rwActives = append(tci.scopes.rwActives, rulex)
		}
		return t
	case "stringify":
		// This is here for bad reasons: because entity-wide ftns with "cutouts" for more
		// specific types don't TC correctly, as the entity-wide interpretation is found first and others
		// aren't looked for. The proper solution is for tcFuncall to always look for methods before general
		// lookup. This is a non-trivial change that might break things, so I plan to do it later.
		// todo: make the indicated changes
		arg := t.args[0].Typecheck(nil, tci)
		if arg.Tag() == ErrorTag {
			return arg
		}
		var acfn *Symbol
		seen := []*Type{}
		if cntt != nil && !TypeString.compat(cntt, &seen) {
			acfn = tryAutocvt(TypeString, cntt, tci)
		}
		atyp := arg.Dtype()
		var inDefnSym *Symbol
		if len(tci.ftnInDef) > 0 {
			inDefnSym = tci.ftnInDef[len(tci.ftnInDef)-1]
		}
		sfymthd := atyp.stringifyMethod(inDefnSym)
		sfycall := makeFuncall(sfymthd, []Term{arg}, TypeString, t)
		if acfn != nil {
			sfycall = makeFuncall(acfn, []Term{sfycall}, cntt, t)
		}
		return sfycall
	case "matchquery", "pattern", "parseRW", "contains", "match", "matching", "matches", "matchcount",
		"mksym", "mktype", "mkterm", "mkstmts", "lookup", "lookupType", "rewriteStoreGet", "rewriteStoreSet",
		"rewriteStoreDeclare", "defineGlobal":
		if !tci.inXprftn {
			return nil // this causes caller to fall through and check normal defns
		}
		return t.tcSpecialXprftns(ftnname, cntt, tci)
	case "xp":
		// any list(tuple(...)) to the corresp tuple(list, list, ...)
		t0 := t.args[0].Typecheck(nil, tci)
		if t0.Tag() == ErrorTag {
			return t0
		}
		if t0.Dtype().family == TFTuple {
			// we're going to check manually that all the attribs are enumerable; if so,
			// succeed and synth the rettype.
			tupt := t0.Dtype().v.(*Tupletype)
			allgood := true
			eltyps := make([]*Symbol, len(tupt.attribs))
			for i, a := range tupt.attribs {
				eltyp := a.dtype.Elttype()
				if eltyp == nil {
					allgood = false
					break
				}
				eltyps[i] = makeSymbol(a.ident, eltyp, nil)
			}
			if allgood {
				rettyp := makeListType(makeType(TFTuple, eltyps, false))
				xpsym := makeSymbol("xp", makeType(TFFtn, []*Type{t0.Dtype()}, rettyp, true), nil)
				return makeFuncall(xpsym, []Term{t0}, rettyp, nil)
			}
		}
	}
	return tci.Error(errmsg, t)
}
