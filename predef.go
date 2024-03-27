// Predefined symbols, types, functions, etc.

package main

var biTypreds = []*Type{TPEntity, TPQty, TPList, TPTuple,
	TPModdable, TPSituated, TPEnumerable}
var biBasetypes = []*Type{TypeNothing, TypeBit, TypeBool, TypeInt, TypeFloat,
	TypeByte, TypeLabel, TypeString, TypeNil}
var nilEntity = makeSymbol("nil", TypeNil, nil)
var gomapTypegen *Typegen // Gomap is accessed through this global and special handling in TC4Type
var listTG *Typegen       // lots of code needs to make list(T); this helps.
var spaceTG *Typegen

var gblScope = makeScope()

// This is used only this file; it's gbl because need in init and also in a ftn called from main.
var gvnscope = makeScope()

// This is an actual global, used to typecheck xprftns. Initzd here.
var XprScope = makeScope()

// The following globals are initzd in the init() fn; note that the genzd types aren't extensional -- to see if a
// Type t is xprsym it suffices to do "t == Gzsym". (And so on.)
var Gzsym *Type      // xprsym
var Gzsymlist *Type  // list(xprsym)
var Gztermlist *Type // xprterms or list(xprterm)
var Gzterm *Type     // xprterm
var Gztype *Type     // xprtype
var Gztypelist *Type // list(xprtype)
var Gzstmts *Type    // xprstmts
var Gzptn *Type
var matchany *Symbol

// The reservedInGo gbl is searched by the backend code ("identifierOutOfScope") that checks for identifier conflicts.
// There may be a few other words needing to be added.
var reservedInGo = []string{"append", "copy", "delete", "len", "make", "new"}

var compareOpNames = []string{"<", ">", "<=", ">="}

func isComparisonOp(str string) bool {
	for _, s := range compareOpNames {
		if str == s {
			return true
		}
	}
	return false
}

// Note that these initial entries write directly into biScope instead of using
// the Add method which handles multibinding. The upshot is that there must be
// no duplication in identifiers, on pain of losing bindings with same identifier.
func init() {
	// make the types corresponding to BI type predicates
	for _, ty := range biTypreds {
		tp := ty.v.(*Typred) // need this for the ident
		biScope.Add(makeSymbol(tp.ident, nil, ty))
	}
	// add BI base types to BI symtbl
	for _, ty := range biBasetypes {
		biScope.Add(makeSymbol(typeFamilyStrings[ty.family], nil, ty))
	}
	// add misc Symbols: for entity nil, start:time, etc
	biScope.Add(nilEntity)
	biScope.Add(makeSymbol("start", TypeTime, nil))
	// the BI exit ftn, also add the Error method to Goerror here
	exitfntyp := makeType(TFFtn, []*Type{TypeString}, TypeExit, true)
	exitsym := makeSymbol("exit", exitfntyp, makeFuninst(nil, nil, exitfntyp, nil))
	exitsym.plist.Add("connected", true)
	biScope.Add(exitsym)
	errorfn := makeSymbol("Error", makeType(TFFtn, []*Type{TypeGoerror}, TypeString, true), nil)
	errorfn.plist.Add("Gomethod", true)
	TypeGoerror.addMethod(errorfn)

	// make a Symbol for the typechecker to represent correctly applied effective assert predicates
	biScope.Add(makeSymbol("__EAP", TypeBool, nil))
	// make range types
	for _, intflt := range []struct {
		nm  string
		typ *Type
	}{{"rangeI", TypeInt}, {"rangeF", TypeFloat}} {
		bps := []*Symbol{makeSymbol("low", intflt.typ, nil), makeSymbol("high", intflt.typ, nil),
			makeSymbol("step", intflt.typ, nil), makeSymbol("inclusive", TypeBool, nil)}
		rt := makeType(TFTuple, bps, false)
		biScope.Add(makeSymbol(intflt.nm, nil, rt))
	}
	// make the Typegen for "list"
	eltT := makeType(TFGTS, makeGTS("LE", TPEntity))
	listT := makeType(TFList, eltT) // this will be the dtype of the Typegen.
	// This function creates and adds Symbols designating BI ftns a/w lists that the compiler will handle as primitives.
	// These are not methods of list (though some of them could be).
	var listMethodsBI = func() {
		var builtins = []struct {
			nm    string
			args  []*Type
			ret   *Type
			ismod bool
		}{
			{"__len", []*Type{listT}, TypeInt, false},
			{"__cap", []*Type{listT}, TypeInt, false},
			{"__cvtf", []*Type{TypeFloat}, TypeInt, false},
			{"__append", []*Type{listT, eltT}, listT, true},
			{"__appendDDD", []*Type{listT, listT}, listT, true},
			{"__make", []*Type{TypeInt}, listT, false},           // count --> cap, len --> 0
			{"__copy", []*Type{listT, listT}, TypeNothing, true}, // dest, src
			{"__slicex", []*Type{listT, TypeInt, TypeInt}, listT, false},
			{"__singletonSliceLit", []*Type{eltT}, listT, false},
		}
		for _, tinfo := range builtins {
			typ := makeType(TFFtn, tinfo.args, tinfo.ret, !tinfo.ismod)
			biScope.Add(makeSymbol(tinfo.nm, typ, nil))
		}
		biScope["__len"][0].plist.Add("symbeval", "listlen")
	}
	listMethodsBI()
	// create the typegen for list, initially without methods
	leSym := makeSymbol("LE", nil, eltT)
	listparams := []*Symbol{leSym}
	listparams0 := make([]Term, 1)
	listparams0[0] = eltT
	listTG = makeTypegen(listT, listparams)
	listTG.add2Cache(listparams0, listT)

	eltU := makeType(TFGTS, makeGTS("U", TPEntity))
	tmp := []Term{eltU}
	listU := listTG.Instantiate(tmp)
	listTG.add2Cache(tmp, listU) // generator slot isn't set so must do by hand

	// gvnscope is used later; it helps if its GTS symbol for LE is identical to the one in list typegen
	gvnscope.Add(leSym, 0)
	ugnc := makeSymbol("U", nil, eltU)
	gvnscope.Add(ugnc, 0)
	// The next function creates the methods of list. At this point, they're going to have Funinsts but the Funinsts
	// won't have bodies. That'll get fixed later, when parsing and typechecking are sufficiently set up, by calling
	// initLateBIFunctions. Generally, these ftn bodies will use the ftns just defined in listMethodsBI.
	type mbinfo struct {
		ident    string
		typ      *Type    // always a ftn type
		argnames []string // same length as fmlargs slot of typ
	}
	var listMethods = func() {
		mapthunk := makeType(TFFtn, []*Type{eltT, TypeInt}, eltU, true)
		filtthunk := makeType(TFFtn, []*Type{eltT, TypeInt}, TypeBool, true)
		nplt := makeNilposs(listT)
		npt := makeNilposs(eltT)
		npi := makeNilposs(TypeInt)
		npu := makeNilposs(eltU)
		var mthdBodies = []mbinfo{
			{"+", makeType(TFFtn, []*Type{listT, listT}, listT, true), []string{"lst", "other"}},
			{"+", makeType(TFFtn, []*Type{listT, eltT}, listT, true), []string{"lst", "elt"}},
			{"__eq", makeType(TFFtn, []*Type{listT, listT}, TypeBool, true), []string{"a", "b"}},
			{"__filt1", makeType(TFFtn, []*Type{listT, filtthunk}, npt, true), []string{"lst", "fn"}},
			{"__head", makeType(TFFtn, []*Type{listT}, npt, true), []string{"lst"}},
			{"__index", makeType(TFFtn, []*Type{listT, eltT}, npi, true), []string{"lst", "elt"}},
			{"__last", makeType(TFFtn, []*Type{listT}, npt, true), []string{"lst"}},
			{"__popb", makeType(TFFtn, []*Type{listT}, npt, false), []string{"lst"}},
			{"__popf", makeType(TFFtn, []*Type{listT}, npt, false), []string{"lst"}},
			{"__tail", makeType(TFFtn, []*Type{listT}, nplt, true), []string{"lst"}},
			{"append", makeType(TFFtn, []*Type{listT, listT}, listT, false), []string{"lst", "other"}},
			{"contains", makeType(TFFtn, []*Type{listT, listT}, TypeBool, true), []string{"l1", "l2"}},
			{"copy", makeType(TFFtn, []*Type{listT}, listT, true), []string{"lst"}},
			{"copy", makeType(TFFtn, []*Type{listT, TypeInt, TypeInt, TypeInt}, TypeNothing, false), []string{"lst", "from", "to", "cnt"}},
			{"copyFrom", makeType(TFFtn, []*Type{listT, TypeInt, listT}, TypeNothing, false), []string{"lst", "ofs", "from"}},
			{"count", makeType(TFFtn, []*Type{listT}, TypeInt, true), []string{"lst"}},
			{"disjoint", makeType(TFFtn, []*Type{listT, listT}, TypeBool, true), []string{"l1", "l2"}},
			{"filt", makeType(TFFtn, []*Type{listT, filtthunk}, listT, true), []string{"lst", "fn"}},
			{"filt1", makeType(TFFtn, []*Type{listT, filtthunk}, npt, true), []string{"lst", "fn"}},
			{"filt1x", makeType(TFFtn, []*Type{listT, filtthunk, mapthunk}, npu, true), []string{"lst", "fn", "xfm"}},
			{"filtx", makeType(TFFtn, []*Type{listT, filtthunk, mapthunk}, listU, true), []string{"lst", "fn", "xfm"}},
			{"head", makeType(TFFtn, []*Type{listT}, npt, true), []string{"lst"}},
			{"in", makeType(TFFtn, []*Type{listT, eltT}, TypeBool, true), []string{"lst", "elt"}},
			{"index", makeType(TFFtn, []*Type{listT, eltT}, npi, true), []string{"lst", "elt"}},
			{"insert", makeType(TFFtn, []*Type{listT, TypeInt, listT}, listT, false), []string{"lst", "inx", "other"}},
			{"insert", makeType(TFFtn, []*Type{listT, TypeInt, eltT}, listT, false), []string{"lst", "inx", "elt"}},
			{"last", makeType(TFFtn, []*Type{listT}, npt, true), []string{"lst"}},
			{"lvalindex", makeType(TFFtn, []*Type{listT, TypeInt, eltT}, TypeNothing, false), []string{"lst", "inx", "elt"}},
			{"maplst", makeType(TFFtn, []*Type{listT, mapthunk}, listU, true), []string{"lst", "fn"}},
			{"popb", makeType(TFFtn, []*Type{listT}, npt, false), []string{"lst"}},
			{"popf", makeType(TFFtn, []*Type{listT}, npt, false), []string{"lst"}},
			{"pushb", makeType(TFFtn, []*Type{listT, eltT}, listT, false), []string{"lstpb", "eltpb"}},
			{"pushf", makeType(TFFtn, []*Type{listT, eltT}, listT, false), []string{"lst", "elt"}},
			{"removeElt", makeType(TFFtn, []*Type{listT, eltT}, TypeNothing, false), []string{"lst", "elt"}},
			{"removeIndex", makeType(TFFtn, []*Type{listT, TypeInt}, TypeNothing, false), []string{"lst", "inx"}},
			{"removeSeq", makeType(TFFtn, []*Type{listT, TypeInt, TypeInt}, TypeNothing, false), []string{"lst", "lo", "hi"}},
			{"reverse", makeType(TFFtn, []*Type{listT}, listT, true), []string{"lst"}},
			{"rvalindex", makeType(TFFtn, []*Type{listT, TypeInt}, eltT, true), []string{"lst", "inx"}},
			{"select", makeType(TFFtn, []*Type{listT, TypeInt, TypeInt}, listT, true), []string{"lst", "lo", "hi"}},
			{"stringify", makeType(TFFtn, []*Type{listT}, TypeString, true), []string{"arg"}},
			{"swap", makeType(TFFtn, []*Type{listT, TypeInt, TypeInt}, TypeNothing, false), []string{"lst", "a", "b"}},
			{"tail", makeType(TFFtn, []*Type{listT}, nplt, true), []string{"lst"}},
			{"uniq", makeType(TFFtn, []*Type{listT}, listT, true), []string{"lst"}},
		}
		for _, mb := range mthdBodies {
			args := make([]*Symbol, len(mb.argnames))
			funtyp := mb.typ.v.(*Ftntype)
			for i, nm := range mb.argnames {
				args[i] = makeSymbol(nm, funtyp.fmlargs[i], nil)
			}
			funi := makeFuninst(makeScope(), args, mb.typ, nil)
			for _, fa := range funi.funargs {
				funi.scope.Add(fa, 0)
			}
			funi.plist.Add("biFtn", true) // various things want to know (xprftn matching, stmapping, etc)
			// mark the rcvr of mod ftns as moddable; add asgnftn tag as needed (not for always-inlined)
			if !funtyp.pure {
				args[0].plist.Add("var", true)
				if mb.ident == "insert" || mb.ident == "removeSeq" || mb.ident == "removeIndex" {
					funi.plist.Add("asgnftn", true)
				}
			}
			// don't use addMethod here because that doesn't add things in order
			mthdsym := makeSymbol(mb.ident, mb.typ, funi)
			listTG.dtype.methods = append(listTG.dtype.methods, SymX{mthdsym, 0})
		}
		listTG.dtype.methods.Find("count").plist.Add("symbeval", "listlen")
		// since we had to create list(U) before defining methods on list(LE) we need to fix that now
		lstuType := listTG.insts[1][1].(*Type)
		for _, sx := range listTG.dtype.methods {
			czi := makeConczInfo(listTG.funargs, []Term{eltU})
			czi.tgen = listTG
			mthdtyp := czi.handleType(sx.sym.dtype)
			mthdbdg := czi.concretize(sx.sym.binding)
			mthdsym := makeSymbol(sx.sym.ident, mthdtyp, mthdbdg)
			lstuType.methods = append(lstuType.methods, SymX{mthdsym, 0})
		}
	}
	listMethods()
	// At this point, the list methods are in the dtype of listTG but they don't have bodies (ie code).
	// That will be added at the beginning of main, when we're ready to parse, TC, etc.
	// now that listtg is finished, install the generator pointer so we'll get caching
	listTG.dtype.generator = listTG

	// make the Typegen for "space"
	seltT := makeType(TFGTS, makeGTS("SE", TPEntity))
	slen := makeSymbol("spcdim", TypeInt, nil)
	spaceT := makeType(TFSpace, seltT, slen) // this will be the dtype of the Typegen.
	seSym := makeSymbol("SE", nil, seltT)
	spcparams := []*Symbol{seSym, slen}
	spcparams0 := make([]Term, 2)
	spcparams0[0] = seltT
	spcparams0[1] = slen
	spaceTG = makeTypegen(spaceT, spcparams)
	spaceTG.add2Cache(spcparams0, spaceT)
	var spaceMethods = func() {
		var mthdBodies = []mbinfo{
			{"copy", makeType(TFFtn, []*Type{spaceT}, spaceT, true), []string{"spc"}},
			{"count", makeType(TFFtn, []*Type{spaceT}, TypeInt, true), []string{"spc"}},
			{"lvalindex", makeType(TFFtn, []*Type{spaceT, TypeInt, seltT}, TypeNothing, false),
				[]string{"spc", "elt", "value"}},
			{"move", makeType(TFFtn, []*Type{spaceT, TypeInt, TypeInt, TypeInt}, TypeNothing, false),
				[]string{"spc", "lo", "hi", "dest"}},
			{"rvalindex", makeType(TFFtn, []*Type{spaceT, TypeInt}, seltT, true), []string{"spc", "inx"}},
		}
		for _, mb := range mthdBodies {
			args := make([]*Symbol, len(mb.argnames))
			funtyp := mb.typ.v.(*Ftntype)
			for i, nm := range mb.argnames {
				args[i] = makeSymbol(nm, funtyp.fmlargs[i], nil)
			}
			// mark the rcvr of mod ftns as moddable
			if !funtyp.pure {
				args[0].plist.Add("var", true)
			}
			funi := makeFuninst(nil, args, mb.typ, nil)
			funi.plist.Add("biFtn", true)
			// don't use addMethod here because that doesn't add things in order
			mthdsym := makeSymbol(mb.ident, mb.typ, funi)
			spaceTG.dtype.methods = append(spaceTG.dtype.methods, SymX{mthdsym, 0})
		}
	}
	spaceMethods()
	spaceTG.dtype.generator = spaceTG

	biScope.Add(makeSymbol("list", nil, listTG))
	biScope.Add(makeSymbol("space", nil, spaceTG))

	var makeSingletonCall = func(fn *Symbol, args []*Symbol, rtyp *Type) *TermL {
		argterms := make([]Term, len(args))
		for i, x := range args {
			argterms[i] = x
		}
		fcall := makeFuncall(fn, argterms, rtyp, nil)
		return makeScopedTermL(Stmts, makeScope(), []Term{fcall}, rtyp, Pos(-1), Pos(-1))
	}

	// define ftn to create methods on various basic types
	var basictypeMethods = func() {
		arith := []string{"+", "-", "*", "/", "%"}
		intint := []*Type{TypeInt, TypeInt}
		fltflt := []*Type{TypeFloat, TypeFloat}
		strgstrg := []*Type{TypeString, TypeString}
		bytebyte := []*Type{TypeByte, TypeUint8}
		u8u8 := []*Type{TypeUint8, TypeUint8}
		var sym *Symbol
		for i, nm := range arith {
			typ := makeType(TFFtn, intint, TypeInt, true)
			sym = makeSymbol(nm, typ, nil)
			TypeInt.addMethod(sym)
			if i == 0 {
				typ = makeType(TFFtn, strgstrg, TypeString, true)
				funi := makeFuninst(nil, nil, typ, nil)
				funi.plist.Add("biFtn", true)
				sym = makeSymbol(nm, typ, funi)
				TypeString.addMethod(sym)
			}
			if i < 4 {
				typ = makeType(TFFtn, fltflt, TypeFloat, true)
				funi := makeFuninst(nil, nil, typ, nil)
				funi.plist.Add("biFtn", true)
				sym = makeSymbol(nm, typ, funi)
				TypeFloat.addMethod(sym)
			}
			if i < 2 {
				typ = makeType(TFFtn, bytebyte, TypeUint8, true)
				sym = makeSymbol(nm, typ, nil)
				TypeByte.addMethod(sym)
				typ = makeType(TFFtn, u8u8, TypeUint8, true)
				sym = makeSymbol(nm, typ, nil)
				TypeUint8.addMethod(sym)
			}
		}
		for _, nm := range compareOpNames {
			typ := makeType(TFFtn, intint, TypeBool, true)
			funi := makeFuninst(nil, nil, typ, nil)
			funi.plist.Add("biFtn", true)
			sym = makeSymbol(nm, typ, funi)
			TypeInt.addMethod(sym)
			typ = makeType(TFFtn, fltflt, TypeBool, true)
			funi = makeFuninst(nil, nil, typ, nil)
			funi.plist.Add("biFtn", true)
			sym = makeSymbol(nm, typ, funi)
			TypeFloat.addMethod(sym)
			typ = makeType(TFFtn, strgstrg, TypeBool, true)
			funi = makeFuninst(nil, nil, typ, nil)
			funi.plist.Add("biFtn", true)
			sym = makeSymbol(nm, typ, funi)
			TypeString.addMethod(sym)
			typ = makeType(TFFtn, bytebyte, TypeBool, true)
			funi = makeFuninst(nil, nil, typ, nil)
			funi.plist.Add("biFtn", true)
			sym = makeSymbol(nm, typ, funi)
			TypeByte.addMethod(sym)
		}
		for _, nm := range []string{"floor", "ceiling", "round"} {
			typ := makeType(TFFtn, []*Type{TypeFloat}, TypeInt, true)
			args := []*Symbol{makeSymbol("flt", TypeFloat, nil)}
			funi := makeFuninst(nil, args, typ, nil)
			funi.plist.Add("biFtn", true)
			sym = makeSymbol(nm, typ, funi)
			TypeFloat.addMethod(sym)
		}
		for _, ty := range []*Type{TypeInt, TypeFloat} {
			ftyp := makeType(TFFtn, []*Type{ty}, TypeString, true)
			args := []*Symbol{makeSymbol("arg", ty, nil)}
			scp := makeScope()
			scp.Add(args[0], 0)
			funi := makeFuninst(scp, args, ftyp, nil)
			ty.addMethod(makeSymbol("stringify", ftyp, funi))
		}
		typ := makeType(TFFtn, []*Type{TypeInt}, TypeFloat, true)
		funi := makeFuninst(nil, nil, typ, nil)
		sym = makeSymbol("to_f", typ, funi)
		biScope.Add(sym)
		// todo: add pow BI's for float
		typ = makeType(TFFtn, []*Type{TypeInt, TypeInt}, TypeInt, true)
		sym = makeSymbol("pow", typ, nil)
		biScope.Add(sym)
		// more string methods: count, rvalindex
		typ = makeType(TFFtn, []*Type{TypeString}, TypeInt, true)
		args := []*Symbol{makeSymbol("strg", TypeString, nil)}
		funi = makeFuninst(nil, args, typ, nil)
		funi.plist.Add("biFtn", true)
		sym = makeSymbol("count", typ, funi)
		TypeString.addMethod(sym)
		typ = makeType(TFFtn, []*Type{TypeString, TypeInt}, TypeByte, true)
		args = []*Symbol{makeSymbol("strg", TypeString, nil), makeSymbol("inx", TypeInt, nil)}
		funi = makeFuninst(nil, args, typ, nil)
		funi.plist.Add("biFtn", true)
		sym = makeSymbol("rvalindex", typ, funi)
		TypeString.addMethod(sym)
		typ = makeType(TFFtn, []*Type{TypeString, TypeInt, TypeInt}, TypeString, true)
		args = []*Symbol{makeSymbol("strg", TypeString, nil), makeSymbol("lo", TypeInt, nil),
			makeSymbol("hi", TypeInt, nil)}
		sslcx := makeSymbol("__slicex", makeType(TFFtn, []*Type{TypeString, TypeInt, TypeInt}, TypeString, true), nil)
		funi = makeFuninst(nil, args, typ, makeSingletonCall(sslcx, args, TypeString))
		funi.plist.Add("biFtn", true)
		sym = makeSymbol("select", typ, funi)
		TypeString.addMethod(sym)
	}
	basictypeMethods()

	// BI autocvt ftns
	var biAutocvt = func(src, dest *Type) {
		actyp := makeType(TFFtn, []*Type{src}, dest, true)
		acfuni := makeFuninst(nil, nil, actyp, nil)
		biScope.Add(makeSymbol("autocvt", acfuni.dtype, acfuni))
	}
	lbyte := listTG.Instantiate([]Term{TypeByte})
	biAutocvt(TypeInt, TypeFloat)
	biAutocvt(TypeInt64, TypeFloat)
	biAutocvt(TypeString, lbyte)
	biAutocvt(biScope["rangeI"][0].binding.(*Type), listTG.Instantiate([]Term{TypeInt}))
	biAutocvt(biScope["rangeF"][0].binding.(*Type), listTG.Instantiate([]Term{TypeFloat}))
	biAutocvt(TypeInt64, TypeInt)
	biAutocvt(TypeInt, TypeInt64)
	biAutocvt(TypeByte, TypeInt)
	biAutocvt(TypeInt, TypeByte) // I'm not happy with autocvts going both ways but for integer<->byte it may be ok
	biAutocvt(TypeUint8, TypeByte)
	biAutocvt(TypeByte, TypeUint8)
	biAutocvt(TypeUint8, TypeInt)
	biAutocvt(TypeLabel, TypeString)
	biAutocvt(TypeGoerrorBase, TypeString)

	biScope.Add(makeSymbol("to_s", makeType(TFFtn, []*Type{lbyte}, TypeString, true), nil))

	// BI copy ftn, zerolist ftn, plus cvt both ways between string and both label and list(byte) and
	// a bunch of specials for handling generic lists and tuples
	tupT := makeType(TFGTS, makeGTS("TT", TPTuple))
	cptyp := makeType(TFFtn, []*Type{tupT}, tupT, true)
	cpsym := makeSymbol("copy", cptyp, nil)
	cpscope := makeScope()
	cpscope.Add(makeSymbol("TT", nil, tupT), 0)
	cpsym.plist.Add("givenscope", cpscope)
	biScope.Add(cpsym)
	entT := makeType(TFGTS, makeGTS("X", TPEntity))
	zltyp := makeType(TFFtn, []*Type{TypeInt}, entT, true)
	zlsym := makeSymbol("zerolist", zltyp, nil)
	zlscope := makeScope()
	zlscope.Add(makeSymbol("X", nil, entT), 0)
	zlsym.plist.Add("givenscope", zlscope)
	zlsym.plist.Add("mustCntt", true) // require a rettype
	biScope.Add(zlsym)
	cvtyp1 := makeType(TFFtn, []*Type{lbyte}, TypeString, true)
	biScope.Add(makeSymbol("cvt", cvtyp1, nil))
	cvtyp2 := makeType(TFFtn, []*Type{TypeString}, lbyte, true)
	biScope.Add(makeSymbol("cvt", cvtyp2, nil))
	cvtyp1 = makeType(TFFtn, []*Type{TypeLabel}, TypeString, true)
	biScope.Add(makeSymbol("cvt", cvtyp1, nil))
	cvtyp2 = makeType(TFFtn, []*Type{TypeString}, TypeLabel, true)
	biScope.Add(makeSymbol("cvt", cvtyp2, nil))
	tatyp := makeType(TFFtn, []*Type{}, TPEntity, true) // Note: TC'd with special handling, 2nd arg is a type
	biScope.Add(makeSymbol("typeAs", tatyp, nil))

	// BI print, println, Gocall, ...
	// These are all TC'd manually, with specialFtnMap
	purefn := makeType(TFFtn, []*Type{}, TypeNothing, true)
	qiv := &QIVal{nil, false, 1, "fmt", "fmt"}
	printSupportIds := []string{"print", "println", "__sprintf"}
	for _, id := range printSupportIds {
		prsym := makeSymbol(id, purefn, nil)
		prsym.plist.Add("pkgsym", qiv)
		prsym.plist.Add("connected", true)
		prsym.plist.Add("varany", true)
		biScope.Add(prsym)
	}
	biScope.Add(makeSymbol("Gocall", purefn, nil))
	biScope.Add(makeSymbol("rewrite", purefn, nil))
	biScope.Add(makeSymbol("applyST", purefn, nil))
	biScope.Add(makeSymbol("compile", purefn, nil))
	strnothing := makeType(TFFtn, []*Type{TypeString}, TypeNothing, true)
	biScope.Add(makeSymbol("load", strnothing, nil))
	biScope.Add(makeSymbol("setwd", strnothing, nil))

	// BI qty
	biScope.Add(makeSymbol("duration", nil, makeType(TFBaseQty, "duration")))

	// Bits64
	// Note: the ftns a/w bits64 were originally methods and I've left the vbl names as they were. But
	// I changed the ftns to std BI ftns because it's not always clear that the rcvr arg is a bits64 and method
	// lookup doesn't autocvt.
	bits64 := spaceTG.Instantiate([]Term{TypeBit, makeIntlit(64)})
	biScope.Add(makeSymbol("bits64", nil, bits64))
	b64mthds := []struct{ mnm, argnm string }{
		{"lshift", "amt"},
		{"rshift", "amt"},
		{"and", "other"},
		{"andnot", "other"},
		{"or", "other"},
		{"xor", "other"},
		{"bitsNot", ""},
	}
	for _, b64mpair := range b64mthds {
		fmlargs := make([]*Type, 2)
		funiargs := make([]*Symbol, 2)
		fmlargs[0] = bits64
		funiargs[0] = makeSymbol("self", bits64, nil)
		scp := makeScope()
		scp.Add(funiargs[0], 0)
		if b64mpair.argnm == "amt" {
			fmlargs[1] = TypeInt
			funiargs[1] = makeSymbol("amt", TypeInt, nil)
			scp.Add(funiargs[1], 0)
		} else if b64mpair.argnm == "other" {
			fmlargs[1] = bits64
			funiargs[1] = makeSymbol("other", bits64, nil)
			scp.Add(funiargs[1], 0)
		} else {
			fmlargs = fmlargs[:1] // bitsNot is single arg
			funiargs = funiargs[:1]
		}
		ftyp := makeType(TFFtn, fmlargs, bits64, true)
		funi := makeFuninst(scp, funiargs, ftyp, nil)
		funi.plist.Add("biFtn", true)
		fsym := makeSymbol(b64mpair.mnm, ftyp, funi)
		fsym.plist.Add("Operator", true)
		biScope.Add(fsym)
	}
	// b64mask = \(n:integer) -> bits64
	ftyp := makeType(TFFtn, []*Type{TypeInt}, bits64, true)
	scp := makeScope()
	funiargs := []*Symbol{makeSymbol("n", TypeInt, nil)}
	funi := makeFuninst(scp, funiargs, ftyp, nil)
	funi.plist.Add("biFtn", true)
	scp.Add(funiargs[0], 0)
	fsym := makeSymbol("b64mask", ftyp, funi)
	fsym.plist.Add("Operator", true)
	biScope.Add(fsym)
	// bitnum = \(n:integer) -> bits64
	ftyp = makeType(TFFtn, []*Type{TypeInt}, bits64, true)
	scp = makeScope()
	funiargs = []*Symbol{makeSymbol("n", TypeInt, nil)}
	funi = makeFuninst(scp, funiargs, ftyp, nil)
	funi.plist.Add("biFtn", true)
	scp.Add(funiargs[0], 0)
	fsym = makeSymbol("bitnum", ftyp, funi)
	fsym.plist.Add("Operator", true)
	biScope.Add(fsym)
	// cvt = \(b:bits64) -> integer
	ftyp = makeType(TFFtn, []*Type{bits64}, TypeInt, true)
	scp = makeScope()
	funiargs = []*Symbol{makeSymbol("b", bits64, nil)}
	funi = makeFuninst(scp, funiargs, ftyp, nil)
	funi.plist.Add("biFtn", true)
	scp.Add(funiargs[0], 0)
	biScope.Add(makeSymbol("cvt", ftyp, funi))
	biAutocvt(TypeInt, bits64)

	// Gomap typegen
	keyT := makeType(TFGTS, makeGTS("KT", TPEntity))
	valueT := makeType(TFGTS, makeGTS("VT", TPEntity))
	mapT := makeType(TFGomap, keyT, valueT) // this will be the dtype of the Typegen.
	gomapArgs := make([]*Symbol, 2)
	gomapArgs[0] = makeSymbol("KT", nil, keyT)
	gomapArgs[1] = makeSymbol("VT", nil, valueT)
	gomapTypegen = makeTypegen(mapT, gomapArgs)
	mapT.generator = gomapTypegen
	biScope.Add(makeSymbol("Gomap", nil, gomapTypegen))
	kvtupT := makeType(TFTuple, []*Symbol{makeSymbol("key", keyT, nil), makeSymbol("value", valueT, nil)}, false)
	// make the Gomap methods
	type gomapMthdInfo struct {
		ident    string
		isMod    bool
		args     []*Type
		ret      *Type
		argnames []string
	}
	keyTList := listTG.Instantiate([]Term{keyT})
	valTList := listTG.Instantiate([]Term{valueT})
	kvtuplist := listTG.Instantiate([]Term{kvtupT})
	var gomapMthds = []gomapMthdInfo{
		// rcvr type to be put into args
		{"count", false, []*Type{nil}, TypeInt, nil},
		{"delete", true, []*Type{nil, keyT}, TypeNothing, nil},
		{"init2const", true, []*Type{nil, keyTList, valueT}, mapT, nil},
		{"keys", false, []*Type{nil}, keyTList, nil},
		{"lithook", true, []*Type{nil, kvtuplist}, mapT, []string{"lstargs"}},
		{"lvalindex", true, []*Type{nil, keyT, valueT}, TypeNothing, []string{"inx", "rhs"}},
		{"rvalindex", false, []*Type{nil, keyT}, makeNilposs(valueT), []string{"inx"}},
		{"rvalindexWD", false, []*Type{nil, keyT, valueT}, valueT, nil},
		{"values", false, []*Type{nil}, valTList, nil},
	}
	for _, gmm := range gomapMthds {
		gmm.args[0] = mapT
		mthdtyp := makeType(TFFtn, gmm.args, gmm.ret, !gmm.isMod)
		var bdg Term
		// some methods need funinsts and rvalindex wants to be "multiret"
		if gmm.argnames != nil {
			fmls := make([]*Symbol, len(gmm.args))
			fmls[0] = makeSymbol("self", mapT, nil)
			fmls[1] = makeSymbol(gmm.argnames[0], gmm.args[1], nil)
			if len(gmm.argnames) == 2 {
				fmls[2] = makeSymbol(gmm.argnames[1], gmm.args[2], nil)
			}
			bdg = makeFuninst(makeScope(), fmls, mthdtyp, nil)
			for _, fml := range fmls {
				bdg.(*Funinst).scope.Add(fml, 0)
			}
		}
		msym := makeSymbol(gmm.ident, mthdtyp, bdg)
		if gmm.ident == "rvalindex" {
			msym.plist.Add("multiret", true)
		}
		mapT.addMethod(msym)
	}

	// Gochannel
	chaneltT := makeType(TFGTS, makeGTS("CE", TPEntity))
	chantyp := makeType(TFGochan, chaneltT, "both")
	chrcvtyp := makeType(TFFtn, []*Type{chantyp}, chaneltT, true)
	chsendtyp := makeType(TFFtn, []*Type{chantyp, chaneltT}, TypeNothing, true)
	chmaketyp := makeType(TFFtn, []*Type{chaneltT, TypeLabel, TypeInt}, chantyp, true)
	biScope.Add(makeSymbol("channelReceive", chrcvtyp, nil))
	biScope.Add(makeSymbol("channelSend", chsendtyp, nil))
	biScope.Add(makeSymbol("channelMake", chmaketyp, nil))

	// xprftn special ftns, genzd types, etc.
	var xprftnDefs = func() {
		// genzd types first
		gztypenames := []string{"sym", "term", "terms", "ftn", "stmts", "ptn", "type"}
		gztypes := make([]*Type, len(gztypenames))
		for i, nm := range gztypenames {
			aident := "__xpr" + nm
			ident := "xpr" + nm
			typ := makeType(TFTuple, []*Symbol{makeSymbol(aident, TypeNothing, nil)}, false)
			typ.cachedString = ident
			gztypes[i] = typ
			XprScope.Add(makeSymbol(ident, nil, typ), 0)
		}
		Gzsym = gztypes[0]
		Gzterm = gztypes[1]
		Gzstmts = gztypes[4]
		Gzptn = gztypes[5]
		Gztype = gztypes[6]
		lsym := listTG.Instantiate([]Term{Gzsym})
		Gzsymlist = lsym
		lterm := listTG.Instantiate([]Term{Gzterm})
		Gztermlist = lterm
		matchany = makeSymbol("_", Gzterm, nil)
		// TC needs this so make it available in a sneaky way
		XprScope.Add(makeSymbol("__lterm", nil, lterm), 0)
		ltype := makeType(TFList, Gztype)
		Gztypelist = ltype
		// Then the special BI ftns. The following table leaves the args empty for special cases.
		type xprinfo struct {
			ident string
			args  []*Type
			ret   *Type
		}
		var biftns = []xprinfo{
			{"matchquery", []*Type{}, TypeNothing},
			{"skipIf", []*Type{TypeBool}, TypeNothing},
			{"pattern", []*Type{}, Gzptn},
			{"match", []*Type{}, TypeBool},
			{"contains", []*Type{}, TypeBool},
			{"matchcount", []*Type{}, TypeInt},
			{"matches", []*Type{}, lterm},
			{"matching", []*Type{Gzptn, TypeBool, TypeBool}, lterm},
			{"methods", []*Type{Gztype}, lsym},
			{"methodNamed", []*Type{Gztype, TypeLabel}, Gzsym},
			{"pkgSymbol", []*Type{TypeString, TypeString}, Gzsym},
			{"lookup", []*Type{TypeLabel}, Gzsym},
			{"lookupType", []*Type{TypeLabel}, Gztype},
			{"rewriteStoreGet", []*Type{Gzterm}, Gzterm},
			{"rewriteStoreSet", []*Type{}, TypeNothing},
			{"rewriteStoreDeclare", []*Type{Gzsym, Gztype, Gzterm}, TypeNothing},
			{"defineGlobal", []*Type{Gzsym}, TypeNothing},
			{"handlers", []*Type{Gztype}, lterm},
			{"asserts", []*Type{Gztype}, lterm},
			{"attributes", []*Type{Gztype}, lsym},
			{"argtypes", []*Type{Gztype}, ltype},
			{"rettype", []*Type{Gztype}, Gztype},
			{"family", []*Type{Gztype}, TypeLabel},
			{"purity", []*Type{Gztype}, TypeLabel},
			{"elttype", []*Type{Gztype}, Gztype},
			{"dimension", []*Type{Gztype}, TypeInt},
			{"type", []*Type{Gzterm}, Gztype},
			{"ident", []*Type{Gzsym}, TypeLabel},
			{"fmlargs", []*Type{Gzterm}, lsym},
			{"defn", []*Type{Gzterm}, Gzterm},
			{"termtag", []*Type{Gzterm}, TypeLabel},
			{"arg0", []*Type{Gzterm}, Gzterm},
			{"arg1", []*Type{Gzterm}, Gzterm},
			{"arg2", []*Type{Gzterm}, Gzterm},
			{"args", []*Type{Gzterm}, lterm},
			{"stmts", []*Type{Gzstmts}, lterm},
			{"uses", []*Type{Gzterm}, lterm},
			{"usescount", []*Type{Gzterm, Gzterm}, TypeInt},
			{"defs", []*Type{Gzterm}, lsym},
			{"mods", []*Type{Gzterm}, lterm},
			{"anyConnected", []*Type{Gzstmts}, TypeBool},
			{"source", []*Type{Gzterm}, TypeString},

			{"subst", []*Type{Gzstmts, lterm, lterm}, Gzstmts},
			{"typesubst", []*Type{Gztype, Gztype, Gztype}, Gztype},
			{"parseRW", []*Type{TypeString}, Gzterm},
			{"gensym", []*Type{}, TypeLabel},
			{"typedAs", []*Type{Gzterm, Gztype}, Gzterm},
			{"renameLocals", []*Type{Gzstmts, Gzstmts}, Gzstmts},
			{"mktype", []*Type{}, Gztype},
			{"mktypex", []*Type{Gztype, lterm, lterm, lterm}, Gztype},
			{"mkterm", []*Type{}, Gzterm},
			{"mksym", []*Type{}, Gzsym},
			{"mkftn", []*Type{Gztype, lsym, Gzstmts}, Gzterm},
			{"mkstmts", []*Type{lterm}, Gzstmts},
			{"ensure", []*Type{Gzterm}, TypeNothing},
		}
		for _, bif := range biftns {
			typ := makeType(TFFtn, bif.args, bif.ret, true)
			XprScope.Add(makeSymbol(bif.ident, typ, nil), 0)
		}

	}
	xprftnDefs()
	// a couple of manifest BI ftns
	manityp := makeType(TFFtn, []*Type{TPEntity}, TypeBool, true)
	biScope.Add(makeSymbol("manifest", manityp, nil))
	deftyp := makeType(TFFtn, []*Type{}, TypeBool, true)
	// this one needs specialftn status
	biScope.Add(makeSymbol("defined", deftyp, nil))
}

func parseBody(defn string) Term {
	// create scanner, parser for mb.defn; parse and TC to Funinst, then asgn that gnc Funinst to appropriate list method.
	parser := setupStringParser(defn, true)
	parser.allowUnderscores = true
	parser.enter4comments()
	nxterm := parser.stmts(false, RBRACE)
	parser.exit4comments()
	return nxterm
}

type mthdbody struct {
	ident    string
	hasGiven bool
	defn     string
}

// listMthdBodies has list defns that must be in the same order as the listMthdBodies table above in listMethods.
// The idents are mostly for documentation. An empty string means no definition is done, it's for methods that are
// handled natively by the compiler.
var listMthdBodies = []mthdbody{
	{"+", false, `{var rslt:list(LE) = __make(count(lst)+count(other))
		__copy(rslt, lst)
		__copy(__slicex(rslt, count(lst), count(rslt)), other)
		rslt
}`},
	{"+", false, `{ var rslt:list(LE) = __make(count(lst)+1); __copy(rslt, lst); rslt[count(lst)] = elt; rslt }`},
	{"__eq", false, `{ var accumd = false; if(count(a) == count(b)) {
	accumd = true; each(ii^(0...count(a))) if(a[ii] != b[ii]) {accumd = false; break} }; accumd }`},
	{"__filt1", false, `{
		var inx = 0
		var ret:LE
		each(elt^lst) { if(fn(elt, inx)) { ret = elt; break }; inx += 1 }
		ret
}`},
	{"__head", false, `{ lst[0] }`},
	{"__index", false, `{ var rslt = 0;
	each(x^lst) { if(x == elt) break; rslt += 1 }
	rslt }`},
	{"__last", false, `{ lst[lst.count-1] }`},
	{"__popb", false, `{ val lastinx = count(lst)-1; val rslt = lst[lastinx]; lst = __slicex(lst, 0, lastinx); rslt }`},
	// following translates to: lst = append([]T{ elt }, lst...); lst
	{"__popf", false, `{ val ret = lst.last; lst = __slicex(lst, 1, count(lst)); ret }`},
	{"__tail", false, `{ __slicex(lst, 1, lst.count) }`},
	{"append", false, `{ lst = __appendDDD(lst, other); lst }`},
	{"contains", false, `{var ret = true
	each(x^l2) unless(x in l1) { ret = false; break }
	ret  
}`},
	{"copy", false, `{ var rslt:list(LE) = __make(count(lst)); __copy(rslt, lst); rslt }`},
	{"copy", false, `{__copy(__slicex(lst, to, to+cnt), __slicex(lst, from, from+cnt))}`},
	{"copyFrom", false, `{__copy(__slicex(lst, ofs, ofs+count(from)), from)}`},
	{"count", false, `{ __len(lst) }`},
	{"disjoint", false, `{var ret = true
	each(x^l1, y^l2) if(x == y) { ret = false; break }
	ret
}`},
	{"filt", false, `{var rslt:list(LE) = [], inx = 0
		each(elt^lst) { if(fn(elt, inx)) rslt.pushb(elt); inx += 1 }
		rslt
}`},
	{"filt1", false, `{
		var inx = 0
		var ret:ortype(main:LE, nil:nil)
		each(elt^lst) { if(fn(elt, inx)) { ret = elt; break }; inx += 1 }
		ret
}`},
	{"filt1x", true, `{
		var inx = 0
		var ret:ortype(main:U, nil:nil)
		each(elt^lst) { if(fn(elt, inx)) { ret = xfm(elt, inx); break }; inx += 1 }
		ret
}`},
	{"filtx", true, `{var rslt:list(U) = [], inx = 0
		each(elt^lst) { if(fn(elt, inx)) rslt.pushb(xfm(elt, inx)); inx += 1 }
		rslt
}`},
	{"head", false, `{ if(lst.count == 0) nil else lst[0] }`},
	{"in", false, `{var rslt = false
  each(x^lst) if(x == elt) { rslt = true; break }
  rslt  
}`},
	{"index", false, `{ var rslt = 0; var found = false
	each(x^lst) { if(x == elt) { found = true; break }; rslt += 1 }
	found ? rslt : nil }`},
	{"insert", false, `{
		val len2 = __len(lst)+__len(other)
		if(__cap(lst) >= len2) {
			lst = __slicex(lst, 0, len2)
			__copy(__slicex(lst, inx+__len(other), len2), __slicex(lst, inx, __len(lst)))
			__copy(__slicex(lst, inx, __len(lst)), other)
		} else {
			var rslt:list(LE) = __make(len2)
			__copy(__slicex(rslt, 0, inx), __slicex(lst, 0, inx))
			__copy(__slicex(rslt, inx, inx+__len(other)), other)
			__copy(__slicex(rslt, inx+__len(other), len2), __slicex(lst, inx, __len(lst)))
			lst = rslt
		}
		lst
	}`},
	{"insert", false, `{
		if(count(lst) == 0) { if(inx == 0) { lst = __append(lst, elt); lst } else { exit("insert index") } 
		} else {
			lst = __append(lst, elt)
			if(inx != count(lst) - 1) { __copy(__slicex(lst, inx+1, count(lst)), __slicex(lst, inx, count(lst)-1)); lst[inx] = elt }
			lst
		} }`},
	{"last", false, `{ if(lst.count == 0) nil else lst[lst.count-1] }`},
	{"lvalindex", false, ""},
	{"maplst", true, `{var rslt:list(U) = [], inx = 0
		each(elt^lst) { rslt.pushb(fn(elt, inx)); inx += 1 }
		rslt
}`},
	{"popb", false, `{ if(count(lst) == 0) nil else { val lastinx = count(lst)-1; val rslt = lst[lastinx]; lst = __slicex(lst, 0, lastinx); rslt } }`},
	// following translates to: lst = append([]T{ elt }, lst...); lst
	{"popf", false, `{if(count(lst) == 0) nil else { val ret = lst.last; lst = __slicex(lst, 1, count(lst)); ret }}`},
	{"pushb", false, `{ lstpb = __append(lstpb, eltpb); lstpb }`},
	{"pushf", false, `{ lst = __appendDDD(__singletonSliceLit(elt), lst); lst }`},
	{"removeElt", false, `{
	var inx = -1
	each(e0^0...lst.count) if(lst[e0] == elt) { inx = e0; break }
	if(inx >= 0) removeIndex(lst, inx)}`},
	{"removeIndex", false, `{
		val lastinx = count(lst)-1
		if(inx < lastinx) __copy(__slicex(lst, inx, lastinx), __slicex(lst, inx+1, count(lst)))
		lst = __slicex(lst, 0, lastinx)
	}`},
	{"removeSeq", false, `{
		val delcount = hi - lo
		val newtop = count(lst) - delcount
		if(hi < count(lst)) __copy(__slicex(lst, lo, newtop), __slicex(lst, hi, count(lst)))
		lst = __slicex(lst, 0, count(lst) - delcount)
	}`},
	{"reverse", false, `{var rslt:list(LE) = __make(count(lst))
		var rinx = count(lst)
		each(elt^lst) { rinx -= 1; rslt[rinx] = elt }
		rslt
	}`},
	{"rvalindex", false, ""},
	{"select", false, `{ __slicex(lst, lo, hi) }`},
	{"stringify", false, ""},
	{"swap", false, `{var tmp:LE = lst[a]; lst[a] = lst[b]; lst[b] = tmp}`},
	{"tail", false, `{ if(lst.count == 0) nil else __slicex(lst, 1, lst.count) }`},
	{"uniq", false, `{var rslt:list(LE) = []; each(x^lst) unless(x in rslt) pushb(rslt, x); rslt}`},
}

// helper for TC'g parsed body of a ftn intended to go into an existing funinst.
// The given funi must have funargs and funtype set; other slots will be set here.
func tcFuni(funi *Funinst, body Term, tci *TCInfo) {
	tci.PushScope(funi.scope)
	tci.cxt.Push(FunLit)
	tci.cxt.SetItem("imp", true)
	tcout := body.Typecheck(nil, tci).(*TermL)
	tci.PopScope()
	tci.cxt.Pop()
	funi.body = tcout
	rcvrT := funi.funargs[0].dtype
	if rcvrT.family == TFList || rcvrT.family == TFSpace {
		funi.markEnumonly()
	}
}

// initLateBIFunctions is called at startup (from main); it defines list methods and other BI ftns whose bodies
// are defined by parse and TC. This is done from main because I don't fully understand go's initialization rules
// wrt ordering, and as a result I'm not confident it would work earlier in startup.
func initLateBIFunctions(tci *TCInfo) {
	// First, handle string ftns that get funi bodies
	smthd := TypeString.methods.Find("count")
	sfuni := smthd.binding.(*Funinst)
	strlen := makeSymbol("__len", makeType(TFFtn, []*Type{TypeString}, TypeInt, true), nil)
	bstmts := []Term{makeTermTL(Funcall, strlen, []Term{sfuni.funargs[0]}, TypeInt, Pos(-1), Pos(-1))}
	sfuni.body = makeScopedTermL(Stmts, makeScope(), bstmts, TypeInt, Pos(-1), Pos(-1))
	localscope := makeScope()
	for _, fa := range sfuni.funargs {
		localscope.Add(fa, 0)
	}
	sfuni.scope = localscope
	sfuni.body.scope.parent = localscope

	// Then, same for Gomap
	lhfuni := gomapTypegen.dtype.methods.Find("lithook").binding.(*Funinst)
	lhdefn := "{ var mp0 = self; val largs = lstargs; each(pr^largs) { mp0[pr.key] = pr.value}; mp0 }"
	nxterm := parseBody(lhdefn)
	tcFuni(lhfuni, nxterm, tci)

	// next, do the float methods to cvt to integer; these will be in alpha order in float's method list.
	var cvtfBodies = [][]string{
		{"ceiling", "{ val adjflt = flt < 0.0 ? flt : flt+1.0; __cvtf(adjflt) }"},
		{"floor", "{ val adjflt = flt < 0.0 ? flt-1.0 : flt; __cvtf(adjflt) }"},
		{"round", "{ val adjflt = flt < 0.0 ? flt-0.5 : flt+0.5; __cvtf(adjflt) }"},
	}
	i := 0
	bd := cvtfBodies[i]
	for _, mthd := range TypeFloat.methods {
		if bd[0] == mthd.sym.ident {
			nxterm = parseBody(bd[1])
			funi := mthd.sym.binding.(*Funinst)
			funi.scope = makeScope()
			funi.scope.Add(funi.funargs[0], 0)
			tcFuni(funi, nxterm, tci)
			i++
			if i >= 3 {
				break
			}
			bd = cvtfBodies[i]
		}
	}

	// Now set up for lists
	// Set up a scope that imitates the header of a "given(LE, U::entity)..."
	tci.PushScope(gvnscope)
	// also set up a scope with just U, since LE is really a typegen parameter.
	gvnscope2 := makeScope()
	gvnscope2.Add(gvnscope.lookupType("U"), 0)
	tci.cxt.Push(FunLit)
	// since the funargs are all listed explicitly in listMthdBodies, we don't want to set MethodTag in tci.cxt
	for mthdinx, mb := range listMthdBodies {
		mthdsym := listTG.dtype.methods[mthdinx].sym
		if mb.defn == "" {
			continue
		}
		nxterm := parseBody(mb.defn)
		// Set up the parsed stmtlist for TC: create a local scope with the funargs from the existing Funinst, and set the
		// tci cxt as an imp ftnlit.
		//fmt.Println("starting body initzn of ", mthdsym.ident)
		mthdsym.plist.Add("givenscope", gvnscope)
		mthdfuni := mthdsym.binding.(*Funinst)
		tcFuni(mthdfuni, nxterm, tci)
		mthdfuni.isLocalfn = true
		mthdsym.plist.Delete("givenscope")
		if mb.hasGiven {
			mthdsym.plist.Add("givenscope", gvnscope2)
		}
		if mthdsym.ident == "count" || mthdsym.ident == "select" {
			mthdsym.plist.Add("simpleInline", true) // hack to improve how anyOOL works
		}
	}
	tci.cxt.Pop()
	tci.PopScope()
	// give the list typepred its affordances
	listMethods0 := listTG.insts[0][1].(*Type).methods
	ltpaffs := make([]*Symbol, len(listMethods0))
	for i, ms := range listMethods0 {
		ltpaffs[i] = ms.sym
	}
	TPList.v.(*Typred).affs = ltpaffs

	// earlier setup has created two cached list types that are incomplete as to methods:
	// for list(byte) no method bodies and for list(U) some missing ones. (List(U) is for filtx.)
	fixupCachedListTypes(gvnscope2)
	initPkgImports()
}
