// This file declares Term, the main internal form. It defines most of the concrete types and methods used to
// represent Terms. And it contains some utility methods and ftns for manipulating them.
package main

import (
	"fmt"
	"strings"
)

type Err string

func (e Err) Error() string { return string(e) }

type Termtag int

const (
	NoTerm Termtag = iota
	ErrorTag
	TypeTag
	TypegenTag
	FuninstTag
	MethodTag
	// "normal" tags below
	Intlit        // TermB
	Floatlit      // TermB
	Stringlit     // TermB
	Bytelit       // TermB
	Labellit      // TermB
	Boollit       // TermB
	Ident         // Token
	Symdecl       // TermL for unprocessed bdgpt+bdg decls
	SymdeclAttrib // for typepred affordances
	SymbolTag     // Bdg
	Valdecl
	Vardecl
	Fwddecl
	Attribdecl // note: parser code (attribOrMethod) depends on order of next 3
	AttribdeclPvt
	Methoddecl
	MethoddeclPvt
	FunTypeLit
	FunLit
	TupleTypeLit // these are tuple and ortype in type decls; "lit" isn't exactly the right term
	OrtypeLit
	AssertStmt
	Stmts
	AsgnStmt
	IfStmt
	LoopStmt
	WhileStmt
	EachStmt
	BreakStmt
	ContinueStmt
	ReturnStmt
	OncondStmt
	Deferstmt // derived from oncond in TC
	OntimeStmt
	ImportStmt
	ImportPkgStmt
	ExportStmt
	GivenStmt
	StmapStmt
	DirectStmt
	RwsDeclStmt
	GomapAllowed // not really a termtag, but used for TC
	// expression tags start here
	Gdref
	Typecond
	TypeAssert
	TypeAssertDecl
	ExtendExpr
	TypepredExpr
	MapExpr
	Ifcase
	Typecase
	Symchain
	Dot
	Dotbrace
	Dotbracket
	TildeExpr
	IndexExpr
	Litform
	Litform0 // post-TC, uses TermL instead of TermTL
	Range    // for iterees of each
	Funcall  // TermTL
	EqExpr
	EqeqExpr
	NeqExpr
	NeqeqExpr
	Arrow
	AtExpr
	Negate
	Lognot
	PluseqExpr
	MinuseqExpr
	MuleqExpr
	DiveqExpr
	AndandExpr
	OrorExpr
	HatExpr
	DotdotExpr
	Dot3Expr
	// used only in backend, restricted circumstances; also hijacked for TC of parsed pattern strings
	SymbolPattern
	Typeterm  // used for rewriting
	ZeroValue // appears in TermT
	Multiret  // tag to generate a decl to receive 2 values
)

var termtag2String = map[Termtag]string{
	NoTerm:         "NoTerm",
	ErrorTag:       "ErrorTag",
	TypeTag:        "TypeTag",
	TypegenTag:     "TypegenTag",
	FuninstTag:     "FuninstTag",
	MethodTag:      "MethodTag",
	Intlit:         "Intlit",
	Floatlit:       "Floatlit",
	Stringlit:      "Stringlit",
	Bytelit:        "Bytelit",
	Labellit:       "Labellit",
	Boollit:        "Boollit",
	Ident:          "Ident",
	Symdecl:        "Symdecl",
	SymdeclAttrib:  "SymdeclAttrib",
	SymbolTag:      "SymbolTag",
	Valdecl:        "Valdecl",
	Vardecl:        "Vardecl",
	Fwddecl:        "Fwddecl",
	Attribdecl:     "Attribdecl",
	AttribdeclPvt:  "AttribdeclPvt",
	Methoddecl:     "Methoddecl",
	MethoddeclPvt:  "MethoddeclPvt",
	FunTypeLit:     "FunTypeLit",
	FunLit:         "FunLit",
	TupleTypeLit:   "TupleTypeLit",
	OrtypeLit:      "OrtypeLit",
	AssertStmt:     "AssertStmt",
	Stmts:          "Stmts",
	AsgnStmt:       "AsgnStmt",
	IfStmt:         "IfStmt",
	LoopStmt:       "LoopStmt",
	WhileStmt:      "WhileStmt",
	EachStmt:       "EachStmt",
	BreakStmt:      "BreakStmt",
	ContinueStmt:   "ContinueStmt",
	ReturnStmt:     "ReturnStmt",
	OncondStmt:     "OncondStmt",
	OntimeStmt:     "OntimeStmt",
	ImportStmt:     "ImportStmt",
	ImportPkgStmt:  "ImportPkgStmt",
	ExportStmt:     "ExportStmt",
	GivenStmt:      "GivenStmt",
	StmapStmt:      "StmapStmt",
	DirectStmt:     "DirectStmt",
	RwsDeclStmt:    "RwsDeclStmt",
	GomapAllowed:   "GomapAllowed",
	Gdref:          "Gdref",
	Typecond:       "Typecond",
	TypeAssert:     "TypeAssert",
	TypeAssertDecl: "TypeAssertDecl",
	ExtendExpr:     "ExtendExpr",
	TypepredExpr:   "TypepredExpr",
	MapExpr:        "MapExpr",
	Ifcase:         "Ifcase",
	Typecase:       "Typecase",
	Symchain:       "Symchain",
	Dot:            "Dot",
	Dotbrace:       "Dotbrace",
	Dotbracket:     "Dotbracket",
	TildeExpr:      "TildeExpr",
	IndexExpr:      "IndexExpr",
	Litform:        "Litform",
	Litform0:       "Litform0",
	Range:          "Range",
	Funcall:        "Funcall",
	EqExpr:         "EqExpr",
	EqeqExpr:       "EqeqExpr",
	NeqExpr:        "NeqExpr",
	NeqeqExpr:      "NeqeqExpr",
	Arrow:          "Arrow",
	AtExpr:         "AtExpr",
	Negate:         "Negate",
	Lognot:         "Lognot",
	PluseqExpr:     "PluseqExpr",
	MinuseqExpr:    "MinuseqExpr",
	MuleqExpr:      "MuleqExpr",
	DiveqExpr:      "DiveqExpr",
	AndandExpr:     "AndandExpr",
	OrorExpr:       "OrorExpr",
	HatExpr:        "HatExpr",
	DotdotExpr:     "DotdotExpr",
	Dot3Expr:       "Dot3Expr",
	SymbolPattern:  "SymbolPattern",
	Typeterm:       "Typeterm",
	ZeroValue:      "ZeroValue",
	Multiret:       "Multiret",
}

func (tt Termtag) baselit() bool {
	return tt == Intlit || tt == Floatlit || tt == Stringlit || tt == Bytelit || tt == Labellit || tt == Boollit
}

// a subset of the Termtags map back to tokens for printing purposes.
// do this in 2 steps so we can get precedence too, for parenthesizing.
var ttag2tcode = map[Termtag]TokenCode{
	EqExpr:      EQ,
	EqeqExpr:    EQEQ,
	NeqExpr:     NEQ,
	NeqeqExpr:   NEQEQ,
	Arrow:       ARROW,
	AtExpr:      TOKAT,
	Lognot:      TOKNOT,
	PluseqExpr:  PLUSEQ,
	MinuseqExpr: MINUSEQ,
	MuleqExpr:   MULEQ,
	DiveqExpr:   DIVEQ,
	HatExpr:     TOKHAT,
	DotdotExpr:  DOTDOT,
	Dot3Expr:    DOT3,
}

// Term is the key type around which the whole compiler is built.
type Term interface {
	First() Pos
	Final() Pos
	Tag() Termtag
	Copy() Term
	Equal(Term) bool
	Dtype() *Type
	Plist() *Plist
	String() string
	Typecheck(*Type, *TCInfo) Term
	Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool
}

// utility ftn for pattern of token with given val.
func tokenNamed(trm Term, nm string) bool {
	return trm != nil && trm.Tag() == Ident && trm.(*Token).val == nm
}

// utility ftn for pattern of funcall to a named ftn. Works pre- or post- TC. IOW with Tokens and Symbols.
func callingNamed(trm Term, nm string) bool {
	if trm != nil && trm.Tag() == Funcall {
		trm0 := trm.(*TermTL)
		if symbolNamed(trm0.trm, nm) {
			return true
		}
		return tokenNamed(trm0.trm, nm)
	}
	return false
}

func symbolNamed(trm Term, nm string) bool {
	return trm != nil && trm.Tag() == SymbolTag && trm.(*Symbol).ident == nm
}

func itemExists(trm Term, nm string) bool {
	return trm.Plist().Find(nm) != nil
}

func itemExistsNot(trm Term, nm string) bool {
	return trm.Plist().Find(nm) == nil
}

// Non-tcinfo error
func genError(msg string, trm Term) Term {
	return makeTermB(ErrorTag, msg, trm.First())
}

// The next few definitions concern a facility to "concretize" abstract terms, both generics (via
// given) and instantiatable (via typegen). The (post-order) walk is done with Termfind.
// We use Termfind to traverse through all the subterms of a given Term and mod as
// needed to create a Term in which the generic types are replaced by corresp concrete ones.
// Note that these can be generic as well, though they usually aren't. At bottom, GTS in the
// generic slice change Types and GES change non-Type Terms. But this is more complicated than
// than just copying everything that changes because we need to maintain the invariant that every
// use of a Symbol declared anywhere (including outside the scope of the original term) is represented
// by the same Symbol, not a copy. That's the case when TC creates such terms and we want it to be
// true when generics are concretized.
type ConczInfo struct {
	generics   []*Symbol // save original constructor args
	concretes  []Term    // retained as is for typegen cache management
	gnctypes   []*Type   // extract type-valued args and corresponding actuals
	conctypes  []*Type
	gncthings  []*Symbol // ditto for entity-valued args
	concthings []Term
	// next slots hold state while doing an instantiation
	seen      []*Type     // infloop prevention
	origsyms  symbolSlice // Symbols seen in origterm ordered on idents
	postsyms  symbolSlice // replacements for origsyms or nil if not changed
	termstack []Term      // this has subterm copies while traversing, finished copy at end.
	tgen      *Typegen    // set if called to instantiate, to add cache elt before concretizing methods
	auxtg     []*Typegen  // record other typegens encountered, for clearing purposes
}

func makeConczInfo(generics []*Symbol, concretes []Term) *ConczInfo {
	var gtyp, ctyp []*Type
	var gent []*Symbol
	var cent []Term
	// easier if we pre-calc which args are GTS vs GES.
	// assume len(generics) == len(concretes)
	for i, g := range generics {
		if g.dtype == nil {
			gtyp = append(gtyp, g.binding.(*Type))
			ctyp = append(ctyp, concretes[i].(*Type))
		} else {
			gent = append(gent, g)
			cent = append(cent, concretes[i])
		}
	}
	czi := ConczInfo{generics, concretes, gtyp, ctyp, gent, cent,
		[]*Type{}, make([]*Symbol, 0, 10), make([]*Symbol, 0, 10),
		make([]Term, 0, 10), nil, make([]*Typegen, 0, 4)}
	return &czi
}

func (czi *ConczInfo) termPop() (ret Term) {
	m1 := len(czi.termstack) - 1
	ret = czi.termstack[m1]
	czi.termstack = czi.termstack[:m1]
	return
}

func (czi *ConczInfo) termPush(trm Term) {
	czi.termstack = append(czi.termstack, trm)
}

func (czi *ConczInfo) addAux(tg *Typegen) {
	for _, tg0 := range czi.auxtg {
		if tg == tg0 {
			return
		}
	}
	czi.auxtg = append(czi.auxtg, tg)
}

// Given a generated type with a generator different from czi.tgen, extract concrete args for
// instantiating it wrt czi. This is done by extracting the actuals used to generate typ (from
// typ.instinx and typ.generator), comparing those to the abstract args to czi, and when they match,
// setting the corresponding concrete arg to the concrete arg in czi. If no czi.abstract arg matches,
// then czi doesn't ground that particular argument of this typegen, so set concrete same as abstract.
// If no actuals of typ match abstracts of czi, then czi doesn't ground typ in any way, possibly because
// it's already completely grounded. In that case, return nil.
func (czi *ConczInfo) extractArgs(typ *Type) []Term {
	tgen := typ.generator
	if tgen == nil {
		return nil
	}
	actuals := tgen.insts[typ.instInx][:len(tgen.funargs)]
	ret := make([]Term, len(tgen.funargs))
	mapped := 0
	for i, tga := range tgen.funargs {
		prm := actuals[i]
		found := false
		for j, gnc := range czi.gnctypes {
			if gnc == prm {
				ret[i] = czi.conctypes[j]
				mapped++
				found = true
			}
		}
		if !found {
			if prm.Tag() == SymbolTag {
				// GES
				ret[i] = czi.concretes[i]
			} else {
				ret[i] = tga.binding
			}
		}
	}
	if mapped == 0 {
		return nil
	}
	return ret
}

// Handle types completely for the concretize method, ie the conczWF workfn.
// Czi.seen is the recursion handler; each entry is a pair of Types, old then new.
// If the type results from a typegen and its cache has an entry with the same args, use that.
// Otherwise, work through structural part, recursing as needed, and then concretize methods,
// asserts, and handlers.
// We make types in 2 steps for recursion handling purposes. The initial creation has empty
// slots in attributes and similar structural elts; these are filled in and the type "finished" later.
func (czi *ConczInfo) handleType(typ *Type) *Type {
	// First, check for hitting a recursion point
	for i := 0; i < len(czi.seen); i += 2 {
		if (czi.seen)[i] == typ {
			return (czi.seen)[i+1]
		}
	}
	// Second, make the substns happen
	for i, gt := range czi.gnctypes {
		if typ == gt {
			return czi.conctypes[i]
		}
	}
	typ0 := typ // typ is the default retval but may be modded in the following switch stmt
	// Next, handle the structural part
	switch typ.family {
	case TFList:
		if typ.generator == czi.tgen && czi.gnctypes[0] == typ.v.(*Type) { // bootstrap case here
			typ0 = typ.generator.checkCache(czi.concretes)
			if typ0 != nil {
				return typ0
			}
			typ0 = makeType(TFList, czi.conctypes[0]) // bootstrap by falling through
			czi.seen = append(czi.seen, typ, typ0)
			czi.tgen.add2Cache(czi.concretes, typ0)
		} else {
			reconc := czi.extractArgs(typ) // if typ is instantiable in czi, set up arg to do so
			if reconc != nil {
				typ0 = typ.generator.Instantiate(reconc)
				czi.seen = append(czi.seen, typ, typ0)
				return typ0
			} else if typ.generator == nil && czi.tgen == nil {
				// here, doing gnc instantiation so could have gnc inside, eg list(space(float, GES))
				panic("unwritten")
			} else {
				// If there's nothing in typ to instantiate wrt czi, return it unchanged, else instantiate manually.
				gncs := typ.generics() // find all the gncs anywhere in typ
				present := false
				for _, g := range gncs {
					if g == czi.gnctypes[0] {
						present = true
						break
					}
				}
				if present {
					// here, do the elttype manually and assemble.
					eltyp := typ.v.(*Type)
					eltyp0 := czi.handleType(eltyp)
					if eltyp != eltyp0 {
						return makeListType(eltyp0)
					}
				}
			}
		}
	case TFSpace:
		// copy and paste from TFList case but note that spaceTG has 2 params
		spctyp := typ.v.(*Spacetype)
		if typ.generator == czi.tgen && czi.gnctypes[0] == spctyp.elttype {
			typ0 = typ.generator.checkCache(czi.concretes)
			if typ0 != nil {
				return typ0
			}
			conczWF(spctyp.dimensions, czi) // this may use the other param
			dim0 := czi.termPop()
			typ0 = makeType(TFSpace, czi.conctypes[0], dim0)
			czi.seen = append(czi.seen, typ, typ0)
			czi.tgen.add2Cache(czi.concretes, typ0)
		} else {
			reconc := czi.extractArgs(typ) // if typ is instantiable in czi, set up arg to do so
			if reconc != nil {
				typ0 = typ.generator.Instantiate(reconc)
				czi.seen = append(czi.seen, typ, typ0)
				return typ0
			} else if typ.generator == nil && czi.tgen == nil {
				// here, doing gnc instantiation so could have gnc inside, eg space(space(float, GES), n)
				panic("unwritten")
			} else {
				// Need to pattern match on the list case above
				panic("unwritten")
			}
		}
	case TFTuple:
		// First, check the cases that shortcut the fully manual synthesis.
		if czi.tgen != nil && typ.generator == czi.tgen {
			cached := czi.tgen.checkCache(czi.concretes)
			if cached != nil {
				return cached
			}
		}
		gncs := typ.generics() // find all the gncs anywhere in typ
		commons := 0           // commons > 0 means not disjoint with czi gncs
		for _, g := range gncs {
			for _, cg := range czi.gnctypes {
				if g == cg {
					commons += 1
					break
				}
			}
		}
		if commons > 0 {
			// I'm not entirely sure whether there are more things to do along the lines of czi.extractArgs for
			// the tuple case, but I am pretty sure they would only be optimizations.
			tupt := typ.v.(*Tupletype)
			natrbs := make([]*Symbol, len(tupt.attribs))
			newtt := &Tupletype{natrbs, tupt.situated}
			typ0 = makeType0(TFTuple, newtt, "")
			czi.seen = append(czi.seen, typ, typ0)
			changed := false
			for i, a := range tupt.attribs {
				typ1 := czi.handleType(a.dtype)
				if typ1 != a.dtype {
					changed = true
				}
				natrbs[i] = makeSymbol(a.ident, typ1, nil)
			}
			if changed {
				itemTN := typ.plist.Find("typename")
				if itemTN != nil {
					typ0.plist.Add("typename", itemTN)
				}
				itemEB := typ.plist.Find("enumerable")
				if itemEB != nil {
					inx := newtt.attribIndex(itemEB.(*Symbol).ident)
					typ0.plist.Add("enumerable", newtt.attribs[inx])
				}
				typ0.SetString()
				if typ.generator != nil {
					var actuals []Term
					if typ.generator == czi.tgen {
						actuals = czi.concretes
					} else if commons == len(gncs) {
						myconcs := make([]Term, len(gncs))
						mycinx := 0
						for i, g := range czi.gnctypes {
							if g == gncs[mycinx] {
								myconcs[mycinx] = czi.concretes[i]
								mycinx++
								if mycinx == len(gncs) {
									break
								}
							}
						}
						cached := typ.generator.checkCache(myconcs)
						if cached != nil {
							return cached
						} else {
							return typ.generator.Instantiate(myconcs)
						}
					} else {
						panic("what to do")
					}
					typ.generator.add2Cache(actuals, typ0)
				}
			} else {
				typ0 = typ
				// Note: next line is wrong
				czi.seen = czi.seen[0 : len(czi.seen)-2]
			}
		} else if typ.plist.Find("nestedGnc") != nil {
			// Normally, if nothing structural has changed, nothing can change in these aspects. However,
			// a type defined inside a type inside a typegen can use a generic symbol from the outer type.
			// The "nestedGnc" plist item records this.
			// Possibly, we need to check for this in cases other than tuple.
			tupt := typ.v.(*Tupletype)
			typ0 = makeType(TFTuple, tupt.attribs, tupt.situated)
			czi.seen = append(czi.seen, typ, typ0)
		}
	case TFFtn:
		funt := typ.v.(*Ftntype)
		nargs := make([]*Type, len(funt.fmlargs))
		typ0 = makeType0(TFFtn, &Ftntype{nargs, funt.rettype, funt.pure}, "")
		czi.seen = append(czi.seen, typ, typ0)
		changed := false
		for i, t := range funt.fmlargs {
			typ1 := czi.handleType(t)
			if typ1 != t {
				changed = true
			}
			nargs[i] = typ1
		}
		nret := czi.handleType(funt.rettype)
		typ0.v.(*Ftntype).rettype = nret
		if changed || nret != funt.rettype {
			typ0.plist = typ.plist // should copy??
			//typ0.SetString()
		} else {
			typ0 = typ
			czi.seen = czi.seen[0 : len(czi.seen)-2]
		}
	case TFOrtype:
		ort := typ.v.(*Ortype)
		nvars := make([]*Symbol, len(ort.variants))
		newot := &Ortype{nvars, ort.isNilposs}
		typ0 = makeType0(TFOrtype, newot, "")
		czi.seen = append(czi.seen, typ, typ0)
		changed := false
		for i, a := range ort.variants {
			typ1 := czi.handleType(a.dtype)
			if typ1 != a.dtype {
				changed = true
			}
			nvars[i] = makeSymbol(a.ident, typ1, nil)
		}
		if changed {
			typ0.plist = typ.plist // should copy??
			typ0.SetString()
		} else {
			typ0 = typ
			czi.seen = czi.seen[0 : len(czi.seen)-2]
		}
	case TFGomap:
		// I'm not sure how to work the logic here compared with list, as there are 2 params.
		// The confusing part is handling a Gomap that's unrelated to the one we're instantiating.
		// This is extremely obscure and I'm going to ignore it for now.
		ret0 := gomapTypegen.checkCache(czi.concretes)
		if ret0 != nil {
			return ret0
		}
		kvpair := typ.v.([]*Type)
		ktype := kvpair[0]
		vtype := kvpair[1]
		ktype0 := czi.handleType(ktype)
		vtype0 := czi.handleType(vtype)
		if ktype != ktype0 || vtype != vtype0 {
			typ0 = makeType(TFGomap, ktype0, vtype0)
			czi.seen = append(czi.seen, typ, typ0)
			gomapTypegen.add2Cache([]Term{ktype0, vtype0}, typ0)
			czi.addAux(gomapTypegen)
		}
	}
	// Here, we've got the last part left, assuming any of methods, handlers, asserts are non-nil.
	if typ == typ0 {
		return typ
	}

	if len(typ.methods) > 0 {
		// concretizing methods is tricky. Make all the new symbols first, without bindings, then fill those in
		for _, m := range typ.methods {
			t0 := czi.handleType(m.sym.dtype)
			sym0 := makeSymbol(m.sym.ident, t0, nil)
			sym0.plist = *m.sym.plist.Copy()
			typ0.methods.Add(sym0, 0)
			// Add these now else recursive ftns won't work
			czi.addSymbol(m.sym, sym0)
		}
		for i, m := range typ.methods {
			if m.sym.binding == nil {
				continue
			}
			m.sym.binding.Termfind(conczWF, czi)
			nbdg := czi.termPop()
			typ0.methods[i].sym.binding = nbdg
		}
	}
	typ.asserts = typ.asserts.Concretize(czi)
	if len(typ.handlers) > 0 {
		for _, h := range typ.handlers {
			conczWF(h, czi)
			h0 := czi.termPop()
			typ0.handlers = append(typ0.handlers, h0)
		}
	}
	return typ0
}

// The following is added late in the game when it turns out that the backend needs correct scopetrees within Funinsts
// in order to rename conflicting symbols. Concretization was designed on the assumption that scopetrees wouldn't matter
// after TC. So conczWF chops up stmtlists and reassembles them in a way that makes rebuilding scopetrees hard because
// the concretized terms are constructed in the wrong order. Quite possibly the most elegant way to fix this would be to
// rewrite concretize to use a topdown recursive scheme that would be more compatible with scopetree construction. And
// given the length of the conczWF code, this might be a good idea on general grouands. But meanwhile, I'm going to put
// in the following, which does a separate fixup and is designed to be called at Funinst level.
//
// In sum then, this is designed to be called on a term that's the body of a Funinst with the Scope of the funinst.
// But it works when called on any TermL. It ensures that every scope slot in a TermL nested in trm has the correct parent.
func fixupScopetree(trm *TermL, parent *Scope) {
	if trm == nil {
		return
	}
	nparent := parent
	if trm.scope != nil {
		trm.scope.parent = parent
		nparent = trm.scope
	}
	for _, s := range trm.args {
		tg := s.Tag()
		if tg == IfStmt {
			s0 := s.(*TermL)
			// ifstmt branches aren't always stmtlists so check that
			if s0.args[1].Tag() == Stmts {
				fixupScopetree(s0.args[1].(*TermL), nparent)
			}
			if len(s0.args) == 3 && s0.args[2].Tag() == Stmts {
				fixupScopetree(s0.args[2].(*TermL), nparent)
			}
		} else if tg == EachStmt {
			s0 := s.(*TermL)
			if s0.scope != nil {
				s0.scope.parent = nparent
				nparent = s0.scope
			}
			fixupScopetree(s0.args[2].(*TermL), nparent)
		} else if tg == LoopStmt {
			s0 := s.(*TermT)
			fixupScopetree(s0.arg0.(*TermL), nparent)
		} else if tg == Ifcase {
			panic("unwritten")
		}
	}
}

// Search for sym in czi.origsyms, -1 if not found else index
func (czi *ConczInfo) findSymbol(sym *Symbol) int {
	originx := czi.origsyms.binsearch(sym.ident)
	if originx >= 0 {
		for ; ; originx++ {
			if originx == len(czi.origsyms) || czi.origsyms[originx].ident != sym.ident {
				return -1
			}
			if czi.origsyms[originx] == sym {
				return originx
			}
		}
	}
	return -1 // notreached in fact
}

// Assume the Symbol b4 is not in origsyms
func (czi *ConczInfo) addSymbol(b4, aftr *Symbol) int {
	originx := czi.origsyms.binsearch(b4.ident)
	czi.origsyms.insert(originx, b4)
	czi.postsyms.insert(originx, aftr)
	return originx
}

// True if czi is instantiating typ.
func (czi *ConczInfo) abstMatch(typ *Type) bool {
	if typ.generator != nil {
		args := typ.generator.funargs
		if len(args) == len(czi.generics) {
			for i, a := range args {
				if a != czi.generics[i] {
					return false
				}
			}
			return true
		}
	}
	return false
}

// The workfn for concretizing, designed for use with Termfind.
func conczWF(trm Term, info interface{}) bool {
	czi := info.(*ConczInfo)
	rslt := trm
	switch trm0 := trm.(type) {
	case *Type:
		ntyp := czi.handleType(trm0)
		czi.termPush(ntyp)
	case *Symbol:
		nsym := trm0
		// Options: (1) if trm0 is a GES, subst its binding; (2) if a method, find & use the inst;
		// (3) if in GTS attempt to bind or use binding; (4) otherwise check if seen before and
		// if not resynth and cache in postsyms.
		for i, gt := range czi.gncthings {
			if gt.Equal(trm0) {
				if gt != trm0 {
					fmt.Println("got symbols equal but not == in conczWF", gt)
				}
				czi.termPush(czi.concthings[i])
				return false
			}
		}
		// also, if a gnctype is Typred and nsym is a method of it, return the corresp method of the conctype
		if nsym.dtype != nil && nsym.dtype.family == TFFtn && len(nsym.dtype.v.(*Ftntype).fmlargs) > 0 {
			rcvrtyp := nsym.dtype.v.(*Ftntype).fmlargs[0]
			checkTypredSpecific := func(typ *Type) bool {
				if typ.family != TFGTS {
					return false
				}
				subtyp := typ.v.(*GTS).dtype
				return subtyp.family == TFTypred && len(subtyp.v.(*Typred).affs) > 0
			}
			if checkTypredSpecific(rcvrtyp) {
				for i, gt := range czi.gnctypes {
					if gt != rcvrtyp.v.(*GTS).dtype {
						continue
					}
					for _, gncmthd := range gt.v.(*Typred).affs {
						if gncmthd == nsym {
							concmthd := czi.conctypes[i].methodLookup(gncmthd.ident)
							czi.termPush(concmthd)
							return false
						}
					}
				}
			}
		}
		originx := czi.findSymbol(trm0)
		if originx >= 0 {
			czi.termPush(czi.postsyms[originx])
			return false
		}
		if trm0.dtype == nil {
			// must be typish
			if trm0.binding != nil && trm0.binding.Tag() == TypeTag {
				typ := trm0.binding.(*Type)
				typ0 := czi.handleType(typ)
				if typ != typ0 {
					nsym = makeSymbol(trm0.ident, nil, typ0)
				}
				czi.addSymbol(trm0, nsym)
				czi.termPush(nsym)
				return false
			}
		}
		// check for substg a concrete method for an abstract one
		if trm0.plist.Find("tpaffdnc") != nil {
			fntyp := trm0.dtype.v.(*Ftntype)
			// the following is not general enough
			if len(fntyp.fmlargs) > 0 && fntyp.fmlargs[0] == czi.gnctypes[0] && len(czi.conctypes[0].methods) > 0 {
				cmthd := czi.conctypes[0].methods.Find(trm0.ident)
				if cmthd != nil {
					czi.termPush(cmthd)
					return false
				}
			}
		}
		typ0 := czi.handleType(trm0.dtype)
		// If trm0 is a method of some generated type and we are instantiating same, return the already created method
		if typ0.family == TFFtn {
			ftntyp := typ0.v.(*Ftntype)
			if len(ftntyp.fmlargs) > 0 {
				rcvr := ftntyp.fmlargs[0]
				mthdinx := rcvr.methods.IndexExact(trm0)
				if czi.abstMatch(rcvr) && mthdinx >= 0 {
					czi.termPush(rcvr.methods[mthdinx].sym)
					return false
				}
			}
		}
		// Not a method, so use and update the czi.origsyms and czi.postsyms slots
		var ntyp *Type
		var nbdg Term
		nsym = makeSymbol(trm0.ident, typ0, nil)
		nsym.plist = *trm0.plist.Copy()
		originx = czi.addSymbol(trm0, nsym)
		if trm0.binding != nil {
			// this hasn't been visited -- Termfind treats Symbol as a leaf.
			// but we can borrow czi
			trm0.binding.Termfind(conczWF, czi)
			nbdg = czi.termPop()
			nsym.binding = nbdg
		}
		// undo new symbol identity if nothing changed
		if originx >= 0 && ntyp == trm0.dtype && nbdg == trm0.binding {
			inx := czi.origsyms.findsym(trm0)
			czi.postsyms[inx] = trm0
			nsym = trm0
		}
		czi.termPush(nsym)
	case *Funinst:
		cpyscope := makeScope()
		scopechanged := false
		// check the funarg scope
		if trm0.scope != nil {
			cpyscope.parent = trm0.scope.parent
			for _, s := range trm0.scope.entries {
				conczWF(s.sym, czi)
				s0 := czi.termPop().(*Symbol)
				cpyscope.Add(s0, s.xcount)
				if s.sym != s0 {
					scopechanged = true
				}
			}
		}
		// now the local symbols are accumd into czi and a new scope is ready if needed. Handle type, funargs, body.
		argschanged := false
		nargs := make([]*Symbol, len(trm0.funargs))
		for i, a := range trm0.funargs {
			conczWF(a, czi)
			a0 := czi.termPop().(*Symbol)
			if a0 != a {
				argschanged = true
				if a.plist.Find("enumonly") != nil {
					a0.plist.Add("enumonly", true)
				}
			}
			nargs[i] = a0
		}
		ntyp := czi.handleType(trm0.dtype)
		var nbody *TermL
		if trm0.body != nil {
			nbody = czi.termPop().(*TermL)
			if nbody.scope != nil {
				nbody.scope.parent = cpyscope
			}
		}
		// at this point, we've done everything. Check for changes and create a new Funinst if needed
		nfuni := trm0
		if scopechanged || argschanged || ntyp != trm0.dtype || nbody != trm0.body {
			nfuni = makeFuninst(cpyscope, nargs, ntyp, nbody)
			nfuni.plist = *trm0.plist.Copy()
			fixupScopetree(nbody, cpyscope)
		}
		czi.termPush(nfuni)
	case *TermB:
		czi.termPush(trm) // cannot change
	case *TermT:
		var a0 Term
		if trm0.arg0 != nil {
			a0 = czi.termPop() // get processed arg0
		}
		ntyp := czi.handleType(trm0.dtype)
		if ntyp != trm0.dtype || a0 != trm0.arg0 {
			rslt = makeTermT(trm0.kind, a0, ntyp, trm0.first, trm0.final)
		}
		czi.termPush(rslt)
	case *TermTT:
		a1 := czi.termPop() // get processed arg1
		a0 := czi.termPop() // get processed arg0
		ntyp := czi.handleType(trm0.dtype)
		if ntyp != trm0.dtype || a0 != trm0.arg0 || a1 != trm0.arg1 {
			rslt = makeTermTT(trm0.kind, a0, a1, ntyp, trm0.first, trm0.final)
		}
		czi.termPush(rslt)
	case *TermL: // all the args are pushed in reverse order so pop, checking for changes
		nargs := len(trm0.args)
		args0 := make([]Term, nargs)
		changed := false
		for i := 1; i <= nargs; i++ {
			tmp := czi.termPop()
			if tmp != trm0.args[nargs-i] {
				changed = true
			}
			args0[nargs-i] = tmp
		}
		ntyp := czi.handleType(trm0.dtype)
		if ntyp != trm0.dtype || changed {
			rslt0 := makeScopedTermL(trm0.kind, trm0.scope, args0, ntyp, trm0.first, trm0.final)
			if trm0.scope != nil {
				cpscope := makeScope()
				cpscope.parent = trm0.scope.parent
				for _, sx := range trm0.scope.entries {
					inx := czi.origsyms.binsearch(sx.sym.ident)
					if inx < 0 {
						panic("impossible")
					}
					for ; ; inx++ {
						if inx == len(czi.origsyms) || czi.origsyms[inx].ident != sx.sym.ident {
							panic("impossible")
						}
						if czi.origsyms[inx] == sx.sym {
							cpscope.Add(czi.postsyms[inx], sx.xcount)
							break
						}
					}
				}
				rslt0.scope = cpscope
			}
			rslt = rslt0
		}
		czi.termPush(rslt)
	case *TermTL: // trm then args pushed, pop in reverse
		nargs := len(trm0.args)
		args0 := make([]Term, nargs)
		changed := false
		for i := 1; i <= nargs; i++ {
			tmp := czi.termPop()
			if tmp != trm0.args[nargs-i] {
				changed = true
			}
			args0[nargs-i] = tmp
		}
		var t0 Term
		if trm0.trm != nil {
			t0 = czi.termPop() // get processed arg0
		}
		ntyp := czi.handleType(trm0.dtype)
		if ntyp != trm0.dtype || t0 != trm0.trm || changed {
			rslt = makeTermTL(trm0.kind, t0, args0, ntyp, trm0.first, trm0.final)
		}
		czi.termPush(rslt)

	}
	return false
}

// special purpose ftn to fix issues arising from initzn. It's here because
// the code is similar to method concretization code just above here.
// Specifically, list(byte) has no method bodies and list(U) has missing ones
// Also list(xprterm) and list(xprsym) need methods (not just bodies).
func fixupCachedListTypes(gvnscope *Scope) {
	// add body to Funinsts of existing methods of rslt by concretizing methods of proto
	var addmethods = func(proto, rslt *Type, gnc []*Symbol, inst Term) {
		czi := makeConczInfo(gnc, []Term{inst})
		czi.tgen = listTG
		for i, m := range proto.methods {
			gncbody := m.sym.binding.(*Funinst).body
			if gncbody != nil {
				rsltsym := rslt.methods[i].sym
				if rsltsym.ident == "maplst" || rsltsym.ident == "filtx" || rsltsym.ident == "filt1x" {
					rsltsym.plist.Add("givenscope", gvnscope)
				}
				rsltfuni := rsltsym.binding.(*Funinst)
				// make sure the funargs symbols are available
				protoargs := m.sym.binding.(*Funinst).funargs
				for i, s := range rsltfuni.funargs {
					protarg := protoargs[i]
					inx := czi.origsyms.binsearch(s.ident)
					czi.origsyms.insert(inx, protarg)
					czi.postsyms.insert(inx, s)
					if protarg.plist.Find("enumonly") != nil {
						s.plist.Add("enumonly", true)
					}
				}
				gncbody.Termfind(conczWF, czi)
				nbody := czi.termPop()
				rsltfuni.body = nbody.(*TermL)
				//rsltfuni.body.scope.parent = rsltfuni.scope
				fixupScopetree(rsltfuni.body, rsltfuni.scope)
			}
		}
	}
	typ0 := listTG.insts[0][1].(*Type)
	gnc := listTG.funargs
	for _, inst := range listTG.insts[1:] {
		addmethods(typ0, inst[1].(*Type), gnc, inst[0].(*Type))
	}
}

// Usually the generics and concretes are *Types (with family TFGTS at that) but in GES case they can be other things
func (czi *ConczInfo) concretize(trm Term) Term {
	trm.Termfind(conczWF, czi)
	return czi.termPop()
}

// Below here we define most of the concrete types implementing the Term interface.
// Many of these concrete types are named TermX or TermXX where X is one of B, L, or T.
// B=Baselit, L=List, T=Term. E.g. TermTL is a struct with a Term slot and a []Term slot.
// The exceptions are types that have special properties: Symbol, Funinst, etc.
type TermB struct {
	kind         Termtag
	value        string
	first, final Pos
	plist        Plist
}

func makeStringTerm(strg string, first Pos, isRaw bool) *TermB {
	return &TermB{Stringlit, strg, first, first.plus(len(strg)), Plist{"isRaw", isRaw, nil}}
}

// Make an integer literal term from a string, eg from a token
func makeIntTerm(strg string, first Pos) Term {
	var value int
	if len(strg) > 2 && lower(strg[1]) == 'x' {
		fmt.Sscanf(strg[2:len(strg)], "%x", &value)
	} else {
		fmt.Sscanf(strg, "%d", &value)
	}
	return &TermB{Intlit, strg, first, first.plus(len(strg)), Plist{"intval", value, nil}}
}

// Make an integer literal term from an integer. Since this will always be synthetic, no position
func makeIntTerm2(val int) *TermB {
	intstrg := fmt.Sprintf("%d", val)
	return &TermB{Intlit, intstrg, Pos(-1), Pos(-1), Plist{"intval", val, nil}}
}

// Make a float literal term from a string, eg from a token
func makeFloatTerm(strg string, first Pos) Term {
	var value float64
	fmt.Sscanf(strg, "%f", &value)
	return &TermB{Floatlit, strg, first, first.plus(len(strg)), Plist{"floatval", value, nil}}
}

func makeLabelTerm(strg string, first Pos) Term {
	return &TermB{Labellit, strg, first, first.plus(len(strg)), zeroPlist()}
}

// In order to properly TC ELTs compared with label lits, we need a label Type per lit; this map prevents dups
var ELTMap = make(map[string]*Type, 30)

func labelType4Label(lbl string) *Type {
	typ := ELTMap[lbl]
	if typ == nil {
		typ = makeType0(TFLabel, nil, "label4"+lbl)
		typ.plist.Add("labellit", lbl)
		ELTMap[lbl] = typ
	}
	return typ
}

// use for Bytelit and Boollit.
func makeTermB(tg Termtag, strg string, first Pos) *TermB {
	return &TermB{tg, strg, first, first.plus(len(strg)), zeroPlist()}
}

func makeErrorTerm(strg string, first, last Pos) *TermB {
	return &TermB{ErrorTag, strg, first, last, zeroPlist()}
}

// Some special makers for TermB's
func makeIntlit(val int) *TermB {
	ret := makeTermB(Intlit, fmt.Sprint(val), Pos(-1))
	ret.plist.Add("intval", val)
	return ret
}

func (t *TermB) First() Pos   { return t.first }
func (t *TermB) Final() Pos   { return t.final }
func (t *TermB) Tag() Termtag { return t.kind }
func (t *TermB) Copy() Term   { return t }
func (t *TermB) Equal(other Term) bool {
	otb, ok := other.(*TermB)
	return ok && t.kind == otb.kind && t.value == otb.value
}
func (t *TermB) Plist() *Plist { return &t.plist }

// Dtype is computed, and most of the cases are obvious. ErrorTag less so; it can arise in TC.
func (t *TermB) Dtype() *Type {
	want := NoType
	switch t.kind {
	case Intlit:
		want = TFInt
		eltyp := t.plist.Find("ELType")
		if eltyp != nil {
			return eltyp.(*Type)
		}
	case Floatlit:
		want = TFFloat
	case Stringlit:
		want = TFString
	case Boollit:
		want = TFBool
	case Labellit:
		// treat as a label but record value for possible ELT
		cachetype := t.plist.Find("labeltype")
		if cachetype == nil {
			t.plist.Add("labeltype", makeType(TFLabellit, t.value))
		}
		return labelType4Label(t.value)
	case Bytelit:
		want = TFByte
	case ErrorTag:
		//fmt.Println("got dtype of errortag (", t.value, ")")
		want = TFNothing
	default:
		panic("impossible")
	}
	return basicTypes[want]
}

// Termfind is a traversal method designed for answering predicate style questions
// But since it has a dynamically typed state arg, it can be used for most anything.
// It visits subparts of terms in post-order. The bool return means stop if true.
func (t *TermB) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	// no internal structure so just call workfn and return result
	return workfn(t, state)
}

// helper for printing double quoted strings
func formatDQString(strg string) string {
	var bldr strings.Builder
	bldr.WriteByte('"')
	for _, c := range strg {
		if c == '\r' {
			bldr.WriteString(`\r`)
		} else if c == '\n' {
			bldr.WriteString(`\n`)
		} else if c == '\t' {
			bldr.WriteString(`\t`)
		} else {
			bldr.WriteRune(c)
		}
	}
	bldr.WriteByte('"')
	return bldr.String()
}

// Implement Stringer intfc for the tags that use TermB
func (trm0 *TermB) String() string {
	switch trm0.kind {
	case Intlit, Floatlit:
		return trm0.value
	case Stringlit:
		isRaw := trm0.plist.Find("isRaw").(bool)
		if isRaw {
			return fmt.Sprintf("`%s`", trm0.value)
		} else {
			return formatDQString(trm0.value)
		}
	case Labellit:
		return fmt.Sprintf(":%s", trm0.value)
	case Boollit:
		return trm0.value
	case Bytelit:
		bytestrg := trm0.value
		byteval := bytestrg[0]
		if byteval == '\n' {
			bytestrg = `\n`
		} else if byteval == '\t' {
			bytestrg = `\t`
		} else if byteval == '\r' {
			bytestrg = `\r`
		}
		return fmt.Sprintf("'%s'", bytestrg)
	case ErrorTag:
		lci0 := trm0.plist.Find("linechar")
		lcinfo := ""
		if lci0 != nil {
			lci := lci0.(lineCharInfo)
			lcinfo = fmt.Sprintf("%s:%d:%d ", lci.fname, lci.linenum, lci.colnum)
		}
		return lcinfo + trm0.value
	default:
		panic("bad tag")
	}
}

// These are for non-source-derived uses of these built in literals.
var TrueLiteral = &TermB{Boollit, "true", Pos(-1), Pos(-1), zeroPlist()}
var FalseLiteral = &TermB{Boollit, "false", Pos(-1), Pos(-1), zeroPlist()}

// These are for parser-level signalling of the ':' vs '::' distinction in sym decls.
var OneColon = SynthToken(IDENT, "__single")
var TwoColon = SynthToken(IDENT, "__double")

// give Token the Term interface so the parser can spit out IDENT tokens for
// conversion by the typechecker into Symbols.

func (t *Token) First() Pos   { return t.pos }
func (t *Token) Final() Pos   { return t.pos.plus(len(t.val)) }
func (t *Token) Tag() Termtag { return Ident }
func (t *Token) Equal(other Term) bool {
	otk, ok := other.(*Token)
	return ok && t.val == otk.val
}
func (t *Token) Dtype() *Type  { return nil }
func (t *Token) Copy() Term    { panic("shouldn't happen") }
func (t *Token) Plist() *Plist { return nil } // tokens do not have plists

// Termfind is a traversal method designed for answering predicate style questions
func (t *Token) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	// no internal structure so just call workfn and return result
	return workfn(t, state)
}

// Symbol holds the interned form of all symbols looked up in a symbol table.
// This includes fml args to ftns, despite the fact that these can't really have
// bindings at compile time. Sym.binding == nil doesn't mean it's a fml arg, because
// symbols can be decld without bindings.
//
// Most symtbls use a SymXDict to hold the symbols; this handles storing them in
// sorted order for lg(N) lookup, and handling multibinding issues.
type Symbol struct {
	ident     string
	dtype     *Type
	binding   Term // can be nil, can be a *Type, etc.
	plist     Plist
	homescope *Scope // set in Scope.Add
}

func (s *Symbol) First() Pos {
	tkn := s.plist.Find("token")
	if tkn == nil {
		return Pos(-1)
	} else {
		tkn0 := tkn.(*Token)
		return tkn0.First()
	}
}
func (s *Symbol) Final() Pos {
	tkn := s.plist.Find("token")
	if tkn == nil {
		return Pos(-1)
	} else {
		tkn0 := tkn.(*Token)
		return tkn0.Final()
	}
}
func (s *Symbol) Tag() Termtag { return SymbolTag }
func (s *Symbol) String() string {
	return s.ident
}
func (s *Symbol) Equal(other Term) bool {
	if s == other {
		return true
	}
	osym, ok := other.(*Symbol)
	seen := []*Type{}
	ok = ok && s.ident == osym.ident && s.dtype.equal(osym.dtype, &seen)
	if ok {
		ok = (s.binding == nil && osym.binding == nil) || (s.binding != nil && s.binding.Equal(osym.binding))
	}
	return ok
}
func (s *Symbol) Dtype() *Type  { return s.dtype }
func (s *Symbol) Copy() Term    { return s }
func (s *Symbol) Plist() *Plist { return &s.plist }

// Termfind is a traversal method designed for answering predicate style questions. For Symbol,
// we don't pursue the binding; if you care about it, you can put a traversal in the workfn.
func (t *Symbol) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	return workfn(t, state)
}

// Used when smashing the ident of a Symbol to _. Must remove from scope.
func (sym *Symbol) smash2Anon() {
	sym.ident = "_"
	if sym.homescope != nil {
		sym.homescope.entries.Remove(sym)
		sym.homescope = nil
	}
}

// make a Symbol installed in scp (isBI true iff scp == nil).
func makeSymbol(id string, typ *Type, val Term) *Symbol {
	sym := Symbol{id, typ, val, zeroPlist(), nil}
	return &sym
}

// A GTS is what's in the v slot of a TFGTS Type, which represents a "generic type symbol".
// As such a GTS is essentially a synonym for Symbol, and that's how the code started out.
// But I found this sufficiently confusing that I opted to create a distinct type.
// The binding slot of a GTS is used when gnc ftns are called, the value is whatever
// concrete type the GTS gets bound to via pattern matching.
type GTS struct {
	ident   string
	dtype   *Type
	binding *Type
}

func (gts *GTS) String() string { return gts.ident }

// A GTS binds an identifier to a Typred, which comes wrapped in a Type.
// The binding slot will be used for instantiating gnc ftns and the like
func makeGTS(nm string, typ *Type) *GTS { return &GTS{nm, typ, nil} }

// Funinst is a Term that gets made when we intern FtnLit tagged Terms from the parser.
// For BI Funinsts, all of scope, funargs, and body can be nil. In this case, there
// will normally be something in the plist to guide backend.
type Funinst struct {
	scope     *Scope    // for args and direct locals
	funargs   []*Symbol // these are in decln order, they're also in the scope
	dtype     *Type     // ftn type
	body      *TermL    // will be Stmts tag
	plist     Plist
	isLocalfn bool // help backend decide when to treat functions as gbls in generated code
	codesize  int  // a measure of code size used for inlining decisions in the backend
}

func (funi *Funinst) First() Pos   { return Pos(-1) }
func (funi *Funinst) Final() Pos   { return Pos(-1) }
func (funi *Funinst) Tag() Termtag { return FuninstTag }
func (funi *Funinst) Equal(other Term) bool {
	// not sure what to do here; following is quick and probably wrong
	if funi == other {
		return true
	}
	ofuni, ok := other.(*Funinst)
	if !ok {
		return false
	}
	if len(funi.funargs) != len(ofuni.funargs) {
		return false
	}
	for i, a := range funi.funargs {
		b := ofuni.funargs[i]
		if !a.Equal(b) {
			return false
		}
	}
	if !funi.dtype.v.(*Ftntype).rettype.Equal(ofuni.dtype.v.(*Ftntype).rettype) {
		return false
	}
	if (funi.body == nil && ofuni.body != nil) || (ofuni.body == nil && funi.body != nil) {
		return false
	}
	if funi.body != nil {
		return funi.body.Equal(ofuni.body)
	}
	return true
}
func (funi *Funinst) Dtype() *Type { return funi.dtype }
func (funi *Funinst) Copy() Term {
	cpargs := make([]*Symbol, len(funi.funargs))
	copy(cpargs, funi.funargs)
	nfuni := makeFuninst(funi.scope, cpargs, funi.dtype, funi.body.Copy().(*TermL))
	return nfuni
}
func (funi *Funinst) Plist() *Plist                         { return &funi.plist }
func (funi *Funinst) Typecheck(typ *Type, tci *TCInfo) Term { return nil }
func (funi *Funinst) String() string {
	if funi.body == nil { // assume this means BI
		typstrg := funi.dtype.String()
		return fmt.Sprintf("%s { ...BI ... }", typstrg)
	}
	var bldr strings.Builder
	bldr.WriteString(`\(`)
	last := len(funi.funargs) - 1
	for i, a := range funi.funargs {
		bldr.WriteString(a.ident)
		if i != last {
			bldr.WriteString(",")
		}
	}
	bldr.WriteString(") { ")
	if len(funi.body.args) == 1 {
		bldr.WriteString(funi.body.args[0].String())
	} else {
		bldr.WriteString("\n\t")
		for _, a := range funi.body.args {
			bldr.WriteString(a.String())
			bldr.WriteString("\n\t")
		}
	}
	bldr.WriteString(" }")
	return bldr.String()
}

// Return an index indicating the position of the first gnc found in the signature, or -1 if none.
// The index is 0...nargs for args, nargs for rettype
func (funi *Funinst) anyGncSignature() int {
	fntyp := funi.dtype.v.(*Ftntype)
	if fntyp.rettype.family == TFTypred {
		return len(fntyp.fmlargs)
	}
	for i, at := range fntyp.fmlargs {
		if at.family == TFTypred {
			return i
		}
	}
	return -1
}

// Inlining policy here: not recursive, no return stmts, smallish (codesize metric < 65) or good reason.
// Idea is for "good reason" to be extensible; initially it is that the fn has gnc args and typecase
// internally and is called with type-specific args s.t. Simplify will reduce the typecase to a single one.
func (funi *Funinst) inlineable(fnargs []Term) bool {
	if funi.body == nil || funi.plist.Find("returns") != nil || funi.plist.Find("noInline") != nil {
		return false
	}
	recursive := funi.plist.Find("recursive") != nil
	always := funi.plist.Find("alwaysInline") != nil
	if funi.codesize == 0 {
		genCodesize(funi)
	}
	smallish := funi.codesize < 65
	if (smallish || always) && !recursive {
		return true
	}
	ok := false
	gncinx := funi.anyGncSignature()
	if gncinx >= 0 {
		// need that the corresponding actual is concrete and the body has a typecase expr or stmt
		var fam TypeFamily
		if gncinx < len(fnargs) {
			fam = fnargs[gncinx].Dtype().family
		} else {
			fam = TFFtn // Fix to make rettypes work
		}
		if fam != TFTypred {
			for _, s := range funi.body.args {
				if s.Tag() == Typecase {
					ok = true
					break
				}
			}
		}
	}
	return ok
}

// This Termfind only explores the body; other checking must be done in the workfn.
func (funi *Funinst) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	if funi.body != nil && funi.body.Termfind(workfn, state) {
		return true
	}
	return workfn(funi, state)
}

// When a Funinst is defined, the spec requires that list+space valued args that are "enumonly"
// be handled in TC so as to allow any enumerable entity to be passed in those args. We enable
// this by marking the matching funargs (in the Funinst) with a plist item "enumonly". The condition
// is that the only uses of the funarg are (1) as the enumeree of each; (2) as a ftn arg that's
// tagged as enumonly. So the defn is recursive. We use the TraversePre ftn with a workfn that bails
// as soon as it finds a non-enumonly use of the funarg. Thus, this searches for the opposite of the
// condition we're looking for so we invert the sense when deciding whether to tag the funarg.
func (funi *Funinst) markEnumonly() {
	var eoworkfn func(trm Term, st interface{}) int
	eoworkfn = func(trm Term, st interface{}) int {
		funarg := st.(*Symbol)
		ret := 0
		if trm == funarg {
			return -1
		}
		tg := trm.Tag()
		if tg == EachStmt {
			trm0 := trm.(*TermL)
			found := TraversePre(trm0.args[2], eoworkfn, st)
			if found || trm0.args[1] != funarg && TraversePre(trm0.args[1], eoworkfn, st) {
				return -1
			}
			return 1 // don't search again
		} else if tg == Funcall {
			trm0 := trm.(*TermTL)
			// fail if actual matches funarg not marked as enumonly
			fn := trm0.trm
			if fn.Tag() == SymbolTag {
				fn = fn.(*Symbol).binding
			}
			for i, actl := range trm0.args {
				if actl == funarg {
					if fn == nil || fn.(*Funinst).funargs[i].plist.Find("enumonly") == nil {
						return -1
					}
				} else {
					if TraversePre(actl, eoworkfn, st) {
						return -1
					}
				}
			}
			ret = 3 // search nothing
			if trm0.trm.Tag() == FuninstTag {
				ret = 2 // search only the trm slot, ie the funinst
			}
		} else if tg == SymbolTag {
			ret = 1
		} else if tg == Vardecl || tg == Valdecl {
			if TraversePre(trm.(*TermT).arg0.(*Symbol), eoworkfn, st) {
				return -1
			}
			ret = 1
		}
		return ret
	}
	for i := 0; i < len(funi.funargs); i++ {
		fmlty := funi.funargs[i].dtype
		if fmlty.family == TFList || fmlty.family == TFSpace {
			if !TraversePre(funi.body, eoworkfn, funi.funargs[i]) {
				funi.funargs[i].plist.Add("enumonly", true)
			}
		}
	}
}

func makeFuninst(ls *Scope, args []*Symbol, typ *Type, stmts *TermL) *Funinst {
	funi := Funinst{ls, args, typ, stmts, zeroPlist(), false, 0}
	return &funi
}

// A funiCopySV holds state info needed to copy the body correctly. The obscure part has to do with embedded type
// defns, these are copied in the recursive part of the bodyCopy method and then type-substd at the end.
type funiCopySV struct {
	// first attribs are essentially params
	scope    *Scope
	nscope   *Scope
	typedefs []*Type // this is a dict with pairs of before/after types
}

// Analogous to concretization: handle the cases where types change, in this case only locally
// defined types, whose original and copy have been placed on the typedefs list. These will be typesubst'd into
// typ pair by pair.
func (fcsv *funiCopySV) handleType(typ *Type) *Type {
	if typ == nil {
		return typ
	}
	rettyp := typ
	for i := 0; i < len(fcsv.typedefs); i += 2 {
		tsi := &typesubstInfo{fcsv.typedefs[i], fcsv.typedefs[i+1], make([]*Type, 0, 4)}
		rettyp = tsi.typeSubstn(rettyp)
	}
	return rettyp
}

// Copy trm with fcsv params
func (fcsv *funiCopySV) bodyCopy(trm Term) (rslt Term) {
	rslt = trm
	if trm == nil {
		return
	}
	switch trm0 := trm.(type) {
	case *Symbol:
		inx := fcsv.scope.entries.Index(trm0.ident)
		if inx < len(fcsv.scope.entries) && fcsv.scope.entries[inx].sym == trm0 {
			rslt = fcsv.nscope.entries[inx].sym
			return
		}
		// search up the scope tree too; this scope tree only covers scopes in the funi being copied
		nscopex := fcsv.nscope.parent
		for scopex := fcsv.scope.parent; nscopex != nil && scopex != nil; {
			inx = scopex.entries.Index(trm0.ident)
			if inx < len(scopex.entries) && scopex.entries[inx].sym == trm0 {
				for ; nscopex != nil; nscopex = nscopex.parent {
					// possible bug: if a scope can have multibound symbols, failing to search fwd can miss one
					// todo: check into this
					sym := nscopex.entries.Find(trm0.ident)
					if sym != nil {
						return sym
					}
				}
				panic("should not happen")
			}
			scopex = scopex.parent
			nscopex = nscopex.parent
		}
		// anything outside the scopes being copied can stay as is
	case *TermB:
		// nothing to do here
	case *Funinst:
		if trm0.body == nil {
			return
		}
		nscope := makeScope()
		nscope.parent = fcsv.nscope
		trm0.scope.parent = fcsv.scope
		// first copy scope to nscope but without pursuing bindings
		for _, sx := range trm0.scope.entries {
			funarg2 := makeSymbol(sx.sym.ident, fcsv.handleType(sx.sym.dtype), sx.sym.binding)
			funarg2.plist = *sx.sym.plist.Copy()
			nscope.Add(funarg2, sx.xcount)
		}
		// next, make a copy of the funargs
		fmls2 := make([]*Symbol, len(trm0.funargs))
		for i, fa := range trm0.funargs {
			inx := trm0.scope.entries.Index(fa.ident)
			fmls2[i] = nscope.entries[inx].sym
		}
		// now push scopes and recurse
		fcsv.scope = trm0.scope
		fcsv.nscope = nscope
		nbody := fcsv.bodyCopy(trm0.body).(*TermL)
		nscope.importPkgs = trm0.scope.importPkgs
		nscope.importMdls = trm0.scope.importMdls
		nscope.stitems = trm0.scope.stitems
		nscope.stActives = trm0.scope.stActives
		nscope.rwActives = trm0.scope.rwActives
		nscope.plist = *trm0.scope.plist.Copy()
		fcsv.scope = trm0.scope.parent
		fcsv.nscope = nscope.parent
		nfuni := makeFuninst(nscope, fmls2, fcsv.handleType(trm0.dtype), nbody)
		nfuni.isLocalfn = trm0.isLocalfn
		rslt = nfuni
	case *TermT:
		if trm0.kind == Valdecl || trm0.kind == Vardecl {
			// The new symbol is created and in nscope but (1) put it here; and (2) copy the binding
			sym := trm0.arg0.(*Symbol)
			inx := fcsv.scope.entries.IndexExact(sym)
			nsym := fcsv.nscope.entries[inx].sym
			nsym.dtype = fcsv.handleType(sym.dtype)
			if sym.binding == nil {
				nsym.binding = makeTermT(ZeroValue, nil, nsym.dtype, Pos(-1), Pos(-1))
			} else if sym.dtype == nil {
				// the following probably fails on local typegen defns and suchlike...
				if sym.binding.Tag() == TypeTag {
					etyp := sym.binding.(*Type)
					nsym.binding = etyp
					if len(etyp.methods) > 0 {
						ntyp := etyp.Copy().(*Type)
						nsym.binding = ntyp
						fcsv.typedefs = append(fcsv.typedefs, etyp, ntyp)
						tsi := &typesubstInfo{etyp, ntyp, make([]*Type, 0, 2)}
						for i, ms := range etyp.methods {
							nmthdtyp := tsi.typeSubstn(ms.sym.dtype)
							nbdg := fcsv.bodyCopy(ms.sym.binding)
							nmthd := makeSymbol(ms.sym.ident, nmthdtyp, nbdg)
							ntyp.methods[i].sym = nmthd
						}
					}
				}
			} else {
				nsym.binding = fcsv.bodyCopy(sym.binding)
			}
			rslt = makeTermT(trm0.kind, nsym, TypeNothing, trm0.first, trm0.final)
		} else {
			var a0 Term
			if trm0.arg0 != nil {
				a0 = fcsv.bodyCopy(trm0.arg0) // get processed arg0
			}
			ntyp := fcsv.handleType(trm0.dtype)
			if ntyp != trm0.dtype || a0 != trm0.arg0 {
				rslt = makeTermT(trm0.kind, a0, ntyp, trm0.first, trm0.final)
			}
		}
	case *TermTT:
		narg0 := fcsv.bodyCopy(trm0.arg0)
		narg1 := fcsv.bodyCopy(trm0.arg1)
		rslt = makeTermTT(trm0.kind, narg0, narg1, fcsv.handleType(trm0.dtype), trm0.first, trm0.final)
		//goland:noinspection GoNilness
	case *TermL:
		// a TermL has a scope slot which we need to recreate
		var lscope *Scope
		var svscope *Scope
		if trm0.scope != nil {
			lscope = makeScope()
			for _, sx := range trm0.scope.entries {
				funarg2 := makeSymbol(sx.sym.ident, sx.sym.dtype, nil)
				funarg2.plist = *sx.sym.plist.Copy()
				lscope.Add(funarg2, sx.xcount)
			}
			lscope.parent = fcsv.nscope
			svscope = fcsv.scope
			fcsv.scope = trm0.scope
			fcsv.nscope = lscope
		}
		nargs := make([]Term, 0, len(trm0.args))
		for _, s := range trm0.args {
			if s.Tag() != AssertStmt { // filter asserts here
				nargs = append(nargs, fcsv.bodyCopy(s))
			}
		}
		if trm0.scope != nil {
			fcsv.scope = svscope
			fcsv.nscope = lscope.parent
		}
		rslt = makeScopedTermL(trm0.kind, lscope, nargs, fcsv.handleType(trm0.dtype), trm0.first, trm0.final)
	case *TermTL:
		ntrm := fcsv.bodyCopy(trm0.trm)
		nargs := make([]Term, len(trm0.args))
		for i, s := range trm0.args {
			nargs[i] = fcsv.bodyCopy(s)
		}
		rslt = makeTermTL(trm0.kind, ntrm, nargs, fcsv.handleType(trm0.dtype), trm0.first, trm0.final)
	case *Type: // in typecase
	default:
		panic("should not happen")
	}
	return
}

// Here's the entry point for copying funinst bodies
func (funi *Funinst) copyFuninst() *Funinst {
	var fcsv = &funiCopySV{funi.scope.parent, funi.scope.parent, nil}
	nfuni := fcsv.bodyCopy(funi).(*Funinst)
	nfuni.plist = *funi.plist.Copy()
	nfuni.isLocalfn = funi.isLocalfn
	return nfuni
}

// Typegen is derived from FtnLit (like Funinst), but fairly different internally.
// Most Typegens are fairly complicated types, typically tuple-based types with lots of methods.
// As such, each instantiation involves significant copying of generic Terms to concrete ones.
// To save some time+space, we cache these in insts.
type Typegen struct {
	dtype   *Type // is the generic type it's going to return when instantiated.
	funargs []*Symbol
	insts   [][]Term // see add2Cache and checkCache methods for details of how insts is used
	plist   Plist
}

func makeTypegen(typ *Type, args []*Symbol) *Typegen {
	tg := Typegen{typ, args, nil, zeroPlist()}
	return &tg
}

func (tg *Typegen) First() Pos                            { return Pos(-1) }
func (tg *Typegen) Final() Pos                            { return Pos(-1) }
func (tg *Typegen) Tag() Termtag                          { return TypegenTag }
func (tg *Typegen) Dtype() *Type                          { return nil }
func (tg *Typegen) Copy() Term                            { panic("shouldn't happen") }
func (tg *Typegen) Plist() *Plist                         { return &tg.plist }
func (tg *Typegen) Typecheck(typ *Type, tci *TCInfo) Term { panic("impossible"); return nil }
func (tg *Typegen) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	return workfn(tg, state)
}

func (tg *Typegen) String() string {
	args := make([]string, len(tg.funargs))
	for i, bpt := range tg.funargs {
		sep := ":"
		var rhs string
		if bpt.dtype == nil {
			sep = "::"
			rhs = bpt.binding.(*Type).v.(*GTS).dtype.String()
		} else {
			rhs = bpt.dtype.String()
		}
		args[i] = fmt.Sprint(bpt.ident, sep, rhs)
	}
	return fmt.Sprintf("\\typegen(%s) { %s }", strings.Join(args, ", "), tg.dtype.String())
}

// At this point I'm not sure whether Typegen.Equal should never be called. If so, should it be intensional?
func (tg *Typegen) Equal(other Term) bool {
	// similar issue to Funinst; postpone the problem
	return false
}

// Called when tg is "applied". Normally, the actuals are types but typegens can have GES args which invoke to entities.
// Hence we treat them as Terms.
// This entry point assumes no inferred generics with this typegen. Use the -0 entry point if that's not true.
func (tg *Typegen) Instantiate(actuals []Term) *Type {
	czi := makeConczInfo(tg.funargs, actuals)
	typ := tg.Instantiate0(czi)
	return typ
}

// This entry point to typegen instantiation is available for when the czi is already available.
func (tg *Typegen) Instantiate0(czi *ConczInfo) *Type {
	ret := tg.checkCache(czi.concretes)
	if ret != nil {
		return ret
	}
	czi.tgen = tg
	ret = czi.handleType(tg.dtype)
	tg.add2Cache(czi.concretes, ret)
	tgname0 := tg.plist.Find("typegenName")
	if tgname0 != nil {
		tstate := tgname0.(*struct {
			nm    string
			count int
		})
		tgname := fmt.Sprint(tstate.nm, tstate.count)
		tstate.count++
		ret.plist.Add("typename", tgname)
	}
	// clear out GTS bindings after each instantiation
	seen := make([]*Type, 0, 10)
	czi.addAux(tg)
	for _, tg0 := range czi.auxtg {
		for _, fa := range tg0.funargs {
			var aType *Type
			if fa.dtype == nil {
				aType = fa.binding.(*Type)
			} else {
				aType = fa.dtype
			}
			aType.clearGTS(&seen)
		}
	}
	ret.SetString()
	return ret
}

// checkCache (and add2Cache) are called when instantiating a typegen, see above.
// The cache of instantiated types is in the insts slot; each elt is a []Term
// of length 1+len(funargs), first elts are funarg values and last is result
// checkCache gets actuals, whose elts it matches up with its funargs; if these types match an elt of insts, it returns
// the previously instantiated type, else nil.
// At the point of program development where I start using assertions in types to affect stmapping and rewriting,
// I discover a need to distinguish the assertion state of types in the cache. Assertions are ignored for TC but
// not for stmapping and (certain aspects of) rewrite rule matching. Accordingly, we match based on both Equal of
// instantiation args (which ignores assertions) and Equal of the assertion slot of corresponding types. Unless both
// are affirmative, we report no match.
func (tg *Typegen) checkCache(actuals []Term) *Type {
	if len(tg.insts) == 0 {
		return nil
	}
	if len(actuals) != len(tg.insts[0])-1 {
		panic("bad arg to checkCache")
	}
	for _, ent := range tg.insts {
		allmatch := true
		for i, a := range actuals {
			if !ent[i].Equal(a) {
				allmatch = false
				break
			}
			if a.Tag() == TypeTag && !a.(*Type).asserts.Equal(ent[i].(*Type).asserts) {
				allmatch = false
				break
			}
		}
		if allmatch {
			return ent[len(ent)-1].(*Type)
		}
	}
	return nil
}

// Similar to checkCache but used to check if the typ was put into the cache earlier
// Search bkwds but return the index or -1
func (tg *Typegen) inxCached(typ *Type) int {
	cachesz := len(tg.insts)
	for i := 1; i <= cachesz; i++ {
		ce := tg.insts[cachesz-i]
		if ce[len(ce)-1] == typ {
			return cachesz - i
		}
	}
	return -1
}

// add2Cache gets actuals corresp to the funargs, and the result; it assembles
// these into an item it can add to tg's insts slot.
func (tg *Typegen) add2Cache(actuals []Term, rslt *Type) {
	if tg.checkCache(actuals) == nil {
		newelt := make([]Term, len(actuals)+1)
		copy(newelt, actuals)
		newelt[len(newelt)-1] = rslt
		rslt.generator = tg
		rslt.instInx = len(tg.insts)
		tg.insts = append(tg.insts, newelt)
	}
}

// Below are the "working class" term types that the parser uses

type TermT struct {
	first, final Pos
	kind         Termtag
	arg0         Term
	dtype        *Type
	plist        Plist
}

func makeTermT(tg Termtag, a0 Term, typ *Type, p0 Pos, pn Pos) Term {
	return &TermT{p0, pn, tg, a0, typ, zeroPlist()}
}

func (t *TermT) First() Pos   { return t.first }
func (t *TermT) Final() Pos   { return t.final }
func (t *TermT) Tag() Termtag { return t.kind }
func (t *TermT) Equal(other Term) bool {
	ott, ok := other.(*TermT)
	if t == ott {
		return true
	}
	if !ok || t.kind != ott.kind {
		return false
	}
	if t.kind == Gdref {
		return t.arg0.Equal(ott.arg0) && t.dtype.Equal(ott.dtype)
	}
	return t.arg0 == ott.arg0 || (t.arg0 != nil && ott.arg0 != nil && t.arg0.Equal(ott.arg0))
}
func (t *TermT) Dtype() *Type { return t.dtype }
func (t *TermT) Copy() Term {
	return makeTermT(t.kind, t.arg0.Copy(), t.dtype, t.first, t.final)
}

func (t *TermT) Plist() *Plist { return &t.plist }

func (t *TermT) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	if t.arg0 != nil && t.arg0.Termfind(workfn, state) {
		return true
	}
	return workfn(t, state)
}

func (pl *Plist) prComments() string {
	cmts := pl.Find("comments")
	if cmts == nil {
		return ""
	} else if c1, ok := cmts.(*Token); ok {
		return c1.val + "\n"
	} else { // must be []*Token
		var bldr strings.Builder
		cl := cmts.([]*Token)
		for _, c := range cl {
			bldr.WriteString(c.val)
			bldr.WriteString("\n")
		}
		return bldr.String()
	}
}

// Implement Stringer intfc for the tags that use TermT
func (trm *TermT) String() string {
	s0 := ""
	if trm.arg0 != nil {
		s0 = trm.arg0.String()
	}
	switch trm.kind {
	case AssertStmt:
		return fmt.Sprintf("%sassert %s", trm.plist.prComments(), s0)
	case RwsDeclStmt:
		return fmt.Sprintf("%srewriteStoreDeclare %s", trm.plist.prComments(), s0)
	case BreakStmt:
		return fmt.Sprintf("%sbreak", trm.plist.prComments())
	case Deferstmt:
		return fmt.Sprintf("%sdefer %s", trm.plist.prComments(), trm.arg0.String())
	case ContinueStmt:
		return fmt.Sprintf("%scontinue", trm.plist.prComments())
	case Gdref:
		return fmt.Sprintf("%sgdref[%s]", trm.plist.prComments(), trm.arg0.String())
	case Lognot:
		return fmt.Sprintf("!(%s)", trm.arg0.String())
	case LoopStmt:
		return fmt.Sprintf("%sloop %s", trm.plist.prComments(), s0)
	case Negate:
		return fmt.Sprintf("-(%s)", s0)
	case ZeroValue:
		return fmt.Sprintf("[def val for %s]", trm.dtype.String())
	case ReturnStmt:
		return fmt.Sprintf("%sreturn %s\n", trm.plist.prComments(), s0)
	case Range:
		a0 := trm.arg0.(*TermL)
		return fmt.Sprintf("range[%s,%s,%s,%s]", a0.args[0].String(), a0.args[1].String(), a0.args[2].String(), a0.args[3].String())
	case Valdecl, Vardecl, Fwddecl:
		wd := "val"
		if trm.kind == Vardecl {
			wd = "var"
		}
		if trm.arg0.Tag() == Symdecl { // this is a pre-TC Term
			trm0 := trm.arg0.(*TermL)
			typ := ""
			if trm0.args[1] != nil {
				typ = fmt.Sprintf(":%s", trm0.args[1].String())
			}
			bdg := ""
			if trm0.args[2] != nil {
				bdg = fmt.Sprintf(" = %s", trm0.args[2].String())
			}
			return fmt.Sprintf("%s %s%s%s", wd, trm0.args[0].(*Token).val, typ, bdg)
		}
		// Normal case here
		sym := trm.arg0.(*Symbol)
		if trm.kind == Vardecl && sym.plist.Find("var") == nil {
			wd = "var[unmarked]"
		}
		if trm.kind == Fwddecl {
			return fmt.Sprintf("%sfwddecl %s:%s", trm.plist.prComments(), sym.ident, sym.dtype.String())
		} else if sym.binding == nil {
			return fmt.Sprintf("%s%s %s:%s", trm.plist.prComments(), wd, sym.ident, sym.dtype.String())
		}
		if sym.dtype == nil {
			return fmt.Sprintf("%s%s %s:typish = %s", trm.plist.prComments(), wd, sym.ident, sym.binding.String())
		}
		rhs := "<empty>"
		if sym.binding != nil && sym.binding.Tag() != ZeroValue {
			rhs = sym.binding.String()
		}
		return fmt.Sprintf("%s%s %s:%s = %s", trm.plist.prComments(), wd, sym.ident, sym.dtype.String(), rhs)
	case TypeAssert:
		return fmt.Sprintf("%s.(%s)", s0, trm.dtype.String())
	case Typeterm:
		return fmt.Sprintf("[typeterm %s, %s]", trm.dtype.String(), s0)
	default:
		panic("bad tag")
	}
	return "" // not reached
}

type TermTT struct {
	first, final Pos
	kind         Termtag
	arg0         Term
	arg1         Term
	dtype        *Type
	plist        Plist
}

func makeTermTT(tg Termtag, a0 Term, a1 Term, typ *Type, p0 Pos, pn Pos) *TermTT {
	return &TermTT{p0, pn, tg, a0, a1, typ, zeroPlist()}
}

func (t *TermTT) First() Pos   { return t.first }
func (t *TermTT) Final() Pos   { return t.final }
func (t *TermTT) Tag() Termtag { return t.kind }
func (t *TermTT) Equal(other Term) bool {
	ott, ok := other.(*TermTT)
	return ok && (t == ott || (t.kind == ott.kind && t.arg0.Equal(ott.arg0) && t.arg1.Equal(ott.arg1)))
}
func (t *TermTT) Dtype() *Type { return t.dtype }
func (t *TermTT) Copy() Term {
	return makeTermTT(t.kind, t.arg0.Copy(), t.arg1.Copy(), t.dtype, t.first, t.final)
}
func (t *TermTT) Plist() *Plist { return &t.plist }

func (t *TermTT) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	if t.arg0 != nil && t.arg0.Termfind(workfn, state) {
		return true
	}
	if t.arg1 != nil && t.arg1.Termfind(workfn, state) {
		return true
	}
	return workfn(t, state)
}

// Implement Stringer intfc for the tags that use TermTT
func (trm *TermTT) String() string {
	s0 := trm.arg0.String()
	s1 := trm.arg1.String()
	// Special case == because the codes are messed up as a result of changing the extensional equality predicate from = to ==.
	if trm.kind == EqExpr {
		return fmt.Sprintf("%s == %s", s0, s1)
	}
	tc := ttag2tcode[trm.kind]
	if tc != ILLEGAL {
		opstrg := opstringMap[tc]
		scinf := shiftableCode[tc]
		// possibly parenthesize left and/or right operands based on relative precedences
		tclr := trm.arg0.Tag()
		if ttag2tcode[tclr] != ILLEGAL {
			scinf2 := shiftableCode[ttag2tcode[tclr]]
			if scinf2 != nil && scinf2.prec <= scinf.prec {
				s0 = fmt.Sprintf("(%s)", s0)
			}
		}
		tclr = trm.arg1.Tag()
		if ttag2tcode[tclr] != ILLEGAL {
			scinf2 := shiftableCode[ttag2tcode[tclr]]
			if scinf2 != nil && scinf2.prec <= scinf.prec {
				s1 = fmt.Sprintf("(%s)", s1)
			}
		}
		return fmt.Sprintf("%s %s %s", s0, opstrg, s1)
	}
	switch trm.kind {
	case AsgnStmt:
		return fmt.Sprintf("%s = %s", s0, s1)
	case Dot:
		return fmt.Sprintf("%s.%s", s0, s1)
	case Dotbrace:
		return fmt.Sprintf("%s.{ %s }", s0, s1)
	case Dotbracket:
		return fmt.Sprintf("%s.[%s]", s0, s1)
	case ImportStmt, ImportPkgStmt:
		pkg := ""
		if trm.kind == ImportPkgStmt {
			pkg = " go"
		}
		if s1 == "" {
			s1 = "melted"
		}
		return fmt.Sprintf("%simport%s %s %s\n", trm.plist.prComments(), pkg, s0, s1)
	case IndexExpr:
		return fmt.Sprintf("%s[%s]", s0, s1)
	case OncondStmt:
		return fmt.Sprintf("%soncondition %s %s\n", trm.plist.prComments(), s0, s1)
	case OntimeStmt:
		return fmt.Sprintf("%sontime %s %s\n", trm.plist.prComments(), s0, s1)
	case TildeExpr:
		return fmt.Sprintf("%s~%s", s0, s1)
	case WhileStmt:
		return fmt.Sprintf("%swhile(%s) %s\n", trm.plist.prComments(), s0, s1)
	case ExtendExpr:
		return fmt.Sprintf("extend %s where %s\n", s0, s1)
	case Typecond:
		return fmt.Sprintf("%s.(%s)", trm.arg0.String(), trm.arg1.(*Type).String())
	default:
		panic("bad tag")
	}
}

type TermL struct {
	first, final Pos
	kind         Termtag
	args         []Term
	dtype        *Type
	plist        Plist
	scope        *Scope // extra slot for Stmts tag and some other cases.
}

// This is the standard constructor but it enforces the rule that certain tags require scopes
func makeTermL(tg Termtag, az []Term, typ *Type, p0 Pos, pn Pos) *TermL {
	if tg == Stmts || tg == EachStmt {
		panic("use makeScopedTermL")
	}
	return &TermL{p0, pn, tg, az, typ, zeroPlist(), nil}
}

// Here's the alt constructor for TermL that must be called for Stmts and EachStmt tags.
func makeScopedTermL(tg Termtag, scp *Scope, az []Term, typ *Type, p0 Pos, pn Pos) *TermL {
	return &TermL{p0, pn, tg, az, typ, zeroPlist(), scp}
}
func makeSymchain(az []Term, typ *Type, p0 Pos, pn Pos) *TermL {
	return &TermL{p0, pn, Symchain, az, typ, zeroPlist(), nil}
}

func (t *TermL) First() Pos   { return t.first }
func (t *TermL) Final() Pos   { return t.final }
func (t *TermL) Tag() Termtag { return t.kind }
func (t *TermL) Equal(other Term) bool {
	otl, ok := other.(*TermL)
	ok = ok && t.kind == otl.kind && len(t.args) == len(otl.args)
	if ok && t == otl {
		return true
	}
	if ok {
		for i, a := range t.args {
			if !a.Equal(otl.args[i]) {
				ok = false
				break
			}
		}
	}
	return ok
}
func (t *TermL) Dtype() *Type { return t.dtype }
func (t *TermL) Copy() Term {
	cpargs := make([]Term, len(t.args))
	for i, a := range t.args {
		cpargs[i] = a.Copy()
	}
	return makeScopedTermL(t.kind, t.scope, cpargs, t.dtype, t.first, t.final)
}
func (t *TermL) Plist() *Plist { return &t.plist }

func (t *TermL) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	for _, a := range t.args {
		if a.Termfind(workfn, state) {
			return true
		}
	}
	return workfn(t, state)
}

func prSymdecl(t Term, tcd Termtag) string {
	tt := t.(*TermL) // also sure to be symdecl
	pvt := ""
	if tcd == TupleTypeLit && tt.args[2] == TrueLiteral {
		pvt = "private "
	}
	cln := ":"
	if tcd == GivenStmt && tt.args[2] == TrueLiteral {
		cln = "::"
	}
	return fmt.Sprintf("%s%s%s%s", pvt, tt.args[0].String(), cln, tt.args[1].String())
}

func (trm *TermL) String() string {
	switch trm.kind {
	case Symdecl: // can be pairs or triples; last elt in [OneColon, TwoColon] says bdgpt
		var bldr strings.Builder
		ident := trm.args[0].String()
		if len(trm.args) == 2 {
			fmt.Fprintf(&bldr, "%s:%s", ident, trm.args[1].String())
		} else {
			elt3 := trm.args[2]
			asTok, isTok := elt3.(*Token)
			if isTok && (asTok.val == "__single" || asTok.val == "__double") {
				colons := ":"
				if asTok.val == "__double" {
					colons = "::"
				}
				fmt.Fprintf(&bldr, "%s%s%s", ident, colons, trm.args[1].String())
			} else if trm.args[1] == nil {
				fmt.Fprintf(&bldr, "%s = %s", ident, elt3.String())
			} else if elt3 != nil {
				fmt.Fprintf(&bldr, "%s:%s = %s", ident, trm.args[1].String(), elt3.String())
			} else {
				fmt.Fprintf(&bldr, "%s:%s", ident, trm.args[1].String())
			}
		}
		return bldr.String()
	case Attribdecl, AttribdeclPvt, Methoddecl, MethoddeclPvt:
		var bldr strings.Builder
		fmt.Fprint(&bldr, trm.plist.prComments())
		if trm.kind == MethoddeclPvt || trm.kind == AttribdeclPvt {
			fmt.Fprint(&bldr, "private ")
		}
		if trm.kind == Attribdecl || trm.kind == AttribdeclPvt {
			fmt.Fprint(&bldr, "attribute ")
		} else {
			fmt.Fprint(&bldr, "method ")
		}
		strgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			strgs[i] = a.String()
		}
		fmt.Fprint(&bldr, strings.Join(strgs, ", "))
		return bldr.String()
	case SymdeclAttrib: // always pairs; occur only in typepred affordances
		return fmt.Sprintf("attribute %s:%s", trm.args[0].String(), trm.args[1].String())
	case TypepredExpr:
		// typepred list is [basepred, aff0, ...], each elt a Symdecl or SymdeclAttrib
		// the typepred-ness of basepred is not indicated so we need to stringify manually.
		bp := trm.args[0].(*TermL)
		bpstrg := fmt.Sprintf("%s::%s", bp.args[0], bp.args[1])
		argstrgs := []string{bpstrg}
		for _, aff := range trm.args[1:len(trm.args)] {
			argstrgs = append(argstrgs, aff.String())
		}
		return fmt.Sprintf("typepred(%s)", strings.Join(argstrgs, ", "))
	case MapExpr: // body is last, prior are 1+ iterators (usually 1)
		topinx := len(trm.args) - 1
		body := trm.args[topinx].String()
		itrstrgs := make([]string, 0, topinx)
		for _, itr := range trm.args[0:topinx] {
			itrstrgs = append(itrstrgs, itr.String())
		}
		return fmt.Sprintf("map(%s) %s", strings.Join(itrstrgs, ", "), body)
	case TupleTypeLit, OrtypeLit:
		wd := "tuple"
		if trm.kind == OrtypeLit {
			wd = "ortype"
		}
		strgs := make([]string, 0, len(trm.args))
		for _, a := range trm.args {
			strgs = append(strgs, prSymdecl(a, trm.kind))
		}
		return fmt.Sprintf("%s(%s)", wd, strings.Join(strgs, ", "))
	case AndandExpr, OrorExpr:
		midstrg := " && "
		if trm.kind == OrorExpr {
			midstrg = " || "
		}
		strgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			strgs[i] = a.String()
		}
		return strings.Join(strgs, midstrg)
	case Litform0:
		tt := fmt.Sprintf("%s: ", trm.dtype.String())
		strgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			if a == nil {
				strgs[i] = "!!NIL!!"
			} else {
				strgs[i] = a.String()
			}
		}
		return fmt.Sprintf("[%s%s]", tt, strings.Join(strgs, ", "))
	case Stmts:
		var bldr strings.Builder
		fmt.Fprint(&bldr, trm.plist.prComments())
		// smoke test on scope ordering
		//if trm.scope != nil {
		//	lim := len(trm.scope.entries) - 1
		//	for i, sx := range trm.scope.entries {
		//		if(i < lim && sx.sym.ident > trm.scope.entries[i+1].sym.ident) {
		//			panic("entries out of order")
		//		}
		//	}
		//}
		bldr.WriteString("{ ")
		nstmts := len(trm.args) // must be > 0
		stmtstrgs := make([]string, nstmts)
		bytecount := 0
		for inx, arg := range trm.args {
			stmtstrg := (arg).String()
			stmtstrgs[inx] = stmtstrg
			bytecount += len(stmtstrg)
		}
		if bytecount > 60 && nstmts > 1 {
			for _, stmtstrg := range stmtstrgs {
				bldr.WriteString("\n\t")
				bldr.WriteString(stmtstrg)
			}
			bldr.WriteString("\n}\n")
		} else {
			bldr.WriteString(strings.Join(stmtstrgs, "; "))
			bldr.WriteString(" }\n")
		}
		return bldr.String()
	case FunLit, FunTypeLit:
		// Funlit args is [a0,...ak,rettype,purity,body]. rettype maybe nil.
		// Funtypelit is same except without body, rettype may not be nil, the ai are types.
		nargs := len(trm.args)
		var body string
		if trm.kind == FunLit {
			body = trm.args[nargs-1].String()
			nargs -= 1
		}
		retstrg := ""
		if trm.args[nargs-2] != nil {
			retstrg = fmt.Sprintf("->%s", trm.args[nargs-2].String())
		}
		purstrg := ""
		purity := trm.args[nargs-1].(*Token)
		if purity.val != "pure" {
			purstrg = purity.val
		}
		args := make([]string, nargs-2)
		for i, a := range trm.args[0 : nargs-2] {
			args[i] = a.String()
		}
		return fmt.Sprintf("\\%s(%s)%s %s", purstrg, strings.Join(args, ", "), retstrg, body)
	case EachStmt: // here account for diffs of pre-TC and post-TC forms
		var bldr strings.Builder
		bldr.WriteString(trm.plist.prComments())
		bldr.WriteString("each(")
		fmt.Fprint(&bldr, trm.args[0].(*Symbol).ident, "^", trm.args[1].String())
		if len(trm.args) == 4 {
			fmt.Fprint(&bldr, ", ", trm.args[3].String())
		}
		bldr.WriteString(") ")
		bldr.WriteString(trm.args[2].String())
		return bldr.String()
	case IfStmt:
		var bldr strings.Builder
		fmt.Fprintf(&bldr, "%sif(%s) %s", trm.plist.prComments(), trm.args[0].String(), trm.args[1].String())
		if len(trm.args) > 2 {
			fmt.Fprintf(&bldr, " else %s", trm.args[2].String())
		}
		return bldr.String()
	case Ifcase:
		var bldr strings.Builder
		fmt.Fprintln(&bldr, "case {\n")
		for _, clz := range trm.args {
			clz0 := clz.(*TermTT)
			lhsstrg := clz0.arg0.String()
			if lhsstrg == "true" {
				lhsstrg = "else"
			}
			fmt.Fprintln(&bldr, lhsstrg, " => ", clz0.arg1.String())
		}
		fmt.Fprintln(&bldr, "}")
		return bldr.String()
	case StmapStmt:
		var bldr strings.Builder
		fmt.Fprint(&bldr, trm.plist.prComments())
		bldr.WriteString("STMap {\n")
		fmt.Fprintf(&bldr, "\tsource %s\n", trm.args[0].String())
		fmt.Fprintf(&bldr, "\ttarget %s\n", trm.args[1].String())
		xlt0 := 2
		if trm.args[2].Tag() == DirectStmt {
			xlt0 = 3
			decls := trm.args[2].(*TermL)
			declstrgs := make([]string, len(decls.args))
			for i, a := range decls.args {
				declstrgs[i] = a.String()
			}
			fmt.Fprintf(&bldr, "\tdirect %s\n", strings.Join(declstrgs, ", "))
		}
		for _, a := range trm.args[xlt0:len(trm.args)] {
			fmt.Fprintf(&bldr, "\t%s\n", a.String())
		}
		bldr.WriteString("}\n")
		return bldr.String()
	case ExportStmt:
		declstrgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			declstrgs[i] = a.String()
		}
		return fmt.Sprintf("export %s\n", strings.Join(declstrgs, ", "))
	case Symchain:
		chainstrgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			chainstrgs[i] = a.String()
		}
		return strings.Join(chainstrgs, ".")
	case Multiret:
		return fmt.Sprintf("%s, %s := %s\n", trm.args[0].String(), trm.args[1].String(), trm.args[2].String())
	case TypeAssertDecl:
		return fmt.Sprintf("%s, %s := %s\n", trm.args[0].String(), trm.args[1].String(), trm.args[2].String())
	default:
		panic("bad tag")
	}
}

type TermTL struct {
	first, final Pos
	kind         Termtag
	trm          Term
	args         []Term
	dtype        *Type
	plist        Plist
}

func makeTermTL(tg Termtag, trm Term, az []Term, typ *Type, p0 Pos, pn Pos) *TermTL {
	return &TermTL{p0, pn, tg, trm, az, typ, zeroPlist()}
}

func makeFuncall(fn Term, args []Term, rettyp *Type, tmpl Term) *TermTL {
	first := Pos(-1)
	final := Pos(-1)
	if tmpl != nil {
		first = tmpl.First()
		final = tmpl.Final()
	}
	return makeTermTL(Funcall, fn, args, rettyp, first, final)
}

func (t *TermTL) First() Pos   { return t.first }
func (t *TermTL) Final() Pos   { return t.final }
func (t *TermTL) Tag() Termtag { return t.kind }
func (t *TermTL) Equal(other Term) bool {
	otl, ok := other.(*TermTL)
	if ok && t == otl {
		return true
	}
	ok = ok && t.kind == otl.kind && len(t.args) == len(otl.args) && t.trm.Equal(otl.trm)
	if ok {
		for i, a := range t.args {
			if !a.Equal(otl.args[i]) {
				ok = false
				break
			}
		}
	}
	return ok
}
func (t *TermTL) Dtype() *Type  { return t.dtype }
func (t *TermTL) Plist() *Plist { return &t.plist }
func (t *TermTL) Copy() Term {
	cpargs := make([]Term, len(t.args))
	for i, a := range t.args {
		cpargs[i] = a.Copy()
	}
	return makeTermTL(t.kind, t.trm.Copy(), cpargs, t.dtype, t.first, t.final)
}

func (t *TermTL) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	if t.trm != nil && t.trm.Termfind(workfn, state) {
		return true
	}
	for _, a := range t.args {
		if a.Termfind(workfn, state) {
			return true
		}
	}
	return workfn(t, state)
}

func (trm *TermTL) String() string {
	switch trm.kind {
	case Litform:
		tt := ""
		if trm.trm != nil {
			tt = fmt.Sprintf("%s: ", trm.trm.String())
		}
		strgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			strgs[i] = a.String()
		}
		return fmt.Sprintf("[%s%s]", tt, strings.Join(strgs, ", "))
	case Funcall:
		ftn := trm.trm.String()
		strgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			strgs[i] = a.String()
		}
		tcd := stringopMap[ftn]
		if tcd == ILLEGAL {
			return fmt.Sprintf("%s(%s)", ftn, strings.Join(strgs, ", "))
		} else {
			// here, we've got a functionalized binop; print it as such.
			scinf := shiftableCode[tcd] // get the prec for adding parens
			tclr := trm.args[0].Tag()
			if tclr == Funcall {
				tfn := trm.args[0].(*TermTL).trm.String()
				if stringopMap[tfn] != ILLEGAL {
					tcd = stringopMap[tfn]
				}
			} else {
				tcd = ttag2tcode[tclr]
			}
			if tcd != ILLEGAL {
				scinf2 := shiftableCode[tcd]
				if scinf2 != nil && scinf2.prec <= scinf.prec {
					strgs[0] = fmt.Sprintf("(%s)", strgs[0])
				}
			}
			tclr = trm.args[1].Tag()
			if tclr == Funcall {
				tfn := trm.args[1].(*TermTL).trm.String()
				if stringopMap[tfn] != ILLEGAL {
					tcd = stringopMap[tfn]
				}
			} else {
				tcd = ttag2tcode[tclr]
			}
			if tcd != ILLEGAL {
				scinf2 := shiftableCode[tcd]
				if scinf2 != nil && scinf2.prec <= scinf.prec {
					strgs[1] = fmt.Sprintf("(%s)", strgs[1])
				}
			}
			return fmt.Sprintf("%s %s %s", strgs[0], ftn, strgs[1])
		}
	case GivenStmt:
		cmtstrg := trm.plist.prComments()
		body := trm.trm.String()
		strgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			strgs[i] = prSymdecl(a, trm.kind)
		}
		return fmt.Sprintf("%sgiven(%s) %s", cmtstrg, strings.Join(strgs, ", "), body)
	case Typecase:
		ortval := trm.trm.String()
		strgs := make([]string, len(trm.args))
		for i, a := range trm.args {
			strgs[i] = a.String()
		}
		return fmt.Sprintf("case %s {\n%s\n}", ortval, strings.Join(strgs, "\n"))

	default:
		panic("bad tag")
	}
}
