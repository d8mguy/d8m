// definitions and methods around types

package main

import (
	"fmt"
	"sort"
	"strings"
)

type TypeFamily int

const (
	NoType    TypeFamily = iota
	TFGTS                // not actually a type, but a GTS acting as one
	TFTypred             // ditto, this is a Typred masquerading as a Type.
	TFUnknown            // for the placeholder of an unbound type-valued Symbol
	TFNothing
	TFByte
	TFBit
	TFBool
	TFInt
	TFUint8 // for byte arithmetic
	TFInt64 // this is experimental, to help with int64's coming from pkg ftns
	TFFloat
	TFLabel
	TFLabellit // internal type "family" for the type of label literals which can TC to various ELTs
	TFString
	TFNil
	TFList
	TFSpace
	TFFtn
	TFTuple
	TFOrtype
	TFBaseQty
	TFQtyRatio
	TFTime
	TFExit
	TFPtr // used in backend to rep ptrzd types for codegen
	TFGochan
	TFGomap
	TFGoerror
	TFGoanything
)

var typeFamilyStrings = [...]string{
	NoType:       "INVALID TYPE",
	TFGTS:        "GTS",
	TFTypred:     "typepred",
	TFNothing:    "nothing",
	TFByte:       "byte",
	TFBit:        "bit",
	TFBool:       "boolean",
	TFInt:        "integer",
	TFUint8:      "uint8",
	TFInt64:      "integer",
	TFFloat:      "float",
	TFLabel:      "label",
	TFLabellit:   "label",
	TFString:     "string",
	TFNil:        "nil",
	TFList:       "list",
	TFSpace:      "space",
	TFFtn:        "\\",
	TFTuple:      "tuple",
	TFOrtype:     "ortype",
	TFQtyRatio:   "",
	TFTime:       "time",
	TFExit:       "exit",
	TFGochan:     "chan",
	TFGomap:      "map",
	TFGoerror:    "error",
	TFGoanything: "interface{}",
}

// This is used in codegen; it contains only the identifiers for unstructured go types
var gotypeFamilyStrings = []struct {
	fam   TypeFamily
	ident string
}{
	{TFBool, "bool"},
	{TFInt, "int"},
	{TFInt64, "int64"},
	{TFUint8, "uint8"},
	{TFFloat, "float64"},
	{TFLabel, "string"},
	{TFString, "string"},
	{TFByte, "byte"},
}

// Logically, assertions made of a type are just a []Term but internally, it can be helpful to pre-process
// these terms to make manifest eval easier. Accordingly, we define an "abstract type" for holding these
// assertions which gets eval methods, found in the symbolic file.
type AssertsOfType struct {
	tyself     *Symbol  // this is "self"
	clauses    []Term   // assertions other than binary funcalls
	binfNames  []string // binary funcalls are in these 3 slots, with self elided
	binfValues []Term
}

func (aot *AssertsOfType) Add(clz Term) {
	// special treatment of binary funcalls of the form F(self, X) or F(X, self)
	if clz.Tag() == Funcall {
		clz0 := clz.(*TermTL)
		if len(clz0.args) == 2 && clz0.trm.Tag() == SymbolTag {
			fnsym := clz0.trm.(*Symbol)
			a0 := clz0.args[0]
			a1 := clz0.args[1]
			if symbolNamed(a0, "self") && Manifest(a1) {
				aot.binfNames = append(aot.binfNames, fnsym.ident)
				aot.binfValues = append(aot.binfValues, a1)
				return
			} else if symbolNamed(a1, "self") && Manifest(a0) && isComparisonOp(fnsym.ident) {
				// can swap ordering oprs
				swaps := [][]string{{"<", ">"}, {"<=", ">="}, {">", "<"}, {">=", "<="}}
				alt := ""
				for _, sp := range swaps {
					if fnsym.ident == sp[0] {
						alt = sp[1]
						break
					}
				}
				// for now I'm going to make the further assumption that the 4 oprs are methods of the arg type(s).
				a0t := a0.Dtype()
				altfninx := a0t.methodIndex(alt)
				if a0t.methods[altfninx].sym.ident != alt {
					panic("can't happen")
				}
				aot.binfNames = append(aot.binfNames, alt)
				aot.binfValues = append(aot.binfValues, a0)
				return
			}
		}
	}
	aot.clauses = append(aot.clauses, clz)
}

func (aot *AssertsOfType) Count() int {
	if aot == nil {
		return 0
	}
	return len(aot.clauses) + len(aot.binfNames)
}

// compare two assertion sets, allowing nil
func (aot *AssertsOfType) Equal(other *AssertsOfType) bool {
	if aot == nil && other != nil {
		return false
	}
	if aot != nil && other == nil {
		return false
	}
	if aot == other {
		return true
	}
	// for now, a bog simple, incomplete, and inefficient comparison...
	if len(aot.clauses) != len(other.clauses) || len(aot.binfNames) != len(other.binfNames) {
		return false
	}
	for i, nm := range aot.binfNames {
		found := false
		for j, onm := range other.binfNames {
			if nm == onm {
				if !aot.binfValues[i].Equal(other.binfValues[j]) {
					return false
				}
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	for _, clz := range aot.clauses {
		found := false
		for _, oclz := range other.clauses {
			if clz.Equal(oclz) {
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

// Specialized function for the case where an assertion has a form that can induce a list guard. Specifically, if root
// and atrb are two adjacent elts of a symchain, there's a clause of the form count(self.X) != 0 or self.X != [], and
// X matches atrb, then the assertion induces a list guard on the active term. Note that we don't check the root elt of
// the active symchain because we assume the aot came from its type, so it's already matched.
func (aot *AssertsOfType) GuardsList(atrb *Symbol) bool {
	for _, asrtd := range aot.clauses {
		if !(asrtd.Tag() == Lognot && asrtd.(*TermT).arg0.Tag() == EqExpr) {
			continue
		}
		lhs := asrtd.(*TermT).arg0.(*TermTT).arg0
		rhs := asrtd.(*TermT).arg0.(*TermTT).arg1
		if callingNamed(lhs, "count") && rhs.Tag() == Intlit && rhs.(*TermB).value == "0" {
			countarg := lhs.(*TermTL).args[0]
			if countarg.Tag() == Symchain {
				ca0 := countarg.(*TermL)
				return len(ca0.args) == 2 && ca0.args[0].Tag() == SymbolTag && ca0.args[0].(*Symbol).ident == "self" && ca0.args[1].Equal(atrb)
			}
		} else if lhs.Tag() == Symchain {
			lhs0 := lhs.(*TermL)
			if len(lhs0.args) == 2 && lhs0.args[0].Tag() == SymbolTag && lhs0.args[0].(*Symbol).ident == "self" && lhs0.args[1].Equal(atrb) {
				return rhs.Tag() == Litform0 && len(rhs.(*TermL).args) == 0
			}
		}
	}
	return false
}

func (ty *Type) initAsserts() {
	if ty.asserts == nil {
		ty.asserts = &AssertsOfType{nil, nil, nil, nil}
	}
}

func (aot *AssertsOfType) Concretize(czi *ConczInfo) *AssertsOfType {
	ret := aot
	if aot.Count() > 0 {
		ret = aot.Copy()
		// ignoring binfFns on the grounds that they aren't used.
		for i, a := range aot.clauses {
			a.Termfind(conczWF, czi)
			a0 := czi.termPop()
			ret.clauses[i] = a0
		}
		for i, a := range aot.binfValues {
			a.Termfind(conczWF, czi)
			a0 := czi.termPop()
			ret.binfValues[i] = a0
		}
	}
	return ret
}

// Shallow copy of struct and of slices, not of structures contained therein
func (aot *AssertsOfType) Copy() *AssertsOfType {
	if aot == nil {
		return nil
	}
	var clz2 []Term
	var binfn2 []string
	var binfv2 []Term
	if aot.clauses != nil {
		clz2 = make([]Term, len(aot.clauses))
		copy(clz2, aot.clauses)
	}
	if aot.binfNames != nil {
		binfn2 = make([]string, len(aot.binfNames))
		binfv2 = make([]Term, len(aot.binfNames))
		copy(binfn2, aot.binfNames)
		copy(binfv2, aot.binfValues)
	}
	return &AssertsOfType{aot.tyself, clz2, binfn2, binfv2}
}

// Type is of course, a fundamental type here. One thing to note is that we choose
// to copy from base types when a type is extended. Thus, there is never any need
// to look up a chain of Type extensions to find a method, assertion, attribute, etc.
type Type struct {
	family TypeFamily
	// v will be nil for basic types; a Symbol for gnc; one of below defines for others.
	v            TypeDetails
	plist        Plist
	cachedString string
	generator    *Typegen // to accelerate comparisons: same generator means checks not needed other than structure
	instInx      int      // if generator set, tells which cache index for this type
	methods      SymXDict // holds all methods; xcount slot of SymX elts is ignored.
	asserts      *AssertsOfType
	handlers     []Term
	selfsym      *Symbol // in extended types this records the self symbol, which can appear in assertions; used for assert logic
}

func makeType0(fam TypeFamily, vnt TypeDetails, cs string) *Type {
	return &Type{fam, vnt, zeroPlist(), cs, nil, -1,
		nil, nil, nil, nil}
}

func (t *Type) String() string { return t.cachedString }
func (t *Type) SetString() {
	seen := []*Type{}
	t.cachedString = type2string(t, &seen)
}
func (t *Type) First() Pos   { return Pos(-1) }
func (t *Type) Final() Pos   { return Pos(-1) }
func (t *Type) Tag() Termtag { return TypeTag }
func (t *Type) Equal(other Term) bool {
	otyp, ok := other.(*Type)
	if ok && t == otyp {
		return true
	}
	seen := []*Type{}
	return ok && t.equal(otyp, &seen)
}
func (t *Type) Plist() *Plist                 { return &t.plist }
func (t *Type) Dtype() *Type                  { return nil }
func (t *Type) Typecheck(*Type, *TCInfo) Term { panic("shouldn't happen"); return nil }

func (t *Type) Termfind(workfn func(trm Term, stvar interface{}) bool, state interface{}) bool {
	return workfn(t, state)
}

// make it possible to sort lists of bdgpts (on ident), which are formally []*Symbol
type Bplst []*Symbol

func (bplst Bplst) Less(i, j int) bool { return bplst[i].ident <= bplst[j].ident }
func (bplst Bplst) Swap(i, j int)      { bplst[i], bplst[j] = bplst[j], bplst[i] }
func (bplst Bplst) Len() int           { return len(bplst) }
func (bplst Bplst) indexIdent(ident string) int {
	ret := -1
	for inx, elt := range bplst {
		if elt.ident == ident {
			ret = inx
			break
		}
	}
	return ret
}

// Also make Bplst searchable for containment
func (bplst Bplst) contains(other *Symbol) bool {
	for _, bp := range bplst {
		if bp.ident == other.ident && bp.dtype.compat(other.dtype, nil) {
			return true
		}
	}
	return false
}

// Next few type defns cover the concrete types that can go in TypeDetails
// Several families have something in v but not a dedicated type. These are
// TFList : *Type
// TFBaseQty : string

type TypeDetails interface{}

type Spacetype struct {
	elttype    *Type
	dimensions Term
}

// Attribute privacy is stored in the plist of each attrib.
type Tupletype struct {
	attribs  []*Symbol
	situated bool
}

// attribIndex method is used for anchor tagged litforms. Return index of attrib named ident or -1
func (tt *Tupletype) attribIndex(ident string) int {
	for i, a := range tt.attribs {
		if a.ident == ident {
			return i
		}
	}
	return -1
}

type Ftntype struct {
	fmlargs []*Type
	rettype *Type
	pure    bool
}

// The internal form orders the variants in lexical order of tag names. This may
// conflict with the user's preferred ordering but it's semantically identical.
// The nilposs case is handled with variants as a single Symbol with the main type.
type Ortype struct {
	variants  []*Symbol
	isNilposs bool
}

type ChanInfo struct {
	elttype *Type
	dir     string // will be "both", "receive", or "send"
}

type QtyRatioType struct {
	numer []*Type
	denom []*Type
}

// Typred occurs in Terms as the TypeDetails of a special TFTypred Type. The
// basepred slot is a Scope because (1) it's both an identifier and a Typred; and
// (2) it needs to be leaf of the scope tree when TC'g the affordances.
type Typred struct {
	familyset int // bitset of TypeFamily codes
	affs      symbolSlice
	attribset int    // bitset of indices into affs slice.
	basepred  *Scope // will only ever have 1 symbol, the basepred
	ident     string // installed in BI's and when UD's are decld; used for printing.
	plist     Plist
}

func (t *Typred) String() string { return t.ident }

func (t *Typred) DtlString() string {
	if len(t.affs) != 0 {
		var bldr strings.Builder
		bpsym := t.basepred.entries[0] // a Symbol with dtype in TFTypred family.
		tpred := bpsym.sym.dtype.v.(*Typred)
		fmt.Fprintf(&bldr, "typepred(%s::%s", bpsym.sym.ident, tpred.ident)
		if len(t.affs) > 0 {
			for i, aff := range t.affs {
				if i < len(t.affs)-1 {
					fmt.Fprint(&bldr, ", ")
				}
				if ((1 << uint(i)) & t.attribset) != 0 {
					fmt.Fprint(&bldr, "attribute ")
				}
				fmt.Fprintf(&bldr, "%s:%s", aff.ident, aff.dtype.String())
			}
		}
		fmt.Fprint(&bldr, ")")
		return bldr.String()
	}
	return t.ident
}

var entFS = (1 << uint(TFByte)) | (1 << uint(TFBit)) | (1 << uint(TFBool)) | (1 << uint(TFInt)) |
	(1 << uint(TFFloat)) | (1 << uint(TFLabel)) | (1 << uint(TFLabellit)) | (1 << uint(TFString)) | (1 << uint(TFNil)) |
	(1 << uint(TFList)) | (1 << uint(TFSpace)) | (1 << uint(TFFtn)) |
	(1 << uint(TFTuple)) | (1 << uint(TFOrtype)) | (1 << uint(TFBaseQty)) | (1 << uint(TFQtyRatio))
var TPEntity = makeType(TFTypred, &Typred{entFS, nil, 0, nil, "entity", zeroPlist()})
var TPList = makeType(TFTypred, &Typred{1 << uint(TFList), nil, 0, nil, "list", zeroPlist()})
var TPTuple = makeType(TFTypred, &Typred{1 << uint(TFTuple), nil, 0, nil, "tuple", zeroPlist()})
var TPNumber = makeType(TFTypred, &Typred{1<<uint(TFInt) | (1 << uint(TFFloat)), nil, 0, nil, "number", zeroPlist()})
var TPFunction = makeType(TFTypred, &Typred{1 << uint(TFFtn), nil, 0, nil, "function", zeroPlist()})
var TPQty = makeType(TFTypred, &Typred{(1 << uint(TFBaseQty)) |
	(1 << uint(TFQtyRatio)), nil, 0, nil, "quantity", zeroPlist()})
var TPModdable = makeType(TFTypred, &Typred{(1 << uint(TFTuple)) |
	(1 << uint(TFList)) | 1<<uint(TFSpace), nil, 0, nil, "moddable", zeroPlist()})

// hack alert: TFGochan codes for situated and TFExit for enumerable
var TPSituated = makeType(TFTypred, &Typred{(1 << uint(TFTuple)) |
	(1 << uint(TFGochan)), nil, 0, nil, "situated", zeroPlist()})
var TPEnumerable = makeType(TFTypred, &Typred{1 << uint(TFExit), nil, 0, nil, "enumerable", zeroPlist()})

// Make a Typred from the given info. Use the basepred to extract familyset and
// the next 2 args to etract affs and attribset. Affs comes in as afflist which is
// in no special ordering. Internally, we want affs to be ordered to simplify matching
// so do that here. The incoming attribflags are the indices in afflist of those
// required to be attributes. We assume the incoming afflist can be modded (via sorting)
// All of scp, afflist, attribflags may be nil.
func makeTypred(famset int, scp *Scope, afflist []*Symbol, oabits int, nm string) *Typred {
	if len(afflist) > 1 {
		sort.Sort(Bplst(afflist))
	}
	return &Typred{famset, afflist, oabits, scp, nm, zeroPlist()}
}

// extensionally the same?
func (self *Typred) equal(other *Typred) bool {
	if self == other {
		return true
	}
	if self.familyset != other.familyset || self.attribset != other.attribset ||
		len(self.affs) != len(other.affs) {
		return false
	}
	// check the affs modulo basepred
	seen := []*Type{}
	for i, aff := range self.affs {
		affo := other.affs[i]
		if aff.ident != affo.ident || !(aff.dtype.equal(affo.dtype, &seen)) {
			return false
		}
	}
	return true
}

// Same general idea as equal but <=. So every type that matches self must match other.
func (self *Typred) compat(other *Typred) bool {
	if other.ident == "moddable" {
		if self.ident == "list" || self.ident == "space" || self.ident == "tuple" {
			return true
		}
	}
	ntsk := self.familyset & other.familyset // intersect bitsets
	if ntsk != self.familyset || self.attribset != other.attribset || len(self.affs) > len(other.affs) {
		return false
	}
	// check the affs modulo basepred
	seen := []*Type{}
	i := 0
	// note that affs are sorted on idents. For self <= other, need that self.methods superset other.methods.
	for _, aff := range self.affs {
		affo := other.affs[i]
		if aff.ident == affo.ident && aff.dtype.equal(affo.dtype, &seen) {
			i += 1
		}
	}
	return i == len(other.affs)
}

// does typ meet the constraints of self?
func (tp *Typred) match(typ *Type) bool {
	// if tp is entity, return true
	if tp.ident == "entity" {
		return true
	}
	if typ.family == TFGTS {
		// check corresponding constraints
		gts := typ.v.(*GTS)
		return gts.dtype.v.(*Typred).compat(tp)
	}
	// first, check basepred
	if (tp.familyset & (1 << uint(TFExit))) != 0 {
		// tp is enumerable.
		enumflag := typ.plist.Find("enumerable")
		if enumflag == nil {
			return false
		}
	} else {
		typfs := (1 << uint(typ.family))
		asTpl, isTpl := typ.v.(*Tupletype)
		if isTpl {
			if (tp.familyset & (1 << uint(TFGochan))) != 0 { // TPSituated?
				if !asTpl.situated {
					return false
				}
			}
		}
		if (typfs & tp.familyset) == 0 {
			return false
		}
	}
	// basepred passes; if there are any affordances, check those.
	// Note that affordances and methods are both sorted in ident order.
	tminx := 0
	for ainx, aff := range tp.affs {
		// check attrib and then, if method allowed, method.
		if typ.family == TFTuple {
			if Bplst(typ.v.(*Tupletype).attribs).contains(aff) {
				continue
			}
		}
		if (1<<uint(ainx))&tp.attribset == 0 { // can be method?
			for tminx < len(typ.methods) && typ.methods[tminx].sym.ident != aff.ident {
				tminx += 1
			}
			if tminx < len(typ.methods) {
				gncs := aff.dtype.generics()
				defer func() {
					// delete any bindings that were created in the above calls to compat
					for _, gt := range gncs {
						gt.v.(*GTS).binding = nil
					}
				}()
				seen := []*Type{}
				if aff.dtype.compat(typ.methods[tminx].sym.dtype, &seen) {
					tminx += 1
					continue
				}
			}
		}
		// failed!
		return false
	}
	return true
}

// Below, ftns and methods on Type.

// methodIndex returns the index in t.methods of the first SymX with matching Symbol
// or -1 if no such index. It's up to the client to handle multibinding.
func (t *Type) methodIndex(id string) int {
	inx := t.methods.Index(id)
	if inx < len(t.methods) && t.methods[inx].sym.ident == id {
		return inx
	}
	return -1
}

// methodIndexByMethod returns the index in t.methods of the sym == the arg.
// It returns -1 if there is no such method.
func (t *Type) methodIndexByMethod(msym *Symbol) int {
	inx := t.methods.Index(msym.ident)
	for ; inx < len(t.methods); inx++ {
		if t.methods[inx].sym == msym {
			return inx
		}
	}
	return -1
}

// Use methodIndex to get the (first) method Symbol for id, or nil
func (t *Type) methodLookup(id string) *Symbol {
	inx := t.methodIndex(id)
	if inx < 0 {
		return nil
	}
	return t.methods[inx].sym
}

// wrapPointer builds in the assumption that we never generate **T for
// any type T. A type is either pointerized or not. This lets us avoid keeping
// track of things already pointerized, visited, or whatever.
func (t *Type) wrapPointer() *Type {
	if t.family == TFPtr {
		return t
	}
	return makeType0(TFPtr, t, fmt.Sprintf("*%s", t.cachedString))
}

func (t *Type) canAddMethod(mthd *Symbol) bool {
	inx := t.methodIndex(mthd.ident)
	if inx < 0 || t.methods[inx].sym.ident != mthd.ident {
		return true
	}
	// there's an existing method with the same identifier; check for TC-distinctness
	newtyp := mthd.dtype.v.(*Ftntype)
	xsttyp := t.methods[inx].sym.dtype.v.(*Ftntype)
	anydiff := len(newtyp.fmlargs) != len(xsttyp.fmlargs)
	if !anydiff {
		seen := []*Type{}
		for i, _ := range xsttyp.fmlargs {
			if !xsttyp.fmlargs[i].compat(newtyp.fmlargs[i], &seen) {
				anydiff = true
				break
			}
		}
		if !anydiff {
			anydiff = !xsttyp.rettype.compat(newtyp.rettype, &seen)
		}
	}
	return anydiff
}

// addMethod does ordered insert into methods, which is a SymXDict
func (t *Type) addMethod(mthd *Symbol) { t.methods.Add(mthd, 0) }

// Handlers in types don't get any special ordering.
func (t *Type) addHandler(hndlr Term) { t.handlers = append(t.handlers, hndlr) }

// This is the work ftn for Type.String() method. It may acquire other cntl flags later.
// Note that methods (and similar accoutrements) never participate in this string.
func type2string(typ *Type, seen *[]*Type) string {
	// if there's a typename, use that
	tnm := typ.plist.Find("typename")
	if tnm != nil {
		return tnm.(string)
	}
	// check for recursion
	inx := -1
	for finx, xtyp := range *seen {
		if typ == xtyp {
			inx = finx
			break
		}
	}
	if inx >= 0 {
		// yes, recursive. We already checked typename, so make something up.
		return fmt.Sprintf("ty@%d", inx)
	}
	// next, check for the type family that isn't a type family
	if typ.family == TFGTS {
		sym := typ.v.(*GTS)
		return sym.ident
	}
	var bldr strings.Builder
	// ptrzd types should probably never be printed this way but let's cover them in case
	if typ.family == TFPtr {
		bldr.WriteByte('*')
		bldr.WriteString(type2string(typ.v.(*Type), seen))
		return bldr.String()
	}
	bldr.WriteString(typeFamilyStrings[typ.family])
	if typ.v != nil {
		switch dtl := typ.v.(type) {
		case *Type: // has to be TFList
			*seen = append(*seen, typ)
			fmt.Fprintf(&bldr, "(%s)", type2string(dtl, seen))
		case *Spacetype:
			*seen = append(*seen, typ)
			fmt.Fprintf(&bldr, "(%s, %s)", type2string(dtl.elttype, seen), dtl.dimensions.String())
		case *Tupletype:
			*seen = append(*seen, typ)
			bldr.WriteByte('(')
			lim := len(dtl.attribs) - 1
			for inx, atrb := range dtl.attribs {
				if atrb != nil {
					fmt.Fprintf(&bldr, "%s:%s", atrb.ident, type2string(atrb.dtype, seen))
					if inx != lim {
						bldr.WriteString(", ")
					}
				}
			}
			bldr.WriteByte(')')
		case *Ortype:
			*seen = append(*seen, typ)
			bldr.WriteByte('(')
			lim := len(dtl.variants) - 1
			for inx, vnt := range dtl.variants {
				fmt.Fprintf(&bldr, "%s:%s", vnt.ident, type2string(vnt.dtype, seen))
				if inx != lim {
					bldr.WriteString(", ")
				}
			}
			if dtl.isNilposs {
				fmt.Fprint(&bldr, ", nil:nil")
			}
			bldr.WriteByte(')')
		case *Ftntype:
			*seen = append(*seen, typ)
			if dtl.pure {
				bldr.WriteByte('(')
			} else {
				bldr.WriteString("mod(")
			}
			lim := len(dtl.fmlargs) - 1
			for inx, arg := range dtl.fmlargs {
				seenlen := len(*seen)
				fmt.Fprint(&bldr, type2string(arg, seen))
				*seen = (*seen)[0:seenlen]
				if inx != lim {
					bldr.WriteString(", ")
				}
			}
			bldr.WriteString(")->")
			bldr.WriteString(type2string(dtl.rettype, seen))
		case string: // must be TFBaseQty
			bldr.WriteString(dtl)
		case *QtyRatioType:
			lim := len(dtl.numer) - 1
			for inx, qt := range dtl.numer {
				fmt.Fprint(&bldr, type2string(qt, seen))
				if inx != lim {
					bldr.WriteString("*")
				}
			}
			bldr.WriteString("/")
			lim = len(dtl.denom) - 1
			for inx, qt := range dtl.denom {
				fmt.Fprint(&bldr, type2string(qt, seen))
				if inx != lim {
					bldr.WriteString("*")
				}
			}
		case []string: // must be enumlisttype
			return fmt.Sprintf("label(%s)", strings.Join(dtl, ", "))
		case *Typred:
			return dtl.String()
		case []*Type:
			return fmt.Sprintf("Gomap(%s, %s)", dtl[0].String(), dtl[1].String())
		case *ChanInfo:
			chanpart := ""
			if dtl.dir == "both" {
				chanpart = "chan "
			} else if dtl.dir == "send" {
				chanpart = "<- chan "
			} else {
				chanpart = "chan <- "
			}
			return fmt.Sprint(chanpart, type2string(dtl.elttype, seen))
		default:
			return "missing case in type2string"
		}
	}
	return bldr.String()
}

var TypeNothing = makeType0(TFNothing, nil, "nothing")
var TypeByte = makeType0(TFByte, nil, "byte")
var TypeUint8 = makeType0(TFUint8, nil, "uint8")
var TypeBit = makeType0(TFBit, nil, "bit")
var TypeBool = makeType0(TFBool, nil, "boolean")
var TypeInt = makeType0(TFInt, nil, "integer")
var TypeInt64 = makeType0(TFInt64, nil, "int64")
var TypeFloat = makeType0(TFFloat, nil, "float")
var TypeLabel = makeType0(TFLabel, nil, "label")
var TypeString = makeType0(TFString, nil, "string")
var TypeNil = makeType0(TFNil, nil, "nil")
var TypeExit = makeType0(TFExit, nil, "exit")
var TypeTime = makeType0(TFTime, nil, "time")
var TypeGoerrorBase = makeType0(TFGoerror, nil, "goerror")
var TypeGoerror = func() *Type {
	t := makeType(TFOrtype, Bplst{makeSymbol("main", TypeGoerrorBase, nil), nilEntity})
	t.cachedString = "Goerror"
	t.plist.Add("noPointer", true) // protect against ptrzn in backend
	return t
}()
var TypeGoanything = makeType0(TFGoanything, nil, "interface{}")

var basicTypes = map[TypeFamily]*Type{
	TFNothing:    TypeNothing,
	TFByte:       TypeByte,
	TFBit:        TypeBit,
	TFBool:       TypeBool,
	TFInt:        TypeInt,
	TFInt64:      TypeInt64,
	TFFloat:      TypeFloat,
	TFLabel:      TypeLabel,
	TFLabellit:   TypeLabel,
	TFString:     TypeString,
	TFNil:        TypeNil,
	TFTime:       TypeTime,
	TFGoerror:    TypeGoerror,
	TFGoanything: TypeGoanything,
}

// Moddable is a predicate telling if t is moddable
func (t *Type) Moddable() bool {
	if t.family == TFTuple || t.family == TFList || t.family == TFSpace {
		return true
	}
	if t.family == TFGTS {
		gts := t.v.(*GTS)
		typred := gts.dtype.v.(*Typred)
		return typred.compat(TPModdable.v.(*Typred))
	}
	if t.family == TFTypred {
		typred := t.v.(*Typred)
		return typred.ident == "tuple" || typred.ident == "moddable" || typred.ident == "list"
	}
	return false
}

// isGeneric is true if t recursively contains a GTS
func (t *Type) isGeneric() bool {
	return t.generics() != nil
}

// Return the generic types used in t. Examine only the structural parts of t.
func (t *Type) generics() []*Type {
	seen := make([]*Type, 0, 4)
	ret := make([]*Type, 0, 2)
	t.generics0(&seen, &ret)
	if len(ret) == 0 {
		ret = nil
	}
	return ret
}

// Return the generic types used in the methods of t. If this and generics() give different
// answers, we've detected a case where a nested type defn uses generic symbols from the outer
// type defn, and methods need to be concretized even though the type structures aren't different.
func (t *Type) genericsInMethods() []*Type {
	seen := make([]*Type, 0, 4)
	ret := make([]*Type, 0, 2)
	for _, m := range t.methods {
		m.sym.dtype.generics0(&seen, &ret)
	}
	if len(ret) == 0 {
		ret = nil
	}
	return ret
}

// Recursion point for Type.generics
func (t *Type) generics0(seen, ret *[]*Type) {
	for _, t0 := range *seen {
		if t == t0 {
			return
		}
	}
	switch t.family {
	case TFGTS, TFTypred:
		for _, t0 := range *ret {
			if t == t0 {
				return
			}
		}
		*ret = append(*ret, t)
	case TFList:
		*seen = append(*seen, t)
		t.v.(*Type).generics0(seen, ret)
	case TFSpace:
		t.v.(*Spacetype).elttype.generics0(seen, ret)
	case TFOrtype:
		for _, vn := range t.v.(*Ortype).variants {
			vn.dtype.generics0(seen, ret)
		}
	case TFTuple:
		*seen = append(*seen, t)
		for _, at := range t.v.(*Tupletype).attribs {
			at.dtype.generics0(seen, ret)
		}
	case TFFtn:
		*seen = append(*seen, t)
		// the nil case can occur while handleType is working
		for _, at := range t.v.(*Ftntype).fmlargs {
			if at != nil {
				at.generics0(seen, ret)
			}
		}
		t.v.(*Ftntype).rettype.generics0(seen, ret)
	}
}

// Type.clearGTS clears bindings of any GTS directly or indirectly in type t
func (t *Type) clearGTS(seen *[]*Type) {
	for _, t0 := range *seen {
		if t == t0 {
			return
		}
	}
	switch t.family {
	case TFGTS:
		t.v.(*GTS).binding = nil
	case TFList:
		*seen = append(*seen, t)
		t.v.(*Type).clearGTS(seen)
	case TFSpace:
		t.v.(*Spacetype).elttype.clearGTS(seen)
	case TFOrtype:
		for _, vn := range t.v.(*Ortype).variants {
			vn.dtype.clearGTS(seen)
		}
	case TFTuple:
		*seen = append(*seen, t)
		for _, at := range t.v.(*Tupletype).attribs {
			at.dtype.clearGTS(seen)
		}
	case TFFtn:
		for _, at := range t.v.(*Ftntype).fmlargs {
			at.clearGTS(seen)
		}
		t.v.(*Ftntype).rettype.clearGTS(seen)
	}
}

// It seems (rather late in the game) that I sometimes need to create a Type in which all bound GTS get replaced
// with their bindings.
func (t *Type) stripBoundGTS() *Type {
	gncs := t.generics()
	absts := make([]*Symbol, 0, len(gncs))
	concs := make([]Term, 0, len(gncs))
	for _, gt := range gncs {
		if gt.v.(*GTS).binding != nil {
			absts = append(absts, makeSymbol("xx", nil, gt))
			concs = append(concs, gt.v.(*GTS).binding)
		}
	}
	if len(absts) == 0 {
		return t
	}
	czi := makeConczInfo(absts, concs)
	return czi.handleType(t)
}

// Entirely analogous to Type.clearGTS but used to verify that everything is clear that should be.
func (t *Type) checkGTS(seen *[]*Type) {
	for _, t0 := range *seen {
		if t == t0 {
			return
		}
	}
	switch t.family {
	case TFGTS:
		if t.v.(*GTS).binding != nil {
			panic("checkGTS")
		}
	case TFList:
		*seen = append(*seen, t)
		t.v.(*Type).checkGTS(seen)
	case TFOrtype:
		for _, vn := range t.v.(*Ortype).variants {
			vn.dtype.checkGTS(seen)
		}
	case TFSpace:
		t.v.(*Spacetype).elttype.checkGTS(seen)
	case TFTuple:
		*seen = append(*seen, t)
		for _, at := range t.v.(*Tupletype).attribs {
			at.dtype.checkGTS(seen)
		}
	case TFFtn:
		for _, at := range t.v.(*Ftntype).fmlargs {
			at.checkGTS(seen)
		}
		t.v.(*Ftntype).rettype.checkGTS(seen)
	}
}

// If t has a stringify method, return it. If not, create a body-less one, install it, and return that.
func (t *Type) stringifyMethod(inDefn *Symbol) *Symbol {
	mthd := t.methods.Find("stringify")
	if mthd == nil {
		if inDefn != nil && inDefn.ident == "stringify" {
			inDefn.plist.Add("recursive", true)
			return inDefn
		}
		ftyp := makeType(TFFtn, []*Type{t}, TypeString, true)
		argsym := makeSymbol("arg", t, nil)
		funi := makeFuninst(makeScope(), []*Symbol{argsym}, ftyp, nil)
		funi.scope.Add(argsym, 0)
		mthd = makeSymbol("stringify", ftyp, funi)
		t.addMethod(mthd)
	}
	return mthd
}

// attributeLookup returns the Symbol whose name is the arg, if t has such an attribute, else nil.
func (t *Type) attributeLookup(ident string) *Symbol {
	if t.family == TFTuple {
		tuptyp := t.v.(*Tupletype)
		for _, a := range tuptyp.attribs {
			if a.ident == ident {
				return a
			}
		}

	}
	return nil
}

// Elttype is nil just in case t is not enumerable. (IOW, it can serve as a predicate.)
// If t is enumerable, it returns its elttype.
func (t *Type) Elttype() *Type {
	switch t.family {
	case TFList:
		return t.v.(*Type)
	case TFSpace:
		return t.v.(*Spacetype).elttype
	case TFTypred:
		return TPEntity // this isn't right but it's a reasonable first approximation
	case TFTuple:
		// assume that processing an "assert enumerable(X)" stmt puts an "enumerable" on the plist with attrib as value.
		enumsym := t.plist.Find("enumerable")
		if enumsym != nil {
			return enumsym.(*Symbol).dtype.Elttype()
		}
		// That didn't work; try eachstart.
		mInx := t.methodIndex("eachstart")
		if mInx >= 0 {
			esmthd := t.methods[mInx]
			estyp := esmthd.sym.dtype.v.(*Ftntype).rettype // estyp is a type with eachstep, etc; value is what we want.
			vInx := estyp.methodIndex("value")
			if vInx >= 0 {
				return estyp.methods[vInx].sym.dtype.v.(*Ftntype).rettype
			}
			// no method so it must have an attribute
			return estyp.attributeLookup("value").dtype
		}
	case TFGomap:
		keytype := t.v.([]*Type)[0]
		valtype := t.v.([]*Type)[1]
		kvattribs := []*Symbol{makeSymbol("key", keytype, nil), makeSymbol("value", valtype, nil)}
		return makeType(TFTuple, kvattribs, false)
	}
	return nil
}

// Copy makes a new struct that can be extended without affecting self. Aka, a
// shallow copy. (No recursion, elttypes and the like are not copied.) For
// non-extendible types, it just returns self.
// todo: extend to handle recursive types, change method rcvr args to refnc the copy instead of self. (and asserts etc).
func (self *Type) Copy() Term {
	var retval *Type
	switch self.family {
	case TFNothing, TFBit, TFBool, TFLabel, TFLabellit, TFNil, TFTime, TFExit:
		return self
	case TFByte, TFInt, TFInt64, TFFloat, TFString, TFList, TFBaseQty, TFGTS, TFGochan, TFGomap:
		retval = makeType0(self.family, self.v, "")
	case TFSpace:
		newspt := *(self.v.(*Spacetype))
		retval = makeType0(TFSpace, &newspt, "")
	case TFFtn:
		oldft := self.v.(*Ftntype)
		nargs := make([]*Type, len(oldft.fmlargs))
		copy(nargs, oldft.fmlargs)
		newft := &Ftntype{nargs, oldft.rettype, oldft.pure}
		retval = makeType0(TFFtn, newft, "")
	case TFTuple:
		oldtt := self.v.(*Tupletype)
		natrbs := make([]*Symbol, len(oldtt.attribs))
		copy(natrbs, oldtt.attribs)
		newtt := &Tupletype{natrbs, oldtt.situated}
		retval = makeType0(TFTuple, newtt, "")
	case TFOrtype:
		oldort := self.v.(*Ortype)
		nvnts := make([]*Symbol, len(oldort.variants))
		copy(nvnts, oldort.variants)
		newot := &Ortype{nvnts, oldort.isNilposs}
		retval = makeType0(TFOrtype, newot, "")
	case TFQtyRatio:
		oldqrt := self.v.(*QtyRatioType)
		nn := make([]*Type, len(oldqrt.numer))
		nd := make([]*Type, len(oldqrt.denom))
		copy(nn, oldqrt.numer)
		copy(nd, oldqrt.denom)
		retval = makeType0(TFQtyRatio, &QtyRatioType{nn, nd}, "")
	}
	// at this point, retval is the copy but without methods, asserts, handlers, or plist
	retval.plist = *(self.plist.Copy())
	if self.methods != nil {
		// for a long time I thought a shallow copy sufficed. There was a comment to that effect.
		// Then I tried extend of an extend where the base extended type had methods. Shallow copy
		// doesn't work because its rcvr has the wrong type. The deep vs shallow thing below is a hack.
		// There are other times when extending causes this kind of copying and I still think that
		// shallow is good enough, hence the hack.
		m2 := make([]SymX, len(self.methods))
		retval.methods = m2
		if self.family == TFTuple {
			//fmt.Println("deep copying ", len(self.methods), "methods for type", self.String())
			for minx, mthd := range self.methods {
				mthdtyp := mthd.sym.dtype.v.(*Ftntype)
				nargs := make([]*Type, len(mthdtyp.fmlargs))
				copy(nargs, mthdtyp.fmlargs)
				nargs[0] = retval
				tsi := &typesubstInfo{self, retval, make([]*Type, 0, 2)}
				ntyp := tsi.typeSubstn(mthd.sym.dtype)
				nbdg := mthd.sym.binding.(*Funinst).copyFuninst()
				nsym := makeSymbol(mthd.sym.ident, ntyp, nbdg)
				m2[minx] = SymX{nsym, mthd.xcount}
			}
		} else {
			//fmt.Println("shallow copying ", len(self.methods), "methods for type", self.String())
			copy(m2, self.methods)
		}
	}
	retval.asserts = self.asserts.Copy()
	if self.handlers != nil {
		h2 := make([]Term, len(self.handlers))
		copy(h2, self.handlers)
		retval.handlers = h2
	}
	retval.SetString()
	return retval
}

// This checks names and signatures for equality.
func (t *Type) equalMethods(other *Type, seen *[]*Type) bool {
	nmthds := len(t.methods)
	if nmthds != len(other.methods) {
		return false
	}
	for i := 0; i < nmthds; i++ {
		tm := t.methods[i].sym
		otm := other.methods[i].sym
		if tm.ident != otm.ident {
			return false
		}
		tmt := tm.dtype.v.(*Ftntype)
		otmt := otm.dtype.v.(*Ftntype)
		if len(tmt.fmlargs) != len(otmt.fmlargs) || !tmt.rettype.equal(otmt.rettype, seen) {
			return false
		}
		for j := 0; j < len(tmt.fmlargs); j++ {
			if !tmt.fmlargs[j].equal(otmt.fmlargs[j], seen) {
				return false
			}
		}
	}
	return true
}

// Test whether t and other are extensionally equal, including method names and signatures.
// Try to use some speedup tricks. The seen arg is to prevent infloops when traversing circular structures. It
// gets [t, other] pairs when we get to a point where a recursive call is likely.
func (t *Type) equal(other *Type, seen *[]*Type) bool {
	if t == other {
		return true
	}
	if other.family == TFGTS && other.v.(*GTS).binding != nil {
		other = other.v.(*GTS).binding
	}
	if t.family != other.family {
		return false
	}
	for i := 0; seen != nil && i < len(*seen); i += 2 {
		if t == (*seen)[i] {
			return other == (*seen)[i+1]
		}
	}
	*seen = append(*seen, t, other)
	if t.v == nil && other.v == nil { // this handles basic types
		return t.equalMethods(other, &[]*Type{t, other})
	}
	retval := true
	switch dtl := t.v.(type) {
	case *GTS:
		osym := other.v.(*GTS)
		return dtl == osym
	case *Typred: // TFTypred
		// I'm not sure this can happen
		otp := other.v.(*Typred)
		retval = dtl.equal(otp)
	case *Type: // list, Gochannel, TFPtr
		olt := other.v.(*Type)
		retval = dtl.equal(olt, seen)
	case *Spacetype:
		ost := other.v.(*Spacetype)
		retval = dtl.dimensions.Equal(ost.dimensions) && dtl.elttype.equal(ost.elttype, seen)
	case *Ftntype:
		oft := other.v.(*Ftntype)
		if dtl.pure != oft.pure || len(dtl.fmlargs) != len(oft.fmlargs) ||
			!dtl.rettype.equal(oft.rettype, seen) {
			return false
		}
		for i, v := range dtl.fmlargs {
			if v.family != oft.fmlargs[i].family && oft.fmlargs[i].family != TFGTS {
				return false
			}
		}
		// all corresponding families are same, which was fast to check. So do the real check.
		for i, v := range dtl.fmlargs {
			if !v.equal(oft.fmlargs[i], seen) {
				return false
			}
		}
	case *Tupletype:
		ott := other.v.(*Tupletype)
		if dtl.situated != ott.situated || len(dtl.attribs) != len(ott.attribs) {
			return false
		}
		for i, v := range dtl.attribs {
			if v == nil || ott.attribs[i] == nil || v.ident != ott.attribs[i].ident || v.dtype.family != ott.attribs[i].dtype.family {
				return false
			}
		}
		// again, a fast check indicates all corresponding attribs are similar to do real check.
		for i, v := range dtl.attribs {
			if !v.dtype.equal(ott.attribs[i].dtype, seen) {
				return false
			}
		}
	case *Ortype:
		// note that ortype elts are sorted on idents to give set semantics
		// also note that family TFOrtype corresponds to 2 possible types in v slot.
		// Finally, this code assumes the caller has checked the "set" condition for
		// the types, ie. that there are no dups among them.
		ovnts := other.v.(*Ortype)
		if len(ovnts.variants) != len(dtl.variants) || ovnts.isNilposs != dtl.isNilposs {
			return false
		}
		for i, v := range dtl.variants {
			if v.ident != ovnts.variants[i].ident {
				return false
			}
		}
		for i, v := range dtl.variants {
			if !v.dtype.equal(ovnts.variants[i].dtype, seen) {
				return false
			}
		}
	case []string: // ELT
		oelt := other.v.([]string)
		if len(dtl) != len(oelt) {
			return false
		}
		for i, v := range dtl {
			if v != oelt[i] {
				return false
			}
		}
	case string: // this encodes TFBaseQty
		return dtl == other.v.(string)
	case *QtyRatioType:
		qrt := other.v.(*QtyRatioType)
		if len(dtl.numer) != len(qrt.numer) || len(dtl.denom) != len(qrt.denom) {
			return false
		}
		for i, v := range dtl.numer {
			if !v.equal(qrt.numer[i], seen) {
				return false
			}
		}
		for i, v := range dtl.denom {
			if !v.equal(qrt.denom[i], seen) {
				return false
			}
		}
	}
	if retval {
		retval = t.equalMethods(other, &[]*Type{t, other})
	}
	return retval
}

// Helper answers whether ty is a genzd ftn
func anyGenzdType(ty *Type) bool {
	if ty.family == TFTuple {
		tt := ty.v.(*Tupletype)
		// check for the weird encoding of genzd types
		return len(tt.attribs) == 1 && tt.attribs[0].dtype == TypeNothing
	}
	return false
}

// Linear search for a string returning index, -1 if no exact match
func stringInStrings(s0 string, ls []string) int {
	for i, s := range ls {
		if s0 == s {
			return i
		}
	}
	return -1
}

// Test whether t "is compatible with" other. If both are concrete, this means structurally equal
// and t's methods are a superset of other's. If other has generic symbols, they will be treated as bindable.
// Usually in TC, other will be the constraint and t will be the concrete type being tested against it.
// (Except if we're looking at rettype in funcall matching.)
// Accordingly, return true if other is nil.
// The chosen algo is designed to ensure that "normal" cases go fast.
//
// Quite late in the game I'm adding the behavior of ignoring ptrs which can be left around from prev queries.
func (t *Type) compat(other *Type, seen *[]*Type) bool {
	if other == nil || t == other || other.family == TFGoanything { // shortcut some easy cases
		return true
	}
	if t.family == TFPtr {
		t = t.v.(*Type)
	}
	if other.family == TFPtr {
		other = other.v.(*Type)
	}
	for i := 0; seen != nil && i < len(*seen); i += 2 {
		if t == (*seen)[i] {
			return other == (*seen)[i+1]
		}
	}
	// seen is about recursion; GTS is about matching. In fact, it's incorrect to add pairs with GTS.
	if other.family != TFGTS {
		*seen = append(*seen, t, other)
	}
	// this GTS check+bind local ftn facilitates symmetric treatment of GTS's in compat.
	// it will bind the first arg to the second
	var checkAgainstGTS = func(gts, otherarg *Type, gtsfirst bool) bool {
		dtl := gts.v.(*GTS) // get the gnc symbol, its type will be a Typred
		if dtl.binding != nil {
			boundtype := dtl.binding
			seen2 := []*Type{}
			return otherarg.compat(boundtype, &seen2)
		} else {
			typred := dtl.dtype.v.(*Typred)
			if otherarg.family == TFGTS && otherarg.v.(*GTS).binding != nil {
				otherarg = otherarg.v.(*GTS).binding
			}
			if gts == otherarg {
				return true
			}
			if otherarg.family == TFTypred && typred.compat(otherarg.v.(*Typred)) {
				return true
			}
			// do not succeed or bind for circular cases
			for _, gt := range otherarg.generics() {
				if gt == gts {
					return false
				}
			}
			if gtsfirst {
				// if comparing a typred-constrained type (as actual) with any other type, create a "hole" in
				// correct typechecking to get around the fact that I currently don't close basepreds over typred
				// args to arbitrary typegens. The effect of the following is to check only the root generator of the typepred,
				// and not the cntts that might be placed on its type-valued args.
				// See notes at 05/10/21 for details.
				ret := (typred.familyset & (1 << otherarg.family)) != 0
				if ret {
					dtl.binding = otherarg
				}
				return ret
			}
			if typred.match(otherarg) {
				dtl.binding = otherarg
				return true
			}
		}
		return false
	}
	if other.family == TFGTS {
		return checkAgainstGTS(other, t, false)
	}
	// here, we know other isn't a GTS, in which case we might want to bind the actual
	if t.family == TFGTS {
		return checkAgainstGTS(t, other, true)
	}
	// Xprterm dominates everything except nothing
	if other == Gzterm || other == Gzsym {
		return t != TypeNothing
	}
	// And on an experimental basis, we let xprterm be compat with anything
	if t == Gzterm {
		return true
	}
	// A bit special-casey but list typred matches list(T) for any T::entity
	if t == TPList && other.family == TFList {
		return other.v.(*Type).family == TFGTS && other.v.(*Type).v.(*GTS).dtype == TPEntity
	}
	// ELT-valued exprs type as int with an ELT plist item that's accessed by the memberELT predicate.
	// Therefore, in this special case, an "integer" should be compat with a label in either direction.
	// However, there is a fairly deep problem with the following code. The code that checks for ELT-ness
	// could check either that the type is an ELT (which it does now) or that it's an ELT and the label is
	// an elt of the ELT. But the latter isn't manifest in general, so it doesn't really work. OTOH, the
	// former (which is the current behavior) isn't compatible with mapping labels to small integers in cases
	// where the ELT membership isn't manifest. For example, given a funarg of type label, comparing it with
	// an ELT-valued thing is not manifest.
	// todo: fix code to not map ELTs to TFInt when they engage in non-manifest operations
	var ELTandLabel = func(first, second *Type) bool {
		return first.family == TFInt && first.plist.Find("ELT") != nil && second.plist.Find("labellit") != nil
	}
	var ELTLabelCompat = func(first, second *Type) (found bool) {
		eltvalues := first.plist.Find("ELT").([]string)
		otherval := second.plist.Find("labellit").(string)
		for _, v := range eltvalues {
			if v == otherval {
				return true
			}
		}
		return
	}
	if ELTandLabel(t, other) {
		return ELTLabelCompat(t, other)
	} else if ELTandLabel(other, t) {
		return ELTLabelCompat(other, t)
	}

	switch other.family {
	case TFLabel:
		if t.family == TFLabel || t.family == TFLabellit {
			return true
		}
	case TFLabellit:
		return false // labellit should not occur as a cntt
	case TFTypred:
		// this is for special typreds like number and enumerable
		typred := other.v.(*Typred)
		if typred.ident == "number" {
			return t.family == TFInt || t.family == TFFloat
		} else if typred.ident == "enumerable" {
			if t.family == TFList || t.family == TFSpace || t.family == TFGomap {
				return true
			}
			if t.family != TFTuple {
				return false
			}
			if t.plist.Find("enumerable") != nil {
				return true
			}
			if t.methodIndex("eachstart") >= 0 {
				// todo: check the eachstart method has the right type
				return true
			}
		} else if typred.ident == "function" {
			return t.family == TFFtn
		} else if typred.ident == "tuple" {
			return t.family == TFTuple
		} else if typred.ident == "list" {
			return t.family == TFList
		} else if typred.ident == "entity" {
			return true
		} else {
			return typred.match(t)
		}
	case TFNil:
		if t.isNilposs() {
			return true
		}
	case TFOrtype:
		if other.isNilposs() {
			ortm := other.mainType()
			return t == TypeNil || t.compat(ortm, seen) || (t.isNilposs() && t.mainType().compat(ortm, seen))
		}
		// other isn't nilposs so t is compat if (1) it's an ortype subset of other or (2) compat with one of of ort.variants
		ort := other.v.(*Ortype)
		if t.family == TFOrtype {
			// for subset, tags count so we rely on their ordering
			tort := t.v.(*Ortype)
			tinx := 0
			tv := tort.variants[0]
			for _, ots := range ort.variants {
				if tv.ident == ots.ident && tv.dtype.compat(ots.dtype, seen) {
					tinx++
					if tinx == len(tort.variants) {
						break
					}
					tv = tort.variants[tinx]
				}
			}
			return tinx == len(tort.variants)
		} else {
			for _, v := range ort.variants {
				seen2 := []*Type{}
				if t.compat(v.dtype, &seen2) {
					return true
				}
			}
			return false
		}
	}
	if t.family == TFGomap && other.family == TFList {
		// code the fact that gomap matches list(key-value tuple)
		eltyp := other.v.(*Type)
		if eltyp.family == TFTuple {
			atrbs := eltyp.v.(*Tupletype).attribs
			if len(atrbs) == 2 && atrbs[0].ident == "key" && atrbs[1].ident == "value" {
				gmtypes := t.v.([]*Type)
				return gmtypes[0].compat(atrbs[0].dtype, seen) && gmtypes[1].compat(atrbs[1].dtype, seen)
			}
		}
	}
	if t.family != other.family {
		return false
	}
	// At this point, other is not a pattern symbol, so we match parts based on family
	switch t.family {
	case TFList:
		if !t.v.(*Type).compat(other.v.(*Type), seen) {
			return false
		}
	case TFSpace:
		tspc := t.v.(*Spacetype)
		ospc := other.v.(*Spacetype)
		if !tspc.elttype.compat(ospc.elttype, seen) {
			return false
		}
		if !compat(tspc.dimensions, ospc.dimensions, seen) {
			return false
		}
	case TFTuple:
		ttty := t.v.(*Tupletype)
		otty := other.v.(*Tupletype)
		if len(ttty.attribs) != len(otty.attribs) || ttty.situated != otty.situated {
			return false
		}
		for i := 0; i < len(ttty.attribs); i++ {
			if !(ttty.attribs[i].ident == otty.attribs[i].ident && ttty.attribs[i].dtype.compat(otty.attribs[i].dtype, seen)) {
				return false
			}
		}
	case TFFtn:
		tfty := t.v.(*Ftntype)
		ofty := other.v.(*Ftntype)
		if len(tfty.fmlargs) != len(ofty.fmlargs) || !tfty.rettype.compat(ofty.rettype, seen) {
			return false
		}
		for i := 0; i < len(tfty.fmlargs); i++ {
			if !tfty.fmlargs[i].compat(ofty.fmlargs[i], seen) {
				return false
			}
		}
	}

	// Here to check methods. The original language design used standard subtyping ideas but I've changed it
	// to allow structurally identical types to be compat if they extend each other in either direction.
	// (Whereas std subtyping only allows compatibility in one direction -- the extended type is compatible
	// with its basetype but not vice versa.) The reasons are discussed elsewhere; the implications for
	// coding are that structurally identical types are incompat only if each has a method that the other doesn't.
	// There's an easy way to detect this if the methods are totally ordered, which is almost the case here.
	// The exception is that multibound methods (ie same identifier, different types) are ordered only on the
	// identifer. At the moment, I ignore this inconvenient fact, partly because in practice it is fairly difficult
	// to get it to break things. But rest assured, it's not correct!
	// Additionally, there are shortcuts. Fr example, same generator means same methods, so no check needed.
	if t.generator != nil && t.generator == other.generator {
		return true
	}
	missing0, missing1 := 0, 0
	if len(t.methods) > 0 && len(other.methods) > 0 {
		i, j := 1, 1
		tmthd := t.methods[0].sym
		omthd := other.methods[0].sym
		for {
			if i == len(t.methods) {
				if j < len(other.methods) {
					missing1 += 1
				}
				break
			}
			if j == len(other.methods) {
				if i < len(t.methods) {
					missing0 += 1
				}
				break
			}
			if tmthd.ident == omthd.ident {
				if tmthd.dtype.compat(omthd.dtype, seen) {
					tmthd = t.methods[i].sym
					omthd = other.methods[j].sym
					i += 1
					j += 1
				} else {
					return false // incr both missing0 and missing1 which ensures a fail
				}
			} else if tmthd.ident < omthd.ident {
				missing1 += 1
				if missing0 > 0 {
					return false
				}
				tmthd = t.methods[i].sym
				i += 1
			} else {
				missing0 += 1
				if missing1 > 0 {
					return false
				}
				omthd = other.methods[j].sym
				j += 1
			}
		}
	}
	return missing0 == 0 || missing1 == 0
}

// Occasionally, we need compat for Terms.
func compat(trm, otrm Term, seen *[]*Type) bool {
	if otrm.Tag() == SymbolTag {
		sym := otrm.(*Symbol)
		if sym.dtype == nil || !trm.Dtype().compat(sym.dtype, seen) {
			return false
		}
		if sym.binding == nil {
			sym.binding = trm
		} else if !sym.binding.Equal(trm) {
			return false
		}
		return true
	}
	return false
}

// Called only on types generated by a Typegen. Linear search for the value of the generator's insts slot for the type,
// which gives caller the args used to generate it.
func (typ *Type) instanceInfo() []Term {
	for _, ii := range typ.generator.insts {
		if ii[len(ii)-1] == typ {
			return ii
		}
	}
	return nil
}

// Construct and return a type that's the std nilposs extension of maintyp
func makeNilposs(maintyp *Type) *Type {
	t := makeType0(TFOrtype, &Ortype{Bplst{makeSymbol("main", maintyp, nil)}, true}, "")
	t.SetString()
	return t
}

// Predicate testing if typ is a nilposs
func (typ *Type) isNilposs() bool {
	ort, isort := typ.v.(*Ortype)
	return isort && ort.isNilposs
}

// Predicate testing if typ is a mod ftn
func (typ *Type) isMod() bool {
	ft, isfn := typ.v.(*Ftntype)
	return isfn && !ft.pure
}

// Is typ one of the predefined range types?
func (typ *Type) rangeType() bool {
	return typ == biScope["rangeI"][0].binding || typ == biScope["rangeF"][0].binding
}

// Should only be called with a nilposs type; return its main type
func (typ *Type) mainType() *Type {
	ort := typ.v.(*Ortype)
	if ort.isNilposs {
		return ort.variants[0].dtype
	}
	panic("mainType")
}

func makeListType(elttyp *Type) *Type {
	return listTG.Instantiate([]Term{elttyp})
}

func makeSpaceType(elttyp *Type, dim Term) *Type {
	return spaceTG.Instantiate([]Term{elttyp, dim})
}

// Given a type family and some (family-dependent) args create the type.
// This function only creates "structural" types; other methods
// add methods and so on. These need to be careful about basic types that can
// be extended, which are families [int, float, string].
func makeType(fam TypeFamily, rest ...interface{}) (retval *Type) {
	switch fam {
	case TFNothing:
		retval = TypeNothing
	case TFUnknown:
		retval = makeType0(TFUnknown, nil, "unknown")
	case TFByte:
		retval = TypeByte
	case TFBit:
		retval = TypeBit
	case TFBool:
		retval = TypeBool
	case TFInt:
		retval = TypeInt
	case TFInt64:
		retval = TypeInt64
	case TFFloat:
		retval = TypeFloat
	case TFLabel:
		retval = TypeLabel
	case TFLabellit:
		retval = makeType0(TFLabellit, rest[0], "")
	case TFString:
		retval = TypeString
	case TFNil:
		retval = TypeNil
	case TFTime:
		retval = TypeTime
	case TFList:
		elttype := rest[0].(*Type)
		retval = makeType0(TFList, elttype, "")
	case TFSpace:
		dim0 := rest[1].(Term)
		elttyp := rest[0].(*Type)
		retval = makeType0(TFSpace, &Spacetype{elttyp, dim0}, "")
	case TFFtn:
		args := rest[0].([]*Type)
		rett := rest[1].(*Type)
		isPure := rest[2].(bool)
		retval = makeType0(TFFtn, &Ftntype{args, rett, isPure}, "")
	case TFTuple:
		bplist := rest[0].([]*Symbol)
		situated := rest[1].(bool)
		retval = makeType0(TFTuple, &Tupletype{bplist, situated}, "")
	case TFOrtype:
		vntlist := rest[0].(Bplst)
		isnp := false
		if len(vntlist) == 2 {
			inx := -1 // set inx to the index of the nil variant, if present
			seen := []*Type{}
			if vntlist[0].dtype.equal(TypeNil, &seen) {
				inx = 0
			}
			if vntlist[1].dtype.equal(TypeNil, &seen) {
				inx = 1
			}
			if inx >= 0 {
				// make a nilposs by deleting the nil slot
				vntlist = Bplst{vntlist[1-inx]}
				isnp = true
			}
		}
		retval = makeType0(TFOrtype, &Ortype{vntlist, isnp}, "")
	case TFBaseQty: // arg should be a string
		retval = makeType0(TFBaseQty, rest[0], "")
	case TFQtyRatio:
		// in theory, I should look for generics here, but I don't think it's really needed.
		numert := rest[0].([]*Type)
		denomt := rest[1].([]*Type)
		retval = makeType0(TFQtyRatio, &QtyRatioType{numert, denomt}, "")
	case TFGTS: // arg is a *Symbol bound to a GTS
		gts := rest[0].(*GTS)
		retval = makeType0(TFGTS, gts, "")
	case TFTypred: // arg is a *Typred
		typred := rest[0].(*Typred)
		retval = makeType0(TFTypred, typred, "")
	case TFGochan:
		elttype := rest[0].(*Type)
		dir := rest[1].(string)
		retval = makeType0(TFGochan, &ChanInfo{elttype, dir}, "")
	case TFGomap:
		keytype := rest[0].(*Type)
		valtype := rest[1].(*Type)
		retval = makeType0(TFGomap, []*Type{keytype, valtype}, "")
	}
	if retval == nil {
		panic("missing case in makeType")
	}
	retval.SetString()
	return retval
}

// TraverseTypePre traverses a type in pre-order using ideas similar to TraversePre, ie a state var that presents
// as interface{} and a workfn that returns an int interpreted as continue (0), bail (>0), or don't visit components (<0).
// Note that the workfn *must* handle state part of recursion prevention, returning -1 on detection.
// The return value can be meaningless if the intention is to traverse the entire type; but if something is
// being sought, a > 0 value from the workfn causes true to propagate as a return value without further traversal.
func (typ *Type) TraverseTypePre(workfn func(typ *Type, sv interface{}) int, state interface{}) bool {
	found := workfn(typ, state)
	if found > 0 {
		return true
	}
	if found < 0 {
		return false
	}
	switch typ.family {
	case TFList:
		if typ.v.(*Type).TraverseTypePre(workfn, state) {
			return true
		}
	case TFSpace:
		if typ.v.(*Spacetype).elttype.TraverseTypePre(workfn, state) {
			return true
		}
	case TFTuple:
		for _, a := range typ.v.(*Tupletype).attribs {
			tmp := a.dtype.TraverseTypePre(workfn, state)
			if tmp {
				return true
			}
		}
	case TFFtn:
		for _, a := range typ.v.(*Ftntype).fmlargs {
			tmp := a.TraverseTypePre(workfn, state)
			if tmp {
				return true
			}
		}
		if typ.v.(*Ftntype).rettype.TraverseTypePre(workfn, state) {
			return true
		}
	}
	return false
}
