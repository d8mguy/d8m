// code related to import and import pkg, including file loading functionality, the packages database, etc.

package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"
)

var SrcPath = "/Library/Application Support/D8m/modules/"
var importPath = ""
var UserPath = ""

// The next set of definitions provide the functionality around module prefixes. Read from modulesDB.json in installation
// area (SrcPath). This should be a list(tuple(Path:string, Loc:string)). Note that the attribute names must be capitalized,
// else the go json decoder will ignore them as not exported.
//
// Take the list so obtained, split the paths into list(string) of slash-separated components, and turn the list into a tree
// that can be used to search for locations to read from module import paths in tcImport.
//
// Note that there has to be a way to edit (and hopefully view) modulesDB. For now, it's a manually edited json file, but
// in future there should be more robust support, including checks that its locations point to a place where there are modules,
// and that there are no duplicated modules among locations. (Note that the spec explicitly allows a single prefix to be
// mapped to multiple locations and these must all be searched. But there are not supposed to be duplications, which means
// that search order doesn't (shouldn't) matter.)
//

// This is the form decoded from json. (I might add more fields later on.)
type mdbItemRaw struct {
	Path string
	Loc  string
}

// We're going to sort the list on Path.
type mdbrSlice []mdbItemRaw

func (mdbr mdbrSlice) Len() int {
	return len(mdbr)
}

// This is where sorting on Path is defined.
func (mdbr mdbrSlice) Less(i, j int) bool {
	i0 := mdbr[i]
	i1 := mdbr[j]
	return i0.Path < i1.Path
}
func (mdbr mdbrSlice) Swap(i, j int) {
	mdbr[i], mdbr[j] = mdbr[j], mdbr[i]
}

// This type is obtained from the sorted raw list by splitting the Path on slashes.
type mdbItemCooked struct {
	path []string
	loc  string
}

// Tree nodes are path components.
type mdbNode struct {
	component string
	// Normally, either children or loc will be empty, but interior nodes may have mappings so both can be non-empty.
	children []*mdbNode
	loc      []string
}
type mdbTree = struct {
	root []*mdbNode
}

// The modulesTree is obtained from the json list; this is what tcImport uses. Note that its ordering inherits from the sort process.
var modulesTree mdbTree

// Given a list and a level, create a forest of subtrees.
func getMdbChildren(items []mdbItemCooked, level int) []*mdbNode {
	// fmt.Println("getMdbChildren", len(items), level)
	rslt := make([]*mdbNode, 0, 2)
	for i := 0; i < len(items); {
		item := items[i]
		if len(item.path) <= level {
			i++
			continue
		}
		pfx := item.path[level]
		used := 0
		for j := i + 1; ; j++ {
			used++
			if j == len(items) || items[j].path[level] != pfx {
				break
			}
		}
		subslice := items[i : i+used]
		rslt = append(rslt, getMdbNode(subslice, level))
		i += used
	}
	return rslt
}

// Given a list and a level, where the list contains a single tree, return that tree. "Single tree" means
// that items is a slice all of whose members start with the same component.
func getMdbNode(items []mdbItemCooked, level int) *mdbNode {
	//fmt.Println("getMdbNode", len(items), items[0].path[0], level)
	if len(items) == 1 {
		item := items[0]
		if len(item.path)-level == 1 {
			//fmt.Println("returns leaf", item.path[level])
			return &mdbNode{item.path[level], nil, []string{item.loc}}
		} else {
			child := getMdbNode(items, level+1)
			//fmt.Println("returns singleton branch", item.path[level])
			return &mdbNode{item.path[level], []*mdbNode{child}, []string{}}
		}
	} else {
		children := getMdbChildren(items, level+1)
		//fmt.Println("returns interior node", items[0].path[level])
		loc := []string{}
		if len(items) > 1 && len(items[0].path) == level+1 && len(items[1].path) > level+1 {
			loc = append(loc, items[0].loc)
		}
		return &mdbNode{items[0].path[level], children, loc}
	}
}

// Given a list of raw items, sort it, split and create the cooked list, then use getMdbChildren and getMdbNode to set modulesTree.
func makeMdbTree(items mdbrSlice) {
	sort.Sort(items)
	cooked := make([]mdbItemCooked, len(items))
	for i, item := range items {
		location := item.Loc
		if len(location) > 2 && location[:2] == "~/" {
			location = UserPath + location[1:]
		}
		splits := strings.Split(item.Path, "/")
		for _, c := range splits {
			if c == "" {
				panic("bad format in path: " + item.Path)
			}
		}
		cooked[i] = mdbItemCooked{splits, location}
		//fmt.Println(i, cooked[i])
	}
	modulesTree = mdbTree{getMdbChildren(cooked, 0)}
}

/*
// Couple of print codes for debugging.
func printMdpNode(nd *mdbNode) {
	if nd.loc != [] {
		fmt.Printf("%s -> %s\n", nd.component, nd.loc)
	} else {
		fmt.Println(nd.component, "{")
		for _, child := range nd.children {
			printMdpNode(child)
		}
		fmt.Println("}")
	}
}
func printMdbtree(tr mdbTree) {
	for _, child := range tr.root {
		printMdpNode(child)
	}
}
*/

// This is the function that interprets what the modulesDB directs. Given a path, split it into components,
// search in the modulesTree, and use that to return a full path for opening the file. Also handle the builtin
// interpretation of modules/xxx as meaning a module in the local modules dir.
func modulesFullpath(path string) string {
	compnts := strings.Split(path, "/")
	if len(compnts) > 1 && compnts[0] == "modules" {
		return path
	}
	maxdepth := len(compnts) - 1
	curnode := &mdbNode{children: modulesTree.root}
	found := false
	i := 0
	for ; i < maxdepth; i++ {
		curcompnt := compnts[i]
		for _, tn := range curnode.children {
			if tn.component == curcompnt {
				curnode = tn
				found = true
				break
			}
		}
		if !found {
			if i == 0 || len(curnode.loc) == 1 {
				break
			}
			panic("multiple mappings not implemented")
		}
	}
	if found {
		remaining := strings.Join(compnts[i:], "/")
		return curnode.loc[0] + remaining
	}
	return SrcPath + path // no mapping, so it's official
}

// SrcPath must be fully set up before calling this
func readModuleDB() []mdbItemRaw {
	moddbPath := SrcPath + "modulesDB.json"
	fd, err := os.Open(moddbPath)
	if err != nil {
		panic("on open moduleDB.json:" + err.Error())
	}
	finfo, ferr := fd.Stat()
	if ferr != nil {
		panic("can't stat moduleDB.json")
	}
	mdbBytes := make([]byte, finfo.Size())
	nrd, rerr := fd.Read(mdbBytes)
	if rerr != nil || nrd < int(finfo.Size()) {
		panic("can't read moduleDB.json")
	}
	fd.Close()
	var rawItems []mdbItemRaw
	//fmt.Println("from", moddbPath, "read", string(mdbBytes))
	if json.Unmarshal(mdbBytes, &rawItems) != nil {
		panic("json read error")
	}
	//fmt.Println("raw items: ", len(rawItems), rawItems[0].Path, rawItems[0].Loc, rawItems)
	return rawItems
}

func addModuleDB(path, loc string) {
	existing := readModuleDB()
	inx := -1
	for i, item := range existing {
		if item.Path == path {
			inx = i
			break
		}
	}
	if inx >= 0 {
		existing[inx].Loc = loc
	} else {
		existing = append(existing, mdbItemRaw{path, loc})
	}
	coded, jerr := json.MarshalIndent(existing, "", "  ")
	if jerr != nil {
		log.Fatal(jerr)
	}
	moddbPath := SrcPath + "modulesDB.json"
	fd, err := os.Create(moddbPath)
	if err != nil {
		log.Fatal(err)
	}
	//fmt.Println("created", moddbPath, "writing", string(coded))
	n, werr := fd.Write(coded)
	if n != len(coded) || werr != nil {
		log.Fatal("failed to write", werr)
	}
	fd.Close()
}

// QIScope holds the internal identifiers (aka "qualifiers") of imported modules and packages that are currently
// in scope. Because modules are scoped, the "obvious" way to manage qualifiers is with a parallel stack of scopes
// but because I expect scoped usage of qualifiers to be rare, I prefer something that favors the expected case where
// modules are global.
type QIVal struct {
	scope       *Scope
	isModule    bool
	activeCount int
	path        string
	mname       string // this reproduces what's in the key of QIScope, but it helps with path checking
}

var QIScope = make(map[string]*QIVal, 10)

// The QIScope just defined is the way to find a module/pkg in TC but it's not helpful for enforcing the rule
// about re-import of the same module/pkg. In a nutshell the rule is: if the identifier is the same it's ignored,
// otherwise, it's an error. To enforce this rule, we manage a map with path as key. Since each path can correspond
// to 1 module and 1 package, the value is a tuple type with a slot for each.
type mdlOrPkg struct {
	mdl *QIVal
	pkg *QIVal
}

var QIPathMap = make(map[string]*mdlOrPkg, 10)

// The import statement doesn't really need typechecking; the parser ensures that the form is ok.
// We do need to check that the path exists, the module name isn't taken, and so on. Then we load
// the file right here, and melt it if needed.
func tcImport(path, mname string, tci *TCInfo, t *TermTT) Term {
	// first, is mname going to work?
	if mname != "" {
		qi := QIScope[mname]
		if qi != nil && !(path == qi.path && mname == qi.mname && qi.isModule) {
			// whoops, there's a conflict
			return tci.Error(fmt.Sprintf("qualifier %s is already in use for %s", mname, qi.path), t)
		}
	}
	//fmt.Print("import ", path, mname, "...")
	// second sanity check is the pathmap
	inPath := QIPathMap[path]
	if inPath != nil && inPath.mdl != nil {
		// is there an active entry with the same QI? if yes, this is a redundant import which is ok; we're done
		if inPath.mdl.mname == mname && inPath.mdl.activeCount > 0 {
			return t
		}
		// if this path is present but inactive, activate it
		if inPath.mdl.activeCount == 0 {
			//fmt.Println("reactivate")
			inPath.mdl.activeCount = 1
			if mname == "" { // melted?
				for _, sx := range inPath.mdl.scope.entries {
					symx, added := tci.scopes.AddIfPossible(sx.sym)
					if added != "" {
						return tci.Error(added, t)
					}
					sx.sym = symx
				}
			} else {
				QIScope[mname] = inPath.mdl
				inPath.mdl.mname = mname
			}
			tci.scopes.stitems = append(tci.scopes.stitems, inPath.mdl.scope.stitems...)
			tci.scopes.stActives = append(tci.scopes.stActives, inPath.mdl.scope.stActives...)
			tci.scopes.rwActives = append(tci.scopes.rwActives, inPath.mdl.scope.rwActives...)
			tci.scopes.importMdls = append(tci.scopes.importMdls, path)
			return t
		}
		// otherwise, it's an error
		errmsg := path + " was already imported "
		if inPath.mdl.mname == "" {
			errmsg += "melted"
		} else {
			errmsg += "with QI " + inPath.mdl.mname
		}
		return tci.Error(errmsg, t)
	}
	//fmt.Println("loading file")
	importScope := makeScope()
	importScope.topscope = true
	tci.PushScope(importScope)
	tci.cxt.Push(ImportStmt)
	svscan := tci.scanner
	defer func() {
		tci.cxt.Pop()
		tci.scanner = svscan
	}()
	loadFile(path, true, tci)
	tci.PopScope()
	if len(tci.errors) > 0 {
		return tci.errors[len(tci.errors)-1]
	} else {
		xpstmt := tci.cxt.FindItem("export")
		if xpstmt == nil {
			return tci.Error(fmt.Sprintf("missing export statement in file %s", path), t)
		}
		exportlist := xpstmt.(*TermL).args
		melted := mname == ""
		indices := make([]int, 0, len(exportlist))
		scopex := importScope.entries
		qiv := &QIVal{importScope, true, 1, path, mname}
		// the following allows these symbols to be externed in the backend if necessary
		for _, sx := range scopex {
			sx.sym.plist.Add("qisym", qiv)
		}
		for _, xported := range exportlist {
			var ident string
			var typ *Type
			var scpinx int
			switch xp0 := xported.(type) {
			case *Token:
				ident = xp0.val
			case *TermL: // Symdecl, just ident and type
				ident = xp0.args[0].(*Token).val
				typ0 := TC4Type(xp0.args[1], nil, tci)
				if typ0.Tag() == ErrorTag {
					return typ0
				}
				typ = typ0.(*Type)
			default:
				panic("can't happen")
			}
			scpinx = scopex.Index(ident)
			if !(scpinx < len(scopex) && scopex[scpinx].sym.ident == ident) {
				return tci.Error(fmt.Sprintf("%s not found", ident), t)
			}
			// in the following loop, export 1 or all of the symbols with same ident
			found := false
			for ; scpinx < len(scopex) && scopex[scpinx].sym.ident == ident; scpinx++ {
				if typ != nil {
					seen := []*Type{}
					if scopex[scpinx].sym.dtype.compat(typ, &seen) {
						found = true
					}
				}
				if typ == nil || found {
					indices = append(indices, scpinx)
					if found {
						break
					}
				}
			}
			if typ != nil && !found {
				return tci.Error(fmt.Sprintf("can't find symbol %s matching %s", ident, typ.String()), t)
			}
		}
		sort.Ints(indices) // now it'll be in ascending order
		j := len(indices) - 1
		top := len(scopex) - 1
		for i := top; i >= 0; i-- {
			if j >= 0 && indices[j] == i {
				j -= 1
			} else {
				copy(scopex[i:top], scopex[i+1:top+1])
				top -= 1
				scopex = scopex[0 : top+1]
			}
		}
		importScope.entries = scopex
		QIScope[mname] = qiv
		for _, sx := range importScope.entries {
			if melted {
				symx, added := tci.scopes.AddIfPossible(sx.sym)
				if added != "" {
					return tci.Error(added, t)
				}
				sx.sym = symx
			}
		}
		tci.scopes.importMdls = append(tci.scopes.importMdls, path)
		// at this point, symbols are done but not yet  stitems. Move those up whether melted or not.
		// The proper language semantics is to export stitems only if both types are accessible. Otherwise, they're
		// not useful and could (in obscure cases) get in the way. For now, I'm exporting everything.
		// todo: fix this
		tci.scopes.stitems = append(tci.scopes.stitems, importScope.stitems...)
		tci.scopes.stActives = append(tci.scopes.stActives, importScope.stActives...)
		tci.scopes.rwActives = append(tci.scopes.rwActives, importScope.rwActives...)
		if QIPathMap[path] == nil {
			QIPathMap[path] = &mdlOrPkg{QIScope[mname], nil}
		} else {
			QIPathMap[path].mdl = QIScope[mname]
		}
	}
	return t
}

// For now, the go package database is here. In a fully mature implementation, there has to be a tool that can
// read a compiled go package, or perhaps some kind of descrn of it, and create an entry in this database, which
// at that point would no longer be something compiled into the source code. So this approach is a first step.
// The idea is to fully vet the design approach so there's full confidence in what needs to be in this database.
//
// Another caveat about this database: it's a "frontend" one, The essential point is that the types are all d8m
// types. That means no pointers, no direct access to interface{}, or to multiple return, or to int64, or to unsigned,
// etc etc. The idea is that we describe everything in terms of d8m types augmented with selected go type standins
// (like Gochannel). There will be a backend database that annotates the d8m types of various things so the generated
// code reflects what the go package wants, e.g. annotates a d8m integer to be a go uint or int64, etc.
//
// The following table is adapted from the corresponding thing in the ruby version of the compiler. Each main entry
// describes one package with the source path to access it, the types it exports, the entities it exports.
//
// The pkgdb is sorted on pkgpath so things can be looked up with binsearch.

type pkgdbEntry struct {
	pkgpath     string
	pkgtypes    []*Symbol
	pkgentities []*Symbol
}

// This does the "internals" of adding a pkg to the enviro, everything except scope related changes
func (pkgdesc *pkgdbEntry) AddPkg(path, mname string) {
	// no particular reason to make this until somebody asks for it
	pkgscope := makeScope()
	qiv := &QIVal{pkgscope, false, 1, path, mname}
	QIScope[mname] = qiv
	// For types, add "pkgsym" to the Type, for entities to the Symbol; CG needs this
	for _, tsym := range pkgdesc.pkgtypes {
		pkgscope.Add(tsym, 0)
		tsym.binding.Plist().Add("pkgsym", qiv)
	}
	for _, esym := range pkgdesc.pkgentities {
		pkgscope.Add(esym, 0)
		esym.plist.Add("pkgsym", qiv)
	}
	if QIPathMap[path] == nil {
		QIPathMap[path] = &mdlOrPkg{nil, QIScope[mname]}
	} else {
		QIPathMap[path].pkg = QIScope[mname]
	}
}

var pkgdb = make([]pkgdbEntry, 0, 10)
var pkgdbNames []string

// This mainly sets up "import go..." database but for convenience, we also set up SrcPath as being from the current user
func initPkgImports() {
	UserPath = os.Getenv("HOME")
	if UserPath == "" {
		panic("failed to get home dir")
	}
	SrcPath = UserPath + SrcPath
	rawItems := readModuleDB()
	makeMdbTree(rawItems)

	importPath = SrcPath + "/modules/"
	//fmt.Println("prefixing srcpath with", usr.HomeDir)

	// Various helper defns, then pkgdb
	var emptySymlist = []*Symbol{}
	var makePkgType = func(nm string, attribs []*Symbol) *Type {
		ret := makeType(TFTuple, attribs, false)
		ret.plist.Add("pkgtype", nm)
		return ret
	}
	var listb = makeListType(TypeByte)
	var listlistb = makeListType(listb)
	var xOrErrType = func(x *Type) *Type {
		var okerr = []*Symbol{makeSymbol("ok", x, nil), makeSymbol("err", TypeGoerror, nil)}
		return makeType(TFTuple, okerr, false)
	}
	countOrErrT := xOrErrType(TypeInt)
	strgOrErrT := xOrErrType(TypeString)
	var fileT = makePkgType("File", emptySymlist)
	var openretT = makeType(TFTuple, []*Symbol{makeSymbol("ok", fileT, nil), makeSymbol("err", TypeGoerror, nil)}, false)
	var writerT = makePkgType("Writer", emptySymlist)
	writerT.plist.Add("interfaceType", true) // mark because these don't cast
	var writeFtn = makeSymbol("Write", makeType(TFFtn, []*Type{writerT, listb}, countOrErrT, true), nil)
	writerT.addMethod(writeFtn)
	//var purify = func(sym *Symbol) *Symbol {
	//	sym.plist.Add("purified", true)
	//	return sym
	//}
	var connectify = func(sym *Symbol) *Symbol {
		sym.plist.Add("connected", true)
		return sym
	}
	var deferify = func(sym *Symbol) *Symbol {
		sym.plist.Add("connected", true)
		sym.plist.Add("deferred", true)
		return sym
	}
	// Flagvar is a hack so the backend can fix up the fact that the flag ftns return *T instead of T
	var flagvar = func(sym *Symbol) *Symbol {
		sym.plist.Add("flagvar", true)
		return sym
	}
	// Similarly, applyargs causes the backend to turn the final arg from list to ...
	// The name comes from the analogy to "apply" in lisp.
	var applyargs = func(sym *Symbol) *Symbol {
		sym.plist.Add("applyargs", true)
		return sym
	}
	var multiretify = func(sym *Symbol) *Symbol {
		sym.plist.Add("multiret", true)
		return sym
	}
	var methodify = func(sym *Symbol) *Symbol {
		sym.plist.Add("Gomethod", true)
		return sym
	}
	// Sometimes imported pkg ftn args use by-value/by-refnc policies that differ from the std policy of this compiler.
	// In such cases, we need annotations that the compiler backend listens to; these are Pointerize and dePointerize.
	// As of now, Pointerize is only used in varargs places (like fmt.Sscanf) so it's attached to ftn symbols rather than
	// fmlargs.
	var dePointerize = func(typ *Type) {
		typ.plist.Add("noPointer", true)
	}

	// *** don't forget that these pkgdbEntry's must be appended in alphabetic order of path name ***

	// archive/zip
	zfhSlots := []*Symbol{
		makeSymbol("Name", TypeString, nil),
		makeSymbol("Comment", TypeString, nil),
		makeSymbol("CompressedSize64", TypeInt64, nil),
		makeSymbol("UncompressedSize64", TypeInt64, nil),
	}
	zipfilehdrT := makePkgType("FileHeader", zfhSlots)
	zipfileT := makePkgType("File", zfhSlots)
	zrSlots := []*Symbol{
		makeSymbol("File", makeListType(zipfileT), nil),
		makeSymbol("Comment", TypeString, nil),
	}
	zipreaderT := makePkgType("Reader", zrSlots)
	zipreadcloserT := makePkgType("ReadCloser", zrSlots)
	zipreadcloserT.addMethod(methodify(makeSymbol("Close", makeType(TFFtn, []*Type{}, TypeNothing, false), nil)))
	zipfileT.addMethod(multiretify(methodify(makeSymbol("Open", makeType(TFFtn, []*Type{zipfileT}, xOrErrType(fileT), false), nil))))
	pkgdb = append(pkgdb, pkgdbEntry{"archive/zip", []*Symbol{
		makeSymbol("FileHeader", nil, zipfilehdrT),
		makeSymbol("File", nil, zipfileT),
		makeSymbol("Reader", nil, zipreaderT),
		makeSymbol("ReadCloser", nil, zipreadcloserT),
	}, []*Symbol{
		multiretify(makeSymbol("OpenReader", makeType(TFFtn, []*Type{TypeString}, xOrErrType(zipreadcloserT), true), nil)),
	}})

	// bytes
	pkgdb = append(pkgdb, pkgdbEntry{"bytes", emptySymlist, []*Symbol{
		makeSymbol("Join", makeType(TFFtn, []*Type{listlistb, listb}, listb, true), nil),
		makeSymbol("ToLower", makeType(TFFtn, []*Type{listb}, listb, true), nil),
		makeSymbol("ToUpper", makeType(TFFtn, []*Type{listb}, listb, true), nil),
		makeSymbol("Split", makeType(TFFtn, []*Type{listb, listb}, listlistb, true), nil),
		makeSymbol("Index", makeType(TFFtn, []*Type{listb, listb}, TypeInt, true), nil),
	}})

	// flag
	pkgdb = append(pkgdb, pkgdbEntry{"flag", emptySymlist, []*Symbol{
		flagvar(deferify(makeSymbol("Int", makeType(TFFtn, []*Type{TypeString, TypeInt, TypeString}, TypeInt, true), nil))),
		flagvar(deferify(makeSymbol("Float64", makeType(TFFtn, []*Type{TypeString, TypeFloat, TypeString}, TypeFloat, true), nil))),
		flagvar(deferify(makeSymbol("String", makeType(TFFtn, []*Type{TypeString, TypeString, TypeString}, TypeString, true), nil))),
		flagvar(deferify(makeSymbol("Bool", makeType(TFFtn, []*Type{TypeString, TypeBool, TypeString}, TypeBool, true), nil))),
		deferify(makeSymbol("Args", makeType(TFFtn, []*Type{}, makeListType(TypeString), true), nil)),
		deferify(makeSymbol("Parse", makeType(TFFtn, []*Type{}, TypeNothing, true), nil)),
	}})

	// fmt
	ssym := makeSymbol("Sprint", makeType(TFFtn, []*Type{}, TypeString, true), nil)
	ssym.plist.Add("varany", true)
	sfsym := makeSymbol("Sprintf", makeType(TFFtn, []*Type{TypeString}, TypeString, true), nil)
	sfsym.plist.Add("varany", true)
	scantyp := makeType(TFFtn, []*Type{TypeString, TypeString}, TypeNothing, true)
	scansym := makeSymbol("Sscanf", scantyp, nil)
	scansym.plist.Add("varany", true)
	scansym.plist.Add("Pointerize", 2) // value of Pointerize is the index in arglist at which ptrzn starts
	// these are intended to be used with string.Builders, no TC is done on the io writers.
	fsym := makeSymbol("Fprint", makeType(TFFtn, []*Type{}, TypeNothing, true), nil)
	fsym.plist.Add("varany", true)
	ffsym := makeSymbol("Fprintf", makeType(TFFtn, []*Type{}, TypeNothing, true), nil)
	ffsym.plist.Add("varany", true)
	pkgdb = append(pkgdb, pkgdbEntry{"fmt", emptySymlist, []*Symbol{
		ssym,
		sfsym,
		scansym,
		connectify(fsym),
		connectify(ffsym),
	}})

	// gonum.org/v1/plot
	// Note that the subdirectories must appear in alpha order; that means we need to define a bunch of types & such
	// up front and then put them in the package definitions they're actually defined in.
	// Also, Canvas is a quite confusing type in the gonum plot pkgs. vg.Canvas is an interface, vgimg.Canvas is
	// a concrete type that implements vg.Canvas (and same for vgpdf.Canvas etc), while draw.Canvas is a kind of "augmented"
	// vg.Canvas, the kind that plot.Draw requires to actually make a plot.
	canvasT := makePkgType("Canvas", emptySymlist)   // this is the draw.Canvas type
	vgcanvasT := makePkgType("Canvas", emptySymlist) // this is the vgimg.Canvas type
	vglenT := TypeFloat.Copy().(*Type)
	vglenT.plist.Add("pkgtype", "Length")
	rgbaAttribs := make([]*Symbol, 4)
	for i, id := range []string{"R", "G", "B", "A"} {
		rgbaAttribs[i] = makeSymbol(id, TypeUint8, nil)
	}
	colorT := makePkgType("RGBA", rgbaAttribs)
	gstyletups := []*Symbol{makeSymbol("Color", colorT, nil),
		makeSymbol("Shape", TypeLabel, nil),
		makeSymbol("Radius", vglenT, nil)}
	gstyleT := makePkgType("GlyphStyle", gstyletups)
	gstyleSym := makeSymbol("GlyphStyle", gstyleT, nil)
	lstyletups := []*Symbol{makeSymbol("Color", colorT, nil),
		makeSymbol("Width", TypeFloat, nil)}
	lstyleT := makePkgType("LineStyle", lstyletups)
	lstyleSym := makeSymbol("LineStyle", lstyleT, nil)
	bartups := []*Symbol{makeSymbol("Color", colorT, nil),
		makeSymbol("Horizontal", TypeBool, nil),
		makeSymbol("Width", vglenT, nil)}
	barT := makePkgType("BarChart", bartups)
	linetups := []*Symbol{lstyleSym, makeSymbol("FillColor", colorT, nil)}
	lineT := makePkgType("Line", linetups)
	labelstups := []*Symbol{makeSymbol("XOffset", TypeFloat, nil), makeSymbol("YOffset", TypeFloat, nil)}
	labelsT := makePkgType("Labels", labelstups)
	scattertups := []*Symbol{gstyleSym}
	scatterT := makePkgType("Scatter", scattertups)
	plotTitleTups := []*Symbol{makeSymbol("Text", TypeString, nil)}
	plotTitleT := makePkgType("Title", plotTitleTups)
	plottups := []*Symbol{
		makeSymbol("Title", plotTitleT, nil),
		makeSymbol("BackgroundColor", colorT, nil),
	}
	var plotT = makePkgType("Plot", plottups)
	// we won't use these labels, just need a way to let any of the recognized Plotter types occur in Plot.Add.
	pconcsyms := []*Symbol{
		makeSymbol("a", barT, nil),
		makeSymbol("b", lineT, nil),
		makeSymbol("c", labelsT, nil),
		makeSymbol("d", scatterT, nil),
	}
	plotterConcs := makeType(TFOrtype, Bplst(pconcsyms))
	plotT.addMethod(methodify(makeSymbol("Add", makeType(TFFtn, []*Type{plotT, plotterConcs}, TypeNothing, false), nil)))
	plotT.addMethod(methodify(makeSymbol("Draw", makeType(TFFtn, []*Type{plotT, canvasT}, TypeNothing, false), nil)))
	savesym := makeSymbol("Save", makeType(TFFtn, []*Type{plotT, TypeFloat, TypeFloat, TypeString}, TypeGoerror, false), nil)
	plotT.addMethod(methodify(connectify(savesym)))
	newplotsym := makeSymbol("New", makeType(TFFtn, []*Type{}, plotT, true), nil)
	tiletups := []*Symbol{
		makeSymbol("Cols", TypeInt, nil),
		makeSymbol("Rows", TypeInt, nil),
		makeSymbol("PadTop", TypeFloat, nil),
		makeSymbol("PadBottom", TypeFloat, nil),
		makeSymbol("PadRight", TypeFloat, nil),
		makeSymbol("PadLeft", TypeFloat, nil),
		makeSymbol("PadX", TypeFloat, nil),
		makeSymbol("PadY", TypeFloat, nil),
	}
	tileT := makePkgType("Tiles", tiletups)
	llp := makeListType(makeListType(plotT))
	llc := makeListType(makeListType(canvasT))
	alignFntype := makeType(TFFtn, []*Type{llp, tileT, canvasT}, llc, true)
	dePointerize(alignFntype.v.(*Ftntype).fmlargs[1])
	alignsym := makeSymbol("Align", alignFntype, nil)
	pkgdb = append(pkgdb, pkgdbEntry{"gonum.org/v1/plot",
		[]*Symbol{makeSymbol("Plot", nil, plotT)},
		[]*Symbol{newplotsym, alignsym}})

	// gonum.org/v1/plot/plotter
	xytups := []*Symbol{makeSymbol("X", TypeFloat, nil), makeSymbol("Y", TypeFloat, nil)}
	xyT := makePkgType("XY", xytups)
	dePointerize(xyT)
	xysT := makeListType(xyT)
	xysT.plist.Add("pkgtype", "XYs")
	valsT := makeListType(TypeFloat).Copy().(*Type)
	valsT.plist.Add("pkgtype", "Values")
	xylbltups := []*Symbol{makeSymbol("XYs", xysT, nil), makeSymbol("Labels", makeListType(TypeString), nil)}
	xylblT := makePkgType("XYLabels", xylbltups)
	pkgdb = append(pkgdb, pkgdbEntry{"gonum.org/v1/plot/plotter",
		[]*Symbol{
			makeSymbol("XY", nil, xyT),
			makeSymbol("XYs", nil, xysT),
			makeSymbol("Values", nil, valsT),
			makeSymbol("XYLabels", nil, xylblT),
			makeSymbol("BarChart", nil, barT),
			makeSymbol("Line", nil, lineT),
			makeSymbol("Labels", nil, labelsT),
			makeSymbol("Scatter", nil, scatterT),
		},
		[]*Symbol{
			multiretify(makeSymbol("NewBarChart", makeType(TFFtn, []*Type{valsT, TypeFloat}, xOrErrType(barT), true), nil)),
			multiretify(makeSymbol("NewLine", makeType(TFFtn, []*Type{xysT}, xOrErrType(lineT), true), nil)),
			multiretify(makeSymbol("NewScatter", makeType(TFFtn, []*Type{xysT}, xOrErrType(scatterT), true), nil)),
			multiretify(makeSymbol("NewLabels", makeType(TFFtn, []*Type{xylblT}, xOrErrType(labelsT), true), nil)),
		}})

	// gonum.org/v1/plot/vg
	pkgdb = append(pkgdb, pkgdbEntry{"gonum.org/v1/plot/vg",
		[]*Symbol{makeSymbol("Canvas", nil, vgcanvasT), makeSymbol("Length", nil, vglenT)},
		[]*Symbol{}})

	// gonum.org/v1/plot/vg/draw
	pkgdb = append(pkgdb, pkgdbEntry{"gonum.org/v1/plot/vg/draw",
		[]*Symbol{makeSymbol("Canvas", nil, canvasT),
			makeSymbol("Tiles", nil, tileT)},
		[]*Symbol{
			makeSymbol("New", makeType(TFFtn, []*Type{vgcanvasT}, canvasT, true), nil),
		}})

	// gonum.org/v1/plot/vg/vgimg
	vgimgNew := makeSymbol("New", makeType(TFFtn, []*Type{vglenT, vglenT}, vgcanvasT, true), nil)
	pngcnvtups := []*Symbol{makeSymbol("Canvas", vgcanvasT, nil)}
	pngCT := makePkgType("PngCanvas", pngcnvtups)
	wrt2sym := makeSymbol("WriteTo", makeType(TFFtn, []*Type{pngCT, fileT}, countOrErrT, true), nil)
	// Note that "noPointer" is usually attached to types of fmlargs; here it indicates that the rcvr of the WriteTo
	// gomethod is not a pointer
	wrt2sym.plist.Add("noPointer", true)
	pngCT.addMethod(methodify(multiretify(wrt2sym)))
	pkgdb = append(pkgdb, pkgdbEntry{"gonum.org/v1/plot/vg/vgimg",
		[]*Symbol{makeSymbol("Canvas", nil, vgcanvasT), makeSymbol("PngCanvas", nil, pngCT)},
		[]*Symbol{vgimgNew}})

	// html/template
	var templateT = makePkgType("Template", emptySymlist)
	var xttype = makeType(TFFtn, []*Type{templateT, writerT, TypeString, TypeGoanything}, TypeGoerror, true)
	templateT.addMethod(methodify(makeSymbol("ExecuteTemplate", xttype, nil)))
	pkgdb = append(pkgdb, pkgdbEntry{"html/template",
		[]*Symbol{makeSymbol("Template", nil, templateT)},
		[]*Symbol{
			makeSymbol("Must", makeType(TFFtn, []*Type{templateT}, templateT, true), nil),
			makeSymbol("ParseGlob", makeType(TFFtn, []*Type{TypeString}, templateT, true), nil),
		}})

	// image/color
	gray16T := makePkgType("Gray16", emptySymlist)
	pkgdb = append(pkgdb, pkgdbEntry{"image/color",
		[]*Symbol{
			makeSymbol("RGBA", nil, colorT),
			makeSymbol("Gray16", nil, gray16T),
		},
		[]*Symbol{
			makeSymbol("Black", colorT, nil),
			makeSymbol("White", colorT, nil),
		}})

	// io
	pkgdb = append(pkgdb, pkgdbEntry{"io",
		[]*Symbol{
			makeSymbol("Writer", nil, writerT),
		},
		[]*Symbol{}})

	var dirEntryT = makePkgType("DirEntry", emptySymlist)
	var fileinfoT = makePkgType("FileInfo", emptySymlist)
	dirEntryT.plist.Add("interfaceType", true) // mark because these don't cast
	var deMthdName = methodify(makeSymbol("Name", makeType(TFFtn, []*Type{dirEntryT}, TypeString, true), nil))
	dirEntryT.addMethod(deMthdName)
	var deMthdDir = methodify(makeSymbol("IsDir", makeType(TFFtn, []*Type{dirEntryT}, TypeBool, true), nil))
	dirEntryT.addMethod(deMthdDir)
	var deMthdType = methodify(makeSymbol("Type", makeType(TFFtn, []*Type{dirEntryT}, TypeInt, true), nil))
	dirEntryT.addMethod(deMthdType)
	fiOrErrT := xOrErrType(fileinfoT)
	var deMthdInfo = multiretify(methodify(makeSymbol("Info", makeType(TFFtn, []*Type{dirEntryT}, fiOrErrT, true), nil)))
	dirEntryT.addMethod(deMthdInfo)
	var filesys0T = makePkgType("FS", emptySymlist)
	filesys0T.plist.Add("interfaceType", true) // mark because these don't cast
	var fsMthdOpen = makeSymbol("Open", makeType(TFFtn, []*Type{filesys0T}, TypeString, true), nil)
	filesys0T.addMethod(fsMthdOpen)
	delT := makeListType(dirEntryT)
	delOrErrT := xOrErrType(delT)
	strglistOrErrT := xOrErrType(makeListType(TypeString))
	readdir0 := makeSymbol("ReadDir", makeType(TFFtn, []*Type{filesys0T, TypeString}, delOrErrT, true), nil)
	glob0 := makeSymbol("Glob", makeType(TFFtn, []*Type{filesys0T, TypeString}, strglistOrErrT, true), nil)

	// io/fs
	pkgdb = append(pkgdb, pkgdbEntry{"io/fs",
		[]*Symbol{
			makeSymbol("DirEntry", nil, dirEntryT),
		},
		[]*Symbol{
			methodify(multiretify(connectify(readdir0))),
			methodify(multiretify(connectify(glob0))),
		}})

	// log
	var loggerT = makePkgType("Logger", emptySymlist)
	fatalmthd := methodify(makeSymbol("Fatal", makeType(TFFtn, []*Type{loggerT}, TypeNothing, true), nil))
	fatalmthd.plist.Add("varany", true)
	loggerT.addMethod(fatalmthd)
	panicmthd := methodify(makeSymbol("Panic", makeType(TFFtn, []*Type{loggerT}, TypeNothing, true), nil))
	panicmthd.plist.Add("varany", true)
	loggerT.addMethod(panicmthd)
	logpmthd := methodify(makeSymbol("Print", makeType(TFFtn, []*Type{loggerT}, TypeNothing, true), nil))
	logpmthd.plist.Add("varany", true)
	loggerT.addMethod(logpmthd)
	logpfmthd := methodify(makeSymbol("Printf", makeType(TFFtn, []*Type{loggerT}, TypeNothing, true), nil))
	logpfmthd.plist.Add("varany", true)
	loggerT.addMethod(logpfmthd)
	logplmthd := methodify(makeSymbol("Println", makeType(TFFtn, []*Type{loggerT}, TypeNothing, true), nil))
	logplmthd.plist.Add("varany", true)
	loggerT.addMethod(logplmthd)
	pkgdb = append(pkgdb, pkgdbEntry{"log",
		[]*Symbol{makeSymbol("Logger", nil, loggerT)},
		[]*Symbol{
			makeSymbol("Default", makeType(TFFtn, []*Type{}, loggerT, true), nil),
			makeSymbol("New", makeType(TFFtn, []*Type{writerT, TypeString, TypeInt}, loggerT, true), nil),
		}})

	// math
	pkgdb = append(pkgdb, pkgdbEntry{"math", emptySymlist, []*Symbol{
		makeSymbol("Sqrt", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("Sin", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("Cos", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("Exp", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("Log", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("Log2", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("Log10", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("Tan", makeType(TFFtn, []*Type{TypeFloat}, TypeFloat, true), nil),
		makeSymbol("E", TypeFloat, nil),
		makeSymbol("Pi", TypeFloat, nil),
		makeSymbol("Phi", TypeFloat, nil),
		makeSymbol("Sqrt2", TypeFloat, nil),
	}})

	// math/bits
	pkgdb = append(pkgdb, pkgdbEntry{"math/bits", emptySymlist, []*Symbol{
		makeSymbol("OnesCount", makeType(TFFtn, []*Type{TypeInt}, TypeInt, true), nil),
	}})

	// math/rand
	// Note: using connectify; it has same effect on symbols as purify
	var randT = makePkgType("Rand", emptySymlist)
	var randSrcT = makePkgType("Source", emptySymlist)
	intmthd := methodify(makeSymbol("Int", makeType(TFFtn, []*Type{randT}, TypeInt, true), nil))
	randT.addMethod(intmthd)
	intnmthd := methodify(makeSymbol("Intn", makeType(TFFtn, []*Type{randT, TypeInt}, TypeInt, true), nil))
	randT.addMethod(intnmthd)
	fltmthd := methodify(makeSymbol("Float64", makeType(TFFtn, []*Type{randT}, TypeFloat, true), nil))
	randT.addMethod(fltmthd)
	pkgdb = append(pkgdb, pkgdbEntry{"math/rand", []*Symbol{
		makeSymbol("Source", nil, randSrcT),
		makeSymbol("Rand", nil, randT),
	}, []*Symbol{
		connectify(makeSymbol("New", makeType(TFFtn, []*Type{randSrcT}, randT, true), nil)),
		connectify(makeSymbol("NewSource", makeType(TFFtn, []*Type{TypeInt}, randSrcT, true), nil)),
		connectify(makeSymbol("Int", makeType(TFFtn, []*Type{}, TypeInt, true), nil)),
		connectify(makeSymbol("Intn", makeType(TFFtn, []*Type{TypeInt}, TypeInt, true), nil)),
		connectify(makeSymbol("Float64", makeType(TFFtn, []*Type{}, TypeFloat, true), nil)),
		connectify(makeSymbol("NormFloat64", makeType(TFFtn, []*Type{}, TypeFloat, true), nil)),
		connectify(makeSymbol("ExpFloat64", makeType(TFFtn, []*Type{}, TypeFloat, true), nil)),
		connectify(makeSymbol("Seed", makeType(TFFtn, []*Type{TypeInt64}, TypeNothing, true), nil)),
	}})

	// net/http
	var filesysT = makePkgType("FileSystem", emptySymlist)
	filesysT.addMethod(methodify(makeSymbol("Open", makeType(TFFtn, []*Type{filesysT, TypeString}, openretT, true), nil)))
	var reswrtrT = makePkgType("ResponseWriter", emptySymlist)
	dePointerize(reswrtrT)
	reswrtrT.addMethod(methodify(writeFtn))
	// Make a Header getter for ResponseWriter that accepts Add
	hdrT := makePkgType("Header", emptySymlist)
	hdrAddT := makeType(TFFtn, []*Type{hdrT, TypeString, TypeString}, TypeNothing, false)
	hdrAddFn := makeSymbol("Add", hdrAddT, nil)
	hdrfnT := makeType(TFFtn, []*Type{reswrtrT}, hdrT, true)
	hdrT.addMethod(methodify(hdrAddFn))
	reswrtrT.addMethod(methodify(makeSymbol("Header", hdrfnT, nil)))
	// Make a Reader for the Body getter
	var readerT = makePkgType("Reader", emptySymlist)
	// Maybe not ideal to copy same as for File in os pkg but should be ok.
	readsym := makeSymbol("Read", makeType(TFFtn, []*Type{readerT, listb}, countOrErrT, true), nil)
	readerT.addMethod(methodify(multiretify(readsym)))
	rqattribs := make([]*Symbol, 4)
	rqattribs[0] = makeSymbol("Method", TypeString, nil)
	rqattribs[1] = makeSymbol("URL", TypeString, nil)
	rqattribs[2] = makeSymbol("Body", readerT, nil)
	rqattribs[3] = makeSymbol("Header", TypeString, nil)
	var requestT = makePkgType("Request", rqattribs)
	requestT.addMethod(methodify(makeSymbol("FormValue", makeType(TFFtn, []*Type{requestT, TypeString}, TypeString, true), nil)))
	requestT.addMethod(methodify(makeSymbol("Path", makeType(TFFtn, []*Type{requestT}, TypeString, true), nil)))
	var handlerT = makePkgType("Handler", emptySymlist)
	var hftT = makeType(TFFtn, []*Type{reswrtrT, requestT}, TypeNothing, true)
	handlerT.addMethod(deferify(methodify(makeSymbol("ServeHTTP", hftT, nil))))
	var respcodes = []string{
		"StatusFound", "StatusInternalServerError", "StatusOK", "StatusNotFound",
	}
	rcterms := make([]*Symbol, len(respcodes))
	for i, s := range respcodes {
		rcterms[i] = makeSymbol(s, TypeInt, nil)
	}
	pkgdb = append(pkgdb, pkgdbEntry{"net/http",
		[]*Symbol{
			makeSymbol("FileSystem", nil, filesysT),
			makeSymbol("ResponseWriter", nil, reswrtrT),
			makeSymbol("Request", nil, requestT),
			makeSymbol("Handler", nil, handlerT),
			makeSymbol("HandleFuncThunk", nil, hftT),
		},
		[]*Symbol{
			makeSymbol("Dir", makeType(TFFtn, []*Type{TypeString}, filesysT, true), nil),
			makeSymbol("Error", makeType(TFFtn, []*Type{reswrtrT, TypeString, TypeInt}, TypeNothing, true), nil),
			makeSymbol("FileServer", makeType(TFFtn, []*Type{filesysT}, handlerT, true), nil),
			makeSymbol("Handle", makeType(TFFtn, []*Type{TypeString, handlerT}, TypeNothing, true), nil),
			makeSymbol("HandleFunc", makeType(TFFtn, []*Type{TypeString, hftT}, TypeNothing, true), nil),
			deferify(makeSymbol("ListenAndServe", makeType(TFFtn, []*Type{TypeString, TypeNil}, TypeNothing, true), nil)),
			makeSymbol("Redirect", makeType(TFFtn, []*Type{reswrtrT, requestT, TypeString, TypeInt}, TypeNothing, true), nil),
			makeSymbol("StripPrefix", makeType(TFFtn, []*Type{TypeString, handlerT}, handlerT, true), nil),
			makeSymbol("StatusInternalServerError", TypeInt, makeIntlit(500)),
			makeSymbol("StatusFound", TypeInt, makeIntlit(302)),
		}})

	// os
	var timeT = makePkgType("Time", emptySymlist)
	dePointerize(timeT)
	fileinfoT.addMethod(methodify(makeSymbol("Name", makeType(TFFtn, []*Type{fileinfoT}, TypeString, true), nil)))
	fileinfoT.addMethod(methodify(makeSymbol("Size", makeType(TFFtn, []*Type{fileinfoT}, TypeInt64, true), nil)))
	fileinfoT.addMethod(methodify(makeSymbol("IsDir", makeType(TFFtn, []*Type{fileinfoT}, TypeBool, true), nil)))
	fileinfoT.addMethod(methodify(makeSymbol("ModTime", makeType(TFFtn, []*Type{fileinfoT}, timeT, true), nil)))
	var statretT = makeType(TFTuple, []*Symbol{makeSymbol("ok", fileinfoT, nil), makeSymbol("err", TypeGoerror, nil)}, false)
	sym := makeSymbol("Read", makeType(TFFtn, []*Type{fileT, listb}, countOrErrT, true), nil)
	fileT.addMethod(methodify(multiretify(sym)))
	sym = makeSymbol("Write", makeType(TFFtn, []*Type{fileT, listb}, countOrErrT, true), nil)
	fileT.addMethod(methodify(multiretify(sym)))
	sym = makeSymbol("Stat", makeType(TFFtn, []*Type{fileT}, statretT, true), nil)
	fileT.addMethod(methodify(multiretify(sym)))
	sym = makeSymbol("Close", makeType(TFFtn, []*Type{fileT}, TypeGoerror, true), nil)
	fileT.addMethod(methodify(multiretify(connectify(sym))))
	var strgboolT = makeType(TFTuple, []*Symbol{makeSymbol("value", TypeString, nil), makeSymbol("present", TypeBool, nil)}, false)
	pkgdb = append(pkgdb, pkgdbEntry{"os",
		[]*Symbol{
			makeSymbol("File", nil, fileT),
			makeSymbol("FileInfo", nil, fileinfoT),
			makeSymbol("DirEntry", nil, dirEntryT),
		},
		[]*Symbol{
			makeSymbol("Stdin", fileT, nil), makeSymbol("Stdout", fileT, nil), makeSymbol("Stderr", fileT, nil),
			multiretify(connectify(makeSymbol("Open", makeType(TFFtn, []*Type{TypeString}, openretT, true), nil))),
			multiretify(connectify(makeSymbol("Create", makeType(TFFtn, []*Type{TypeString}, openretT, true), nil))),
			multiretify(connectify(makeSymbol("ReadDir", makeType(TFFtn, []*Type{TypeString}, delOrErrT, true), nil))),
			multiretify(connectify(makeSymbol("Getwd", makeType(TFFtn, []*Type{}, strgOrErrT, true), nil))),
			// Mkdir and MkdirAll put permissions in a FileMode, which is an integer. Try pkgtype if int doesn't work?
			connectify(makeSymbol("Getenv", makeType(TFFtn, []*Type{TypeString}, TypeString, true), nil)),
			connectify(makeSymbol("LookupEnv", makeType(TFFtn, []*Type{TypeString}, strgboolT, true), nil)),
			connectify(makeSymbol("Mkdir", makeType(TFFtn, []*Type{TypeString, TypeInt}, TypeGoerror, true), nil)),
			connectify(makeSymbol("MkdirAll", makeType(TFFtn, []*Type{TypeString, TypeInt}, TypeGoerror, true), nil)),
			connectify(makeSymbol("Chdir", makeType(TFFtn, []*Type{TypeString}, TypeGoerror, true), nil)),
			connectify(makeSymbol("Exit", makeType(TFFtn, []*Type{TypeInt}, TypeNothing, true), nil)),
			connectify(makeSymbol("Rename", makeType(TFFtn, []*Type{TypeString, TypeString}, TypeGoerror, true), nil)),
			connectify(makeSymbol("Remove", makeType(TFFtn, []*Type{TypeString}, TypeGoerror, true), nil)),
			multiretify(connectify(makeSymbol("Stat", makeType(TFFtn, []*Type{TypeString}, statretT, true), nil))),
			makeSymbol("DirFS", makeType(TFFtn, []*Type{TypeString}, filesys0T, true), nil),
		}})

	// os/exec
	cmdT := makePkgType("Cmd", emptySymlist)
	var lberr = []*Symbol{makeSymbol("out", listb, nil), makeSymbol("err", TypeGoerror, nil)}
	var lbOrErrT = makeType(TFTuple, lberr, false)
	cosym := makeSymbol("CombinedOutput", makeType(TFFtn, []*Type{cmdT}, lbOrErrT, false), nil)
	cmdT.addMethod(multiretify(methodify(cosym)))
	osym := makeSymbol("Output", makeType(TFFtn, []*Type{cmdT}, lbOrErrT, false), nil)
	cmdT.addMethod(multiretify(methodify(osym)))
	cmdfnsym := makeSymbol("Command", makeType(TFFtn, []*Type{TypeString, makeListType(TypeString)}, cmdT, true), nil)
	cmdfn := applyargs(connectify(cmdfnsym))
	pkgdb = append(pkgdb, pkgdbEntry{"os/exec", []*Symbol{
		makeSymbol("Cmd", nil, cmdT),
	}, []*Symbol{
		cmdfn,
	}})

	// os/user
	userAttribs := make([]*Symbol, 5)
	userAttribs[0] = makeSymbol("Uid", TypeString, nil)
	userAttribs[1] = makeSymbol("Gid", TypeString, nil)
	userAttribs[2] = makeSymbol("Username", TypeString, nil)
	userAttribs[3] = makeSymbol("Name", TypeString, nil)
	userAttribs[4] = makeSymbol("HomeDir", TypeString, nil)
	userT := makePkgType("User", userAttribs)
	var usererr = []*Symbol{makeSymbol("ok", userT, nil), makeSymbol("err", TypeGoerror, nil)}
	var userOrErrT = makeType(TFTuple, usererr, false)
	userfnsym := makeSymbol("Current", makeType(TFFtn, []*Type{}, userOrErrT, true), nil)
	curUserFn := multiretify(connectify(userfnsym))
	pkgdb = append(pkgdb, pkgdbEntry{"os/user", []*Symbol{
		makeSymbol("User", nil, userT),
	}, []*Symbol{
		curUserFn,
	}})

	// path
	pairstrgT := makeType(TFTuple, []*Symbol{makeSymbol("dir", TypeString, nil), makeSymbol("file", TypeString, nil)}, false)
	splttyp := makeType(TFFtn, []*Type{TypeString}, pairstrgT, true)
	pathsplit := makeSymbol("Split", splttyp, nil)
	sstyp := makeType(TFFtn, []*Type{TypeString}, TypeString, true)
	pkgdb = append(pkgdb, pkgdbEntry{"path",
		[]*Symbol{},
		[]*Symbol{multiretify(pathsplit),
			makeSymbol("Base", sstyp, nil),
			makeSymbol("Clean", sstyp, nil),
			makeSymbol("Dir", sstyp, nil),
			makeSymbol("Ext", sstyp, nil),
		}})

	// regexp
	var regexpT = makePkgType("Regexp", emptySymlist)
	var listi = makeListType(TypeInt)
	var listlisti = makeListType(listi)
	regexpT.addMethod(methodify(makeSymbol("Find", makeType(TFFtn, []*Type{regexpT, listb}, listb, true), nil)))
	regexpT.addMethod(methodify(makeSymbol("FindIndex", makeType(TFFtn, []*Type{regexpT, listb}, listi, true), nil)))
	regexpT.addMethod(methodify(makeSymbol("FindSubmatchIndex", makeType(TFFtn, []*Type{regexpT, listb}, listi, true), nil)))
	regexpT.addMethod(methodify(makeSymbol("FindAllIndex", makeType(TFFtn, []*Type{regexpT, listb, TypeInt}, listlisti, true), nil)))
	regexpT.addMethod(methodify(makeSymbol("FindAllSubmatchIndex", makeType(TFFtn, []*Type{regexpT, listb, TypeInt}, listlisti, true), nil)))
	pkgdb = append(pkgdb, pkgdbEntry{"regexp",
		[]*Symbol{makeSymbol("Regexp", nil, regexpT)},
		[]*Symbol{makeSymbol("MustCompile", makeType(TFFtn, []*Type{TypeString}, regexpT, true), nil)}})

	// runtime
	// Note: pkg runtime is full of all kinds of goodies. I've got a very minimal subset defined by what I've needed so far.
	pkgdb = append(pkgdb, pkgdbEntry{"runtime",
		[]*Symbol{},
		[]*Symbol{
			makeSymbol("GC", makeType(TFFtn, []*Type{}, TypeNothing, true), nil),
		}})

	// runtime/pprof
	pkgdb = append(pkgdb, pkgdbEntry{"runtime/pprof",
		[]*Symbol{},
		[]*Symbol{
			connectify(makeSymbol("StartCPUProfile", makeType(TFFtn, []*Type{writerT}, TypeGoerror, true), nil)),
			makeSymbol("StopCPUProfile", makeType(TFFtn, []*Type{}, TypeNothing, true), nil),
			connectify(makeSymbol("WriteHeapProfile", makeType(TFFtn, []*Type{writerT}, TypeGoerror, true), nil)),
		}})

	// sort
	listT := makeListType(makeType(TFGTS, makeGTS("T", TPEntity)))
	pkgdb = append(pkgdb, pkgdbEntry{"sort",
		[]*Symbol{},
		[]*Symbol{makeSymbol("Sort", makeType(TFFtn, []*Type{listT}, TypeNothing, false), nil)}})

	// strings
	lstrg := makeListType(TypeString)
	var builderT = makePkgType("Builder", emptySymlist)
	var bldretT = makeType(TFTuple, []*Symbol{makeSymbol("len", TypeInt, nil), makeSymbol("err", TypeGoerror, nil)}, false)
	builderT.addMethod(methodify(makeSymbol("String", makeType(TFFtn, []*Type{builderT}, TypeString, true), nil)))
	builderT.addMethod(methodify(makeSymbol("WriteString", makeType(TFFtn, []*Type{builderT, TypeString}, bldretT, false), nil)))
	builderT.addMethod(methodify(makeSymbol("WriteByte", makeType(TFFtn, []*Type{builderT, TypeByte}, bldretT, false), nil)))
	builderT.addMethod(methodify(makeSymbol("Write", makeType(TFFtn, []*Type{builderT, listb}, bldretT, false), nil)))
	pkgdb = append(pkgdb, pkgdbEntry{"strings",
		[]*Symbol{makeSymbol("Builder", nil, builderT)},
		[]*Symbol{
			makeSymbol("Contains", makeType(TFFtn, []*Type{TypeString, TypeString}, TypeBool, true), nil),
			makeSymbol("Index", makeType(TFFtn, []*Type{TypeString, TypeString}, TypeInt, true), nil),
			makeSymbol("LastIndex", makeType(TFFtn, []*Type{TypeString, TypeString}, TypeInt, true), nil),
			makeSymbol("IndexByte", makeType(TFFtn, []*Type{TypeString, TypeByte}, TypeInt, true), nil),
			makeSymbol("Join", makeType(TFFtn, []*Type{lstrg, TypeString}, TypeString, true), nil),
			makeSymbol("ToLower", makeType(TFFtn, []*Type{TypeString}, TypeString, true), nil),
			makeSymbol("ToUpper", makeType(TFFtn, []*Type{TypeString}, TypeString, true), nil),
			makeSymbol("TrimSpace", makeType(TFFtn, []*Type{TypeString}, TypeString, true), nil),
			makeSymbol("Split", makeType(TFFtn, []*Type{TypeString, TypeString}, lstrg, true), nil),
		}})

	// time
	// Note: timeT defined earlier to define FileInfo.ModTime
	timeT.addMethod(methodify(makeSymbol("Sub", makeType(TFFtn, []*Type{timeT, timeT}, TypeInt, true), nil)))
	timeT.addMethod(methodify(makeSymbol("Compare", makeType(TFFtn, []*Type{timeT, timeT}, TypeInt, true), nil)))
	timeT.addMethod(methodify(makeSymbol("Before", makeType(TFFtn, []*Type{timeT, timeT}, TypeBool, true), nil)))
	timeT.addMethod(methodify(makeSymbol("After", makeType(TFFtn, []*Type{timeT, timeT}, TypeBool, true), nil)))
	var chansym = makeSymbol("C", makeType(TFGochan, timeT, "both"), nil)
	var tickerT = makeType(TFTuple, []*Symbol{chansym}, false)
	var durationT = TypeInt64.Copy().(*Type)
	nowsym := makeSymbol("Now", makeType(TFFtn, []*Type{}, timeT, true), nil)
	sincesym := makeSymbol("Since", makeType(TFFtn, []*Type{timeT}, durationT, true), nil)
	// now and since need special something to prevent things moving over them in optimzn
	nowsym.plist.Add("realtimer", true)
	sincesym.plist.Add("realtimer", true)
	durationT.addMethod(methodify(makeSymbol("Microseconds", makeType(TFFtn, []*Type{durationT}, TypeInt64, true), nil)))
	durationT.addMethod(methodify(makeSymbol("Milliseconds", makeType(TFFtn, []*Type{durationT}, TypeInt64, true), nil)))
	durationT.addMethod(methodify(makeSymbol("Seconds", makeType(TFFtn, []*Type{durationT}, TypeFloat, true), nil)))
	durationT.addMethod(methodify(makeSymbol("String", makeType(TFFtn, []*Type{durationT}, TypeString, true), nil)))
	pkgdb = append(pkgdb, pkgdbEntry{"time",
		[]*Symbol{makeSymbol("Duration", nil, durationT),
			makeSymbol("Time", nil, timeT),
			makeSymbol("Ticker", nil, tickerT),
		},
		[]*Symbol{
			makeSymbol("NewTicker", makeType(TFFtn, []*Type{TypeInt}, tickerT, true), nil),
			connectify(nowsym),
			connectify(sincesym),
		}})

	// now we're done initz pkgdb. Set up pkgdbNames so we can search fast
	// We can add in order and then binsearch since we've arranged for the pkgdb to list pkgs in alpha order of paths
	pkgdbNames = make([]string, len(pkgdb))
	for i, e := range pkgdb {
		pkgdbNames[i] = e.pkgpath
	}

	// add fmt to gblscope
	inx := binsearchStrings(pkgdbNames, "fmt")
	pkgdb[inx].AddPkg("fmt", "fmt")
	gblScope.importPkgs = append(gblScope.importPkgs, "fmt")
}

// Here for the "import go" statement where again, the parser ensures that the form is ok.
// The needed checking is similar to std import but the actions after that are different.
func tcImportPkg(path, mname string, tci *TCInfo, t *TermTT) Term {
	if mname != "" {
		qi := QIScope[mname]
		if qi != nil {
			if path != qi.path {
				// whoops, there's a conflict
				return tci.Error(fmt.Sprintf("qualifier %s is already in use for %s", mname, qi.path), t)
			}
			qi.activeCount++
			tci.scopes.importPkgs = append(tci.scopes.importPkgs, path)
			return t
		}
	}
	//fmt.Print("importpkg ", path, mname)
	inx := binsearchStrings(pkgdbNames, path)
	if inx < 0 || pkgdbNames[inx] != path {
		return tci.Error(fmt.Sprintf("package not found for %s", path), t)
	}

	// pkg is known, but first check the pathmap
	inPath := QIPathMap[path]
	if inPath != nil && inPath.pkg != nil {
		// is the QI the same? return if yes, error if no
		if inPath.pkg.mname != mname && inPath.pkg.activeCount > 0 {
			return tci.Error(fmt.Sprintf("path %s was already imported with QI %s", path, inPath.pkg.mname), t)
		}
		if inPath.pkg.activeCount > 0 {
			//fmt.Println("same same")
			return TrueLiteral
		}
		//fmt.Println("reactivate")
		inPath.pkg.activeCount = 1
		if mname == "" { // melted?
			for _, sx := range inPath.pkg.scope.entries {
				symx, added := tci.scopes.AddIfPossible(sx.sym)
				if added != "" {
					return tci.Error(added, t)
				}
				sx.sym = symx
			}
		} else {
			QIScope[mname] = inPath.pkg
		}
		tci.scopes.importPkgs = append(tci.scopes.importPkgs, path)
		return t

	}
	//fmt.Println("load first time")

	pkgdb[inx].AddPkg(path, mname)
	tci.scopes.importPkgs = append(tci.scopes.importPkgs, path)
	return t
}

// Predicate true of terms that are considered to be queries.
// Basic literals are not considered here & should be filtered separately.
// Some of these are or might be illegal to call the compiler on, such as loop, while, ITE stmts.
func queryTag(trm Term) bool {
	tg := trm.Tag()

	// only funcalls in above list except "rewrite"
	if tg == Funcall {
		for _, fnnm := range []string{"rewrite", "load", "applyST"} {
			if callingNamed(trm, fnnm) {
				return false
			}
		}
		return true
	}
	return tg == SymbolTag || tg == Symchain || tg == Litform0 || tg == IfStmt || tg == LoopStmt || tg == WhileStmt
}

// loadFile returns an error if the call is malformed, exits on syntax error, prints and returns a TC error,
// and returns the last term TC'd if no error.
// It should be called with the desired path of the file (if not absolute, then relative to the current wd), except
// that if fullpath starts with "modules/" we look first locally and then globally for a file with that name.
func loadFile(fullpath string, asModule bool, tci *TCInfo) (tcout Term) {
	n := len(fullpath)
	if n < 4 || fullpath[n-4:n] != ".d8m" {
		fullpath += ".d8m"
	}
	var fd *os.File
	var err error
	// try opening locally and if not, globally
	if asModule {
		fullpath = modulesFullpath(fullpath)
	}
	fd, err = os.Open(fullpath)
	if err != nil {
		tcout = tci.Error(fmt.Sprintf("can't open %s", fullpath), makeStringTerm(fullpath, Pos(-1), true))
		return
	}
	lineReader := bufio.NewScanner(fd)
	scanner := newScanner(lineReader, fullpath, 0, true)
	parser := newParser(scanner)
	svscanner := tci.scanner
	tci.scanner = scanner
	defer func() {
		r := recover()
		if r == "parser bailing" {
			for _, errtrm := range parser.errors {
				fmt.Println("syntax error:", errtrm.String())
			}
			log.Fatal("exiting...")
		} else if r != nil {
			panic(r)
		}
	}()
	for {
		nxterm := parser.stmtOrExpr()
		if nxterm == nil {
			break
		} else if nxterm.Tag() == ErrorTag {
			tcout = nxterm
		} else if callingNamed(nxterm, "load") {
			args := nxterm.(*TermTL).args
			if len(args) != 1 || args[0].Tag() != Stringlit {
				tcout = tci.Error("bad call to load", nxterm)
			} else {
				path := args[0].(*TermB).value
				asMod := false
				if len(path) >= 8 && path[:8] == "modules/" {
					path = path[8:]
					asMod = true
				}
				tcout = loadFile(path, asMod, tci)
			}
		} else {
			tcout = nxterm.Typecheck(nil, tci)
		}
		//foo := anyModcall(tcout)
		//fmt.Println("(from import) anymod=", foo, ";", tcout)
		if tcout.Tag() == ErrorTag {
			for _, errtrm := range parser.errors {
				fmt.Println("syntax error:", errtrm.String())
			}
			break
		}
		if !tci.queryOk {
			_, istb := tcout.(*TermB)
			if istb || queryTag(tcout) {
				tcout = tci.Error("non-definition in loaded file", tcout)
				break
			}
		}
	}
	fd.Close()
	tci.scanner = svscanner
	return
}
