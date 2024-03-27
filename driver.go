// driver for d8mingo

package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"os/exec"
)

// True if trm contains a call to a deferred ftn. Do not go into non-local bindings.
func anyDeferred(trm Term) bool {
	var deferWorkfn func(t Term, s interface{}) int
	deferWorkfn = func(t Term, s interface{}) int {
		ret := 0
		tg := t.Tag()
		if tg == SymbolTag {
			seen := s.(*[]*Symbol)
			for _, sn := range *seen {
				if t == sn {
					return 1 // don't probe
				}
			}
			sym := t.(*Symbol)
			*seen = append(*seen, sym)
			if sym.plist.Find("deferred") != nil {
				return -1
			}
		}
		if tg == StmapStmt {
			return 1
		}
		if tg == Valdecl || tg == Vardecl {
			sym := t.(*TermT).arg0.(*Symbol)
			if TraversePre(sym.binding, deferWorkfn, s) {
				return -1
			}
			ret = 1
		}
		return ret
	}
	seen := make([]*Symbol, 0, 2)
	return TraversePre(trm, deferWorkfn, &seen)
}

// Use a logger to direct dbgprint output to a file
var dbgfile, _ = os.Create("dbg.log")
var logger = log.New(dbgfile, "", 0)

type compileParamsDict []*struct {
	key   string
	value Term
}

func (cpd compileParamsDict) Find(ky string) (ret Term) {
	for _, kv := range cpd {
		if kv.key == ky {
			return kv.value
		}
	}
	return // nil
}

func main() {
	gblScope.topscope = true
	tci := makeTCInfo(gblScope)
	initLateBIFunctions(tci)
	loadFile("preload.d8m", true, tci)
	fmt.Println("finished preload")

	fd, err := os.Open("foo.txt")
	if err != nil {
		log.Fatal("file open error: ", err)
	}
	lineReader := bufio.NewScanner(fd)
	scanner := newScanner(lineReader, "foo", 0, true)
	tci.scanner = scanner
	parser := newParser(scanner)
	defer func() {
		r := recover()
		if r == "parser bailing" {
			for _, errtrm := range parser.errors {
				fmt.Println("error:", errtrm.String())
			}
			log.Fatal("exiting...")
		} else if r != nil {
			panic(r)
		}
	}()
	for {
		nxterm := parser.stmtOrExpr()
		if nxterm == nil {
			log.Fatal("EOF reached")
		} else if nxterm.Tag() == ErrorTag {
			for _, errterm := range parser.errors {
				fmt.Println("error:", errterm.String())
			}
			log.Fatal("unrecoverable")
		}
		//fmt.Println("starting nxterm")
		var tcout Term
		if nxterm.Tag() == Stmts { // this has to be multi-decl at top level
			for _, nxt := range nxterm.(*TermL).args {
				tcout = nxt.Typecheck(nil, tci)
			}
		} else {
			tcout = nxterm.Typecheck(nil, tci)
		}
		// interpret certain funcalls: load, compile, setwd
		if callingNamed(tcout, "load") || callingNamed(tcout, "setwd") {
			strg := ""
			strgarg := tcout.(*TermTL).args[0]
			if strgarg.Tag() == Stringlit {
				strg = strgarg.(*TermB).value
			} else if strgarg.Tag() == SymbolTag {
				sym := strgarg.(*Symbol)
				if sym.binding != nil && sym.binding.Tag() == Stringlit {
					strg = sym.binding.(*TermB).value
				}
			}
			cmdname := tcout.(*TermTL).trm.(*Symbol).ident
			if strg == "" {
				fmt.Println("error: can't interpret arg to", cmdname, "command")
			} else if cmdname == "load" {
				tcout = loadFile(strg, false, tci)
			} else {
				patherr := os.Chdir(strg)
				if patherr != nil {
					log.Fatal(patherr)
				}
				fmt.Println("changed wd to", strg)
			}
		} else if callingNamed(tcout, "compile") {
			// compile(queryTerm, params...) where params is a seq of keyword~value terms like file and more.
			out0 := tcout.(*TermTL)
			query := out0.args[0]
			defrd := anyDeferred(query)
			lcinfo := scanner.getLineCharInfo(query.First().byteIndex(), scanner.fname)
			fmt.Println("compiling from", scanner.fname, "at line", lcinfo.linenum)
			cparams := make(compileParamsDict, 0, 2)
			if len(out0.args) > 1 {
				for _, tt := range out0.args[1:] {
					if tt.Tag() != TildeExpr {
						log.Fatal("bad format in compile")
					}
					// TC checked that arg0 is a label
					tt0 := tt.(*TermTT)
					cparams = append(cparams, &struct {
						key   string
						value Term
					}{tt0.arg0.(*TermB).value, tt0.arg1})
				}
			}
			// before compiling, set up any relevant params
			// For now, that means "unroll"
			unroll := cparams.Find("unroll")
			if unroll == TrueLiteral {
				unrollEachloops = true
			}
			compiled, qcError := queryCompile(query, gblScope)
			if qcError != nil {
				fmt.Println("compile error:", qcError.String())
			} else {
				// here, postmap is a StringLit
				fname := "d8mqry.go"
				fnameTrm := cparams.Find("file")
				if fnameTrm != nil {
					fnameTrm = Simplify(fnameTrm)
					if fnameTrm.Tag() == Stringlit || fnameTrm.Tag() == Labellit {
						fname = fnameTrm.(*TermB).value
						if len(fname) < 3 || fname[len(fname)-3:] != ".go" {
							fname += ".go"
						}
					}
				}
				f, err := os.Create(fname)
				if err != nil {
					log.Fatal(err)
				}
				f.WriteString(compiled)
				f.Close()
				dbgfile.Close()
				if defrd {
					fmt.Println("query compiled to", fname)
				} else {
					out, _ := exec.Command("go", "run", "d8mqry.go").CombinedOutput()
					fmt.Println("result: ", string(out))
				}
			}
		}
		if len(tci.errors) > 0 {
			for _, errtrm := range tci.errors {
				fmt.Println("error:", errtrm.String())
			}
			log.Fatal("exiting...")
		}
	}

}
