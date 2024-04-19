// test driver for d8m compiler

package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

// filename has a specific form; strip it to the basename
func stripCruft(filename string, pfx, sfx string) string {
	pfxlen := len(pfx)
	sfxlen := len(sfx)
	return filename[pfxlen : len(filename)-sfxlen]
}

// CompareFiles does pretty much what the `cmp` shell command does. Here, it's used for both `.go` and `.out` files.
// For `.out` files, there's a bit of extra hackery, see below.
//
// - The fname argument is the full file name, ready to pass to os.Open.
// - The content argument is the string to compare, either from the d8m compiler (for `.go` files) or from exec of the
// compiled binary (for `.out` files).
//
// The extra hackery is to handle more complicated situations for `.out`. It is not currently used but I've left it in
// in case it might be useful in the future. If the .out file starts with "||| " we treat it as a shell command that should
// be run and its output compared with the following line. Thus, the form of the .out file is as pairs of lines, with the
// first line of each pair preceded by `||| `. The first line of each pair is a shell command, the second is the expected
// output of the shell command. (Often empty.)
func compareFiles(fname, content, msgword string) bool {
	fd, _ := os.Open(fname)
	finfo, _ := fd.Stat()
	flen := int(finfo.Size())
	fbuf := make([]byte, flen)
	nread, _ := fd.Read(fbuf)
	if nread != flen {
		panicmsg := fmt.Sprintf("reading %s output file", msgword)
		panic(panicmsg)
	}
	fd.Close()
	// local function cmdline checks for the ||| pattern and if so, interprets the next two lines as command and response.
	// If this succeeds, return index of line following the pair. If no ||| return -1, if ||| and command fails, return -2.
	var cmdline = func(ofs int) int {
		if string(fbuf[ofs:ofs+4]) != "||| " {
			return -1
		}
		endline := bytes.IndexByte(fbuf[ofs:], '\n')
		cmdstrg := string(fbuf[ofs+4 : endline])
		words := bytes.Split(fbuf[ofs+4:endline], []byte{' '})
		args := make([]string, 0, len(words)-1)
		for _, w := range words[1:] {
			if len(w) > 0 {
				args = append(args, string(w))
			}
		}
		ofs = endline + 1
		endline = bytes.IndexByte(fbuf[ofs:], '\n')
		if endline < 0 {
			endline = len(fbuf)
		} else {
			endline += ofs
		}
		wanted := string(fbuf[ofs:endline])
		out, _ := exec.Command(string(words[0]), args...).CombinedOutput()
		if string(out) != wanted {
			fmt.Println(fmt.Sprintf("on cmd '%s' expected '%s' but got '%s", cmdstrg, wanted, out))
			return -2
		}
		return endline + 1
	}
	if msgword == "runtime" && flen > 4 {
		offset := 0
		runcount := 0
		for {
			offset = cmdline(offset)
			if offset >= 0 {
				runcount++
			}
			if offset < 0 || offset >= len(fbuf)-4 {
				break
			}
		}
		if offset == -2 {
			return false
		}
		if runcount > 0 {
			return true
		}
		// else fall through
	}
	// here, handle the standard case where cmdline doesn't find the ||| pattern.
	compareOk := true
	if content != string(fbuf) {
		compareOk = false
		qcout0 := []byte(content)
		cmplen := len(fbuf)
		if msgword == "compile" && cmplen < 2 {
			if !*flagFix {
				fmt.Println(".go file appears to be empty; run with -f flag")
			}
		} else if cmplen < 200 && len(qcout0) < 2000 {
			fmt.Print("in ", msgword, " comparison, expected: ", string(fbuf))
			fmt.Println("but got: ", content)
		} else {
			if cmplen > len(qcout0) {
				cmplen = len(qcout0)
			}
			for i := 0; i < cmplen; i++ {
				if qcout0[i] != fbuf[i] {
					fmt.Println(msgword, "output differs, first at byte", i)
					break
				}
			}
		}
	}
	return compareOk
}

// Predicate true when queryCompile should be called on trm
func compileableCheck(trm Term) bool {
	tg := trm.Tag()
	for _, tg0 := range []Termtag{Valdecl, Vardecl, ImportStmt, ImportPkgStmt, StmapStmt, GivenStmt, AssertStmt} {
		if tg == tg0 {
			return false
		}
	}
	if trm.Tag() == Funcall && trm.(*TermTL).trm.Tag() == SymbolTag {
		ident := trm.(*TermTL).trm.(*Symbol).ident
		if ident == "applyST" || ident == "rewrite" {
			return false
		}
	}
	return true
}

var flagRunAll = flag.Bool("a", false, "run all: compile and execute everything, even if generated .go matches expected")
var flagFix = flag.Bool("f", false, "fix: if the .go changes but the executable generates expected output, modify the expected .go")

// Std driver creates a real log file for dbgprints, here we don't want it
var dbgfile, _ = os.Create("/dev/null")
var logger = log.New(dbgfile, "", 0)

// Separately, we do want to send most of the output to a log file since the goland console tends to overflow
var logfile, _ = os.Create("./tester.log")
var tolog = log.New(logfile, "", 0)

// Worth noting that a fair amount of attention is given to ensuring that the compiler state is reset to clean between
// each test. This involves rolling back gblScope entries and the like in ways that are highly dependent on implementation
// details. Not pretty...
func main() {
	flag.Parse()
	gblScope.topscope = true
	tci := makeTCInfo(gblScope)
	initLateBIFunctions(tci)
	prInline = false // ensure runtime dbgprints don't waste time...
	prOptimize = false
	prRewrite = false
	prCleanup = false
	errors := 0
	names0, _ := filepath.Glob("tests/*.d8m")
	names1, _ := filepath.Glob("tests/*.go")
	names2, _ := filepath.Glob("tests/*.out")
	fmt.Println("found ", len(names0), "tests:")
	startinx := 0 // for dbgg only
	wallstart := time.Now()
	for testinx := startinx; testinx < len(names0); testinx++ {
		if testinx != startinx {
			// we're going around; reset gbl scope and tci
			QIScope = make(map[string]*QIVal, 10)
			QIPathMap = make(map[string]*mdlOrPkg, 10)
			defConds.defConds = defConds.defConds[:0] // clear any waiting onconds
			gblScope = makeScope()
			gblScope.topscope = true
			inx := binsearchStrings(pkgdbNames, "fmt")
			pkgdb[inx].AddPkg("fmt", "fmt")
			gblScope.importPkgs = append(gblScope.importPkgs, "fmt")
			tci = makeTCInfo(gblScope)
			listTG.insts = listTG.insts[:6]
			for _, inst := range listTG.insts {
				typ := inst[len(inst)-1].(*Type)
				for _, mthdx := range typ.methods {
					mthdx.sym.plist.Delete("cachedInstances")
					// Smash previously synth'd stringify methods in things like list(integer)
					// because in a future QC they can reuse gensyms in an unanticipated way that causes the output to fail.
					if mthdx.sym.ident == "stringify" {
						mthdx.sym.binding.(*Funinst).body = nil
					}
				}
			}
			seen := make([]*Type, 0, 10)
			// Sanity check: vfy that all GTS are cleared in all list instances
			for i := 0; i < len(listTG.insts); i++ {
				inst := listTG.insts[i]
				typ := inst[len(inst)-1].(*Type)
				typ.checkGTS(&seen)
			}
			//for _, inst := range listTG.insts {
			//	typ := inst[len(inst)-1].(*Type)
			//	typ.checkGTS(&seen)
			//}
			gensymCtr = 0
		}
		fn := names0[testinx]
		basename := stripCruft(fn, "tests/", ".d8m")
		if stripCruft(names1[testinx], "tests/", ".go") != basename {
			fmt.Println("missing compiled output for test", basename)
			errors++
			continue
		}
		if stripCruft(names2[testinx], "tests/", ".out") != basename {
			fmt.Println("missing runtime output for test", basename)
			errors++
			continue
		}
		fmt.Print(basename, ": ")
		loadFile("preload.d8m", true, tci)
		fd, err := os.Open(fn)
		if err != nil {
			log.Fatal("file open error: ", err)
		}
		lineReader := bufio.NewScanner(fd)
		scanner := newScanner(lineReader, "modules/preload.d8m", 0, true)
		parser := newParser(scanner)
		for {
			nxterm := parser.stmtOrExpr()
			if nxterm == nil {
				log.Fatal("EOF reached")
			}
			tcout := nxterm.Typecheck(nil, tci)
			if len(tci.errors) > 0 {
				for _, errtrm := range tci.errors {
					fmt.Println("error:", errtrm.String())
				}
				tci.errors = tci.errors[0:0] // clear
				errors++
				break
			}
			if callingNamed(tcout, "load") {
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
				if strg == "" {
					fmt.Println("error: can't interpret arg to load command")
				} else {
					tcout = loadFile(strg, false, tci)
				}
			} else if compileableCheck(nxterm) {
				if callingNamed(nxterm, "compile") {
					ccall := tcout.(*TermTL)
					tcout = ccall.args[0]
					if len(ccall.args) > 1 {
						arg := ccall.args[1]
						badarg := true
						if arg.Tag() == TildeExpr {
							tt := arg.(*TermTT)
							if tt.arg0.Tag() == Labellit && tt.arg0.(*TermB).value == "unroll" && tt.arg1 == TrueLiteral {
								badarg = false
								unrollEachloops = true
							}
						}
						if len(ccall.args) > 2 {
							badarg = true
						}
						if badarg {
							fmt.Println("error: bad compile args")
							break
						}
					}
				}
				//fmt.Println("compiling ", tcout.String())
				qcout, qcerr := queryCompile(tcout, gblScope)
				fd.Close()
				unrollEachloops = false // Not sure this is right; cdriver goes the other way
				if qcerr != nil {
					fmt.Println("rewrite rule error:", qcerr.String())
					errors++
				} else {
					// Here, queryCompile finished with what it thinks is success.
					// If the result equals the .go file then it's certainly success, otherwise write to d8mqry.go
					// and call go compiler to check output.
					goOk := compareFiles(names1[testinx], qcout, "compile")
					if goOk {
						fmt.Println("...success")
						tolog.Println(basename, ": ...success")
					}
					if *flagRunAll || !goOk {
						f, err := os.Create("d8mqry.go")
						if err != nil {
							log.Fatal(err)
						}
						f.WriteString(qcout)
						f.Close()
						if !goOk && !*flagFix {
							errors++
							out, _ := exec.Command("/usr/bin/diff", "-bc", "d8mqry.go", names1[testinx]).CombinedOutput()
							tolog.Print(string(out))
						}
						out, _ := exec.Command("/usr/local/go/bin/go", "run", "d8mqry.go").CombinedOutput()
						if compareFiles(names2[testinx], string(out), "runtime") {
							fmt.Println("...but output comparison succeeds")
							tolog.Println("...but output comparison succeeds")
							if *flagFix {
								fmt.Println("running 'mv d8mqry.go ", names1[testinx], "'")
								rslt, _ := exec.Command("/bin/cp", "d8mqry.go", names1[testinx]).CombinedOutput()
								fmt.Println("rslt: ", string(rslt))
							}
						} else {
							tolog.Println("go compile+execute returns:", string(out))
						}
					}
				}
				break
			}
		}
	}
	if errors == 0 {
		f, err := os.Create("./.lasttest.txt")
		if err != nil {
			log.Fatal(err)
		}
		elapsed := time.Since(wallstart).Seconds()
		fmt.Println("no errors (writing date to ./.lasttest.txt); compiled", len(names0), "files in", fmt.Sprintf("%5.2f", elapsed), "seconds")
		fmt.Fprint(f, "last successful test at", time.Now(), "\n")
		f.Close()
	}
}
