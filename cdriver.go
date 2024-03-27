// driver for standalone d8m compiler

package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	//"runtime/pprof"
	"strings"
)

var flagOutfile = flag.String("o", "", "output file name")
var flagDbg = flag.Bool("d", false, "debug mode (if true, format .go file else delete it)")
var flagLog = flag.String("log", "", "log file if present")
var flagLogFlags = flag.Int("f", 0, "bitset of log flags (inline, cleanup, optimize, rewrite, timing)")
var flagAddmod = flag.String("addmod", "", "if present, should be comma separated path,loc pair for modulesDB; program exits after")

var logger *log.Logger

func main() {
	gblScope.topscope = true
	tci := makeTCInfo(gblScope)
	initLateBIFunctions(tci)
	preloadOk := loadFile("preload.d8m", true, tci)
	if preloadOk.Tag() == ErrorTag {
		log.Fatal("error in preload")
	}
	/*
		proffd, proferr := os.Create("cwebgen.prof")
		if proferr != nil {
			log.Fatal(proferr)
		}
			pprof.StartCPUProfile(proffd)
			defer func() {
				pprof.StopCPUProfile()
				proffd.Close()
			}()
	*/

	flag.Parse()
	unrollEachloops = true
	if *flagAddmod != "" {
		pathloc := strings.Split(*flagAddmod, ",")
		if len(pathloc) != 2 {
			log.Fatal("addmod flag expected comma separated pair")
		}
		addModuleDB(pathloc[0], pathloc[1])
		return
	}
	if *flagLog == "" {
		var dbgfile, _ = os.Create("/dev/null")
		logger = log.New(dbgfile, "", 0)
	} else {
		var dbgfile, _ = os.Create(*flagLog)
		logger = log.New(dbgfile, "", 0)
	}
	prInline = false
	prCleanup = false
	prOptimize = false
	prRewrite = false
	prTiming = false
	if *flagLogFlags&1 != 0 {
		prInline = true
	}
	if *flagLogFlags&2 != 0 {
		prCleanup = true
	}
	if *flagLogFlags&4 != 0 {
		prOptimize = true
	}
	if *flagLogFlags&8 != 0 {
		prRewrite = true
	}
	if *flagLogFlags&16 != 0 {
		prTiming = true
	}
	args := flag.Args()
	filecount := len(args)
	if filecount == 0 {
		log.Fatal("nothing to compile")
	}
	// The first file is always considered to be the "main" one. So if there are multiple files, load the others first.
	for _, fn := range args[1:] {
		loadFile(fn, false, tci)
		if len(tci.errors) > 0 {
			break
		}
	}
	// The "main" file can end with an explicit query or the defn of the zero arg ftn that acts as the main ftn (in the go sense)
	tci.queryOk = true
	fmt.Println("loading main file:", args[0])
	qryterm := loadFile(args[0], false, tci)
	qtstrg := qryterm.String()
	qtstop := len(qtstrg)
	nl0 := strings.IndexByte(qtstrg, '\n')
	if nl0 > 0 {
		qtstop = nl0
	}
	fmt.Println("returns ", qtstrg[0:qtstop])
	if qryterm.Tag() == Valdecl {
		defined := qryterm.(*TermT).arg0.(*Symbol)
		if defined.dtype != nil && defined.dtype.family == TFFtn && len(defined.dtype.v.(*Ftntype).fmlargs) == 0 {
			qryterm = makeFuncall(defined, []Term{}, TypeNothing, nil)
		}
		fmt.Println("synthg qryterm as", qryterm.String())
	} else if qryterm.Tag() == ErrorTag {
		os.Exit(1)
	}

	if len(tci.errors) == 0 {
		compiled, qcError := queryCompile(qryterm, gblScope)
		if qcError != nil {
			fmt.Println("compile error:", qcError.String())
			os.Exit(1)
		} else {
			// compile succeeded, get or generate output filename
			outfilename := ""
			if *flagOutfile != "" {
				outfilename = *flagOutfile
			} else {
				nfn := len(args[0])
				if nfn > 4 && args[0][nfn-4:nfn] == ".d8m" {
					outfilename = args[0][:nfn-4]
				} else {
					outfilename = args[0]
				}
			}
			outfilename += ".go"
			// remove blank lines
			tmp := make([]byte, len(compiled))
			compiledLB := []byte(compiled)
			tmpinx := 0
			for i := 0; i < len(tmp); {
				j := bytes.IndexByte(compiledLB, '\n')
				if j == -1 {
					copy(tmp[tmpinx:], compiledLB)
					tmp = tmp[:tmpinx+len(compiledLB)]
					break
				}
				j += 1
				copy(tmp[tmpinx:], compiledLB[:j])
				tmpinx += j
				compiledLB = compiledLB[j:]
				for len(compiledLB) > 0 && compiledLB[0] == '\n' {
					compiledLB = compiledLB[1:]
				}
			}
			compiled = string(tmp)
			f, err := os.Create(outfilename)
			if err != nil {
				log.Fatal(err)
			}
			f.WriteString(compiled)
			f.Close()
			if *flagDbg {
				// gofmt before go build so that line numbers line up
				exec.Command("/usr/local/go/bin/gofmt", "-r", `(a)->a`, "-w", outfilename).CombinedOutput()
			}
			out, _ := exec.Command("/usr/local/go/bin/go", "build", outfilename).CombinedOutput()
			outstrg := string(out)
			if len(outstrg) <= 1 {
				if !*flagDbg {
					_, rmerr := exec.Command("rm", outfilename).CombinedOutput()
					if rmerr != nil {
						fmt.Println("rmerr:", rmerr)
					}
				}
			} else {
				fmt.Println("result: ", outstrg)
			}
		}

	}
}
