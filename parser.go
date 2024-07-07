// The parser
package main

import (
	"bufio"
	"fmt"
	"strings"
)

func inStrings(strg string, strgs []string) bool {
	for _, s := range strgs {
		if strg == s {
			return true
		}
	}
	return false
}

// Hold state for a parser. Scanner constitutes static state, the other slots
// are dynamic. allowUnderscores is set by the caller as appropriate.
// Initially, provision is made for returning error terms but nothing fancy is
// attempted in the way of error recovery. Most likely, the right approach is for
// the caller to exit after any error.
type Parser struct {
	scanner          *Scanner
	lastToken        *Token
	backTokens       []*Token
	tokenlist        []*Token   // from scanner.GetTokens
	commentslist     [][]*Token // buffer comments until attachComments picks them up
	tokinx           int
	allowUnderscores bool
	eofSeen          bool
	errors           []Term
	inCase           int // count depth
	inExtend         int // count depth
}

// Create and return a Parser entity; all slots get default init values except Scanner.
func newParser(scnr *Scanner) *Parser {
	prsr := Parser{scnr, nil, []*Token{}, []*Token{}, [][]*Token{}, 0, false, false, []Term{}, 0, 0}
	return &prsr
}

func setupStringParser(strg string, gstarts bool) *Parser {
	scanner := newScanner(bufio.NewScanner(strings.NewReader(strg)), "", 0, gstarts)
	return newParser(scanner)
}

// helper for parsing strings in patterns and elsewhere
var parseString = func(strg string) Term {
	parser := setupStringParser(strg, false)
	parser.allowUnderscores = true
	return parser.stmtOrExpr()
}

// Return the next Token, calling the scanner for more if need be.
// Mark the token as consumed.
func (p *Parser) nxToken() *Token {
	nback := len(p.backTokens)
	if nback > 0 {
		tk := p.backTokens[nback-1]
		p.backTokens = p.backTokens[0 : nback-1]
		return tk
	}
	for {
		if p.tokenlist == nil || p.tokinx >= len(p.tokenlist) {
			tmp, err := p.scanner.GetTokens()
			if err != nil || len(tmp) == 0 {
				p.eofSeen = true
				return nil
			}
			p.tokenlist = tmp
			p.tokinx = 0
		}
		tk := p.tokenlist[p.tokinx]
		if tk.code != EOF { // EOF token is sticky
			p.tokinx++
		}
		if tk.code == COMMENT || tk.code == LINECOMMENT {
			level := len(p.commentslist) - 1
			curcmts := p.commentslist[level]
			p.commentslist[level] = append(curcmts, tk)
		} else {
			p.lastToken = tk
			return tk
		}
	}
}

// Return the Token last provided by either peekToken or nxToken.
// Must not be called when there is no active Token.
func (p *Parser) curToken() *Token {
	return p.lastToken
}

// Return the next token but do not consume it.
func (p *Parser) peekToken() *Token {
	tk := p.nxToken()
	p.pushToken(tk)
	return tk
}

// Return this token to the "unconsumed" list.
func (p *Parser) pushToken(tk *Token) {
	p.backTokens = append(p.backTokens, tk)
}

// This should be called at whatever granularity is desired (probably per
// statement) to attach accumulated comment tokens to the given Term.
func (p *Parser) attachComments(trm Term) {
	level := len(p.commentslist) - 1
	curcmts := p.commentslist[level]
	if len(curcmts) > 0 {
		if len(curcmts) > 1 {
			trm.Plist().Add("comments", curcmts)
		} else {
			trm.Plist().Add("comments", curcmts[0])
		}
		p.commentslist[level] = []*Token{}
	}
}

func (p *Parser) enter4comments() {
	p.commentslist = append(p.commentslist, []*Token{})
}

func (p *Parser) exit4comments() {
	p.commentslist = p.commentslist[0 : len(p.commentslist)-1]
}

// Create and return an error term, also storing it in errors list.
func (p *Parser) recordError(msg string, tkn *Token) Term {
	tkpos := Pos(-1)
	if tkn != nil {
		tkpos = tkn.pos
	} else if len(*p.scanner.tokbufp) > 0 {
		tkpos = (*p.scanner.tokbufp)[0].pos
	}
	trm := makeTermB(ErrorTag, msg, tkpos)
	if tkpos != Pos(-1) {
		trm.plist.Add("linechar", p.scanner.getLineCharInfo(tkpos.byteIndex(), p.scanner.fname))
	}
	p.errors = append(p.errors, trm)
	return trm
}

// Policy function for how many errors causes exit. This can be checked
// at whatever granularity, for example, on statement and expression starts.
func (p *Parser) bailOnErrors() bool {
	return len(p.errors) > 0
}

// Tells if next token will cause a new line to be fetched.
// Must not be called until the first line has been fetched.
func (p *Parser) atEOL() bool {
	tokcnt := len(p.tokenlist)
	if tokcnt > 0 && p.tokenlist[tokcnt-1].code == LINECOMMENT {
		tokcnt--
	}
	return p.tokinx == tokcnt && len(p.backTokens) == 0
}

// Record an error unless the next token matches the given code.
// Works entirely via this side effect.
// This may only be used for punctuation tokens.
func (p *Parser) must(tc TokenCode) {
	tk := p.nxToken()
	if tk.code != tc {
		msg := fmt.Sprintf("expected %s but saw %s", opstringMap[tc], tk.String())
		p.recordError(msg, tk)
	}
}

// Helper for stmtStart ftn (defined next). This assumes the relevant TokenCodes
// are ordered (as ints) so we can binsearch them.
var sstartCodes = []int{int(ASSERT), int(BREAK), int(CONTINUE), int(EACH),
	int(EXPORT), int(GIVEN), int(IF), int(IMPORT), int(LOOP), int(ONCONDITION),
	int(RETURN), int(STMAP), int(UNLESS), int(VAL), int(VAR), int(WHILE)}

// Help decide if scanner is at a stmt start.
func stmtStart(tc TokenCode) bool {
	inx := binsearchInts(sstartCodes, int(tc))
	return inx < len(sstartCodes) && sstartCodes[inx] == int(tc)
}

// Parse a statement or expression. Return nil on EOF, else a *Term.
// First, consume any newlines and semicolons, then figure out stmt or expr
// based on next token. Call appropriate production based on that.
func (p *Parser) stmtOrExpr() Term {
	var pktk *Token
	p.enter4comments()
	defer p.exit4comments()
	for {
		pktk = p.peekToken()
		if pktk.code == EOF {
			return nil
		}
		if pktk.code != SEMICOLON {
			break
		}
		p.nxToken()
	}
	// at this point, pktk is the next token which is not consumed.
	if keyword_beg < pktk.code && pktk.code < keyword_end && stmtStart(pktk.code) {
		return p.stmt()
	} else if p.inExtend > 0 && pktk.code == IDENT && inStrings(pktk.val, []string{"attribute", "method", "private"}) {
		return p.stmt()
	} else if pktk.code == LBRACE {
		return p.stmts(false, RBRACE)
	} else if pktk.code == RBRACE {
		// This handles "stmt; }", ie a null stmt before RBRACE
		return nil
	} else {
		return p.expr(0)
	}
}

// Should only be called if scanner is at a statement start. Handle all stmt cases.
func (p *Parser) stmt() Term {
	if p.bailOnErrors() {
		return p.errors[0]
	}
	for {
		tk := p.peekToken()
		if tk.code != SEMICOLON {
			break
		}
		tk = p.nxToken()
	}
	headtoken := p.nxToken()
	var retval Term
	switch headtoken.code {
	case EOF:
		return nil
	case ASSERT:
		arg := p.expr(0)
		retval = makeTermT(AssertStmt, arg, nil, headtoken.pos, arg.Final())
	case IDENT:
		if p.inExtend > 0 && inStrings(headtoken.val, []string{"attribute", "method", "private"}) {
			retval = p.attribOrMethod(headtoken)
		} else {
			panic("internal: stmt code")
		}
	case BREAK, CONTINUE:
		tg := BreakStmt
		if headtoken.code == CONTINUE {
			tg = ContinueStmt
		}
		retval = makeTermT(tg, nil, nil, headtoken.pos, headtoken.pos.plus(len(headtoken.String())))
	case EACH:
		a0 := p.exprlist(false)
		body := p.stmts(false, RBRACE)
		retval = makeScopedTermL(EachStmt, nil, append(a0, body), nil, headtoken.pos, body.Final())
	case EXPORT:
		symlist := p.typedSymList(false)
		lastsym := symlist[len(symlist)-1]
		retval = makeTermL(ExportStmt, symlist, nil, headtoken.pos, lastsym.Final())
	case GIVEN:
		p.must(LPAREN)
		arg := p.symdeclList(GIVEN)
		p.must(RPAREN)
		body := p.stmts(false, RBRACE)
		retval = makeTermTL(GivenStmt, body, arg, nil, headtoken.pos, body.Final())
	case IF, UNLESS:
		pktk := p.peekToken()
		p.must(LPAREN)
		cond := p.expr(0)
		p.must(RPAREN)
		tpart := p.stmts(false, RBRACE)
		if headtoken.code == UNLESS {
			// do the rewrite right now!
			cond = makeTermT(Lognot, cond, nil, cond.First(), cond.Final())
		}
		args := []Term{cond, tpart}
		pktk = p.peekToken()
		lastpos := args[1].Final()
		if pktk != nil && pktk.code == ELSE {
			p.nxToken()
			args = append(args, p.stmts(false, RBRACE))
			lastpos = args[2].Final()
		}
		retval = makeTermL(IfStmt, args, nil, headtoken.pos, lastpos)
	case IMPORT:
		retval = p.parseImport(headtoken.pos)
	case LOOP:
		body := p.stmts(false, RBRACE)
		retval = makeTermT(LoopStmt, body, nil, headtoken.pos, body.Final())
	case ONCONDITION, ONTIME:
		tg := OncondStmt
		if headtoken.code == ONTIME {
			tg = OntimeStmt
		}
		cond := p.expr(0)
		var body Term
		if headtoken.code == ONCONDITION {
			body = p.stmts(false, RBRACE)
		} else {
			body = p.expr(0)
		}
		retval = makeTermTT(tg, cond, body, nil, headtoken.pos, body.Final())
	case RETURN:
		lastpos := headtoken.pos.plus(len(headtoken.String()))
		if p.atEOL() {
			return makeTermT(ReturnStmt, nil, nil, headtoken.pos, lastpos)
		}
		pktk := p.peekToken()
		if !(pktk.code == SEMICOLON || pktk.code == RBRACE) {
			retval = p.expr(0)
			retval = makeTermT(ReturnStmt, retval, nil, headtoken.pos, retval.Final())
		} else {
			retval = makeTermT(ReturnStmt, nil, nil, headtoken.pos, lastpos)
		}
	case STMAP:
		retval = p.parseStmap(headtoken.pos)
	case VAL, VAR:
		// I promised myself when working the backend of the ruby version of this compiler
		// that if I ever wrote another one, I would quickly split decls into 1 per stmt.
		// This eliminates an annoying source of code complexity that has zero value add
		// (below source level). It's a little tricky to do though. I've chosen to do it
		// by rewriting val+var that have multiple decls into a stmtlist (of the decls)
		// and ensure that std stmtlist processing flattens such things and that TC of
		// gbl decls can handle a stmtlist.
		//
		declist, err := p.bdgptlist(true)
		if err != nil {
			return err
		}
		tg := Valdecl
		if headtoken.code == VAR {
			tg = Vardecl
		}
		var lastpos Pos
		ndecls := len(declist)
		tmp := declist[ndecls-1].(*TermL)
		if len(tmp.args) == 3 && tmp.args[2] != nil {
			lastpos = tmp.args[2].Final()
		} else {
			lastpos = tmp.args[1].Final()
		}
		if ndecls > 1 {
			declstmts := make([]Term, ndecls)
			for i, a := range declist {
				declstmts[i] = makeTermT(tg, a, nil, headtoken.pos, lastpos)
			}
			retval = makeScopedTermL(Stmts, nil, declstmts, nil, headtoken.pos, lastpos)
		} else {
			retval = makeTermT(tg, declist[0], nil, headtoken.pos, lastpos)
		}
	case WHILE:
		pktk := p.peekToken()
		if pktk.code != LPAREN {
			return p.recordError("'(' expected after while", pktk)
		}
		cond := p.expr(0)
		body := p.stmts(false, RBRACE)
		retval = makeTermTT(WhileStmt, cond, body, nil, headtoken.pos, body.Final())
	}
	p.attachComments(retval)
	return retval
}

// parse a comma-separated list of the patterns symlist : type, and if bdgOk,
// either sym : type = bdg or sym = bdg. Unravel all symlists into one pair per sym.
// Use Symdecl tagged TermL's to hold all list elts. For "sym = bdg" pattern, the
// TermL arg will have [ident, nil, bdg] whereas for sym : type it will be [ident, type].
// To code error return (which wants to be Term), we use 2 return values; one will
// always be nil.
func (p *Parser) bdgptlist(bdgOk bool) ([]Term, Term) {
	ret := []Term{}
	for { // outer loop collects multiple comma-separated patterns
		nxtk := p.nxToken()
		if nxtk.code != IDENT {
			msg := fmt.Sprintf("unexpected token %s in bdgptlist", nxtk.String())
			return nil, p.recordError(msg, nxtk)
		}
		syms := []*Token{nxtk}
		// local ftn, called after peekToken sees '='
		var collectBdg = func() Term {
			p.nxToken()
			if len(syms) != 1 {
				return p.recordError("single symbol required before '='", nxtk)
			}
			return p.expr(0)
		}
		for { // inner loop collects symlist, if present, then adds 1 or more Terms to ret.
			nxtk := p.peekToken()
			if nxtk.code == EOF {
				return ret, nil
			}
			if nxtk.code == COLON {
				p.nxToken()
				typx := p.expr(5)
				lastpos := typx.Final()
				pktk := p.peekToken()
				var bdg Term
				if pktk != nil && pktk.code == EQ {
					bdg = collectBdg()
					if bdg.Tag() == ErrorTag {
						return nil, bdg
					}
				}
				// note that if bdg != nil, collectBdg has checked len(syms) == 1.
				for _, s := range syms {
					ret = append(ret, makeTermL(Symdecl, []Term{s, typx, bdg}, nil, s.pos, lastpos))
				}
			} else if nxtk.code == COMMA {
				p.nxToken()
				nxtk = p.nxToken()
				if nxtk.code != IDENT {
					msg := fmt.Sprintf("unexpected token %s in bdgptlist", nxtk.String())
					return nil, p.recordError(msg, nxtk)
				}
				syms = append(syms, nxtk)
				continue
			} else if nxtk.code == EQ && bdgOk {
				bdg := collectBdg()
				if bdg.Tag() == ErrorTag {
					return nil, bdg
				}
				s := syms[0]
				ret = append(ret, makeTermL(Symdecl, []Term{s, nil, bdg}, nil, s.pos, bdg.Final()))
			} else {
				msg := fmt.Sprintf("unexpected token %s in bdgptlist", nxtk.String())
				return nil, p.recordError(msg, nxtk)
			}
			// we only get here if we just processed ':' or '='
			// go around loop again if COMMA, else exit.
			if p.atEOL() {
				return ret, nil
			}
			nxtk = p.peekToken()
			if nxtk != nil && nxtk.code == COMMA {
				syms = []*Token{}
			} else {
				return ret, nil
			}
		}
	}
}

// Several uses for comma-separated list of optionally type-tagged identifiers:
// export, direct, typepred args. Each elt of the returned list is either a *Token
// or a Term tagged either Symdecl or SymdeclAttrib.
// Note that in lists handled by this method untyped symbols are left as is.
// In case of error, the last elt of the returned slice will be an Error term.
func (p *Parser) typedSymList(attribOk bool) []Term {
	pktk := p.peekToken()
	symlist := []Term{}
	for {
		if pktk.code != IDENT {
			symlist = append(symlist, p.recordError("expected identifier", pktk))
			return symlist
		}
		attribd := false
		sym := p.nxToken()
		if attribOk && sym.val == "attribute" {
			attribd = true
			sym = p.nxToken()
			if sym.code != IDENT {
				symlist = append(symlist, p.recordError("expected identifier", sym))
				return symlist
			}
		}
		pktk := p.peekToken()
		if pktk.code == COLON {
			p.nxToken()
			typx := p.expr(0)
			tg := Symdecl
			if attribd {
				tg = SymdeclAttrib
			}
			symdecl := makeTermL(tg, []Term{sym, typx}, nil, sym.pos, typx.Final())
			symlist = append(symlist, symdecl)
		} else if attribOk {
			symlist = append(symlist, p.recordError("each affordance must have a type declaration", pktk))
			return symlist
		} else {
			symlist = append(symlist, sym)
		}
		pktk = p.peekToken()
		if pktk.code == COMMA {
			p.nxToken()
		} else {
			return symlist
		}
	}
}

// This variant of typedSymList handles different cases, specifically the bdgpt
// decls in ortype and tuple type expressions and the decl part of given stmts.
// The given decl generally has some DBLCOLON separators, while tuple admits
// the private keyword. Hence the tokencode arg, it will be one of these 3 things.
// The output Symdecls are pairs or triples depending on the code. For given and
// tuple, we store the Symdecls as triples where the third elt tells about either
// single vs double colon, or private vs not.
func (p *Parser) symdeclList(tkcode TokenCode) []Term {
	symlist := []Term{}
	pvt := false
	// each loop pass collects one optionally typed symbol; if what follows is a comma
	// we loop around again, else change all symbols (tag=Ident) to typed symbols (tag=Symdecl).
	for {
		pktk := p.peekToken()
		if tkcode == TUPLE && pktk.val == "private" {
			pvt = true
			p.nxToken()
			pktk = p.peekToken()
		}
		if pktk.code != IDENT {
			symlist = append(symlist, p.recordError("expected identifier", pktk))
			return symlist
		}
		sym := Term(p.nxToken())
		pktk = p.peekToken()
		if pktk.code == COLON || pktk.code == DBLCOLON {
			p.nxToken()
			if pktk.code == DBLCOLON && tkcode != GIVEN {
				symlist = append(symlist, p.recordError("'::' is not allowed here", pktk))
				return symlist
			}
			typx := p.expr(0)
			trmlst := []Term{sym, typx}
			if tkcode == GIVEN {
				isDbl := TwoColon
				if pktk.code == COLON {
					isDbl = OneColon
				}
				trmlst = append(trmlst, isDbl)
			} else if tkcode == TUPLE {
				isPvt := FalseLiteral
				if pvt {
					isPvt = TrueLiteral
				}
				trmlst = append(trmlst, isPvt)
			}
			sym = makeTermL(Symdecl, trmlst, nil, sym.First(), typx.Final())
			pvt = false
		}
		symlist = append(symlist, sym)
		pktk = p.peekToken()
		if pktk.code == COMMA {
			p.nxToken()
		} else {
			// we're done collecting; do fixups for cases where multiple symbols for same type/tpred.
			var trmn *TermL
			for i := len(symlist) - 1; i >= 0; i -= 1 {
				decl := symlist[i]
				if decl.Tag() == Symdecl {
					trmn = decl.(*TermL)
				} else { // can only be IDENT
					if trmn == nil || trmn.args[1] == nil {
						return []Term{p.recordError("missing type declaration", decl.(*Token))}
					}
					typx := trmn.args[1]
					cpylst := []Term{decl, typx}
					if len(trmn.args) > 2 {
						cpylst = append(cpylst, trmn.args[2])
					}
					symlist[i] = makeTermL(Symdecl, cpylst, nil, decl.First(), typx.Final())
				}
			}
			return symlist
		}
	}
}

// We've seen the "STMap" keyword; parse the rest here and return a term with
// StmapStmt tag or else error. The internal form is a single list whose first
// two elts are src and tgt, then optional direct stmt (tag=DirectStmt), then
// xltrules.
func (p *Parser) parseStmap(pos0 Pos) Term {
	p.must(LBRACE)
	tmp := p.nxToken()
	if tmp.code != IDENT || tmp.val != "source" {
		return p.recordError("expected 'source'", tmp)
	}
	p.must(EQ)
	src := p.expr(0)
	tmp = p.nxToken()
	if tmp.code != IDENT || tmp.val != "target" {
		return p.recordError("expected 'target'", tmp)
	}
	p.must(EQ)
	tgt := p.expr(0)
	args := []Term{src, tgt} // first args to a TermL; augment with direct and/or xltrules
	tmp = p.peekToken()
	if tmp != nil && tmp.code == IDENT && tmp.val == "direct" {
		p.nxToken()
		dirlst := p.typedSymList(false)
		args = append(args, makeTermL(DirectStmt, dirlst, nil, tmp.pos, dirlst[len(dirlst)-1].Final()))
	}
	for {
		pktk := p.peekToken()
		if pktk.code == RBRACE {
			p.nxToken()
			break
		}
		xltr := p.expr(0)
		if xltr.Tag() != Arrow {
			// todo: improve this errmsg
			return p.recordError("xltrule has wrong form", pktk)
		}
		args = append(args, xltr)
	}
	return makeTermL(StmapStmt, args, nil, pos0, args[len(args)-1].Final())
}

// We've seen the "import" keyword; parse the rest here and return a term with
// either ImportStmt or ImportPkgStmt (or an error Term)
func (p *Parser) parseImport(pos0 Pos) Term {
	tmp := p.nxToken()
	tg := ImportStmt
	if tmp.code == IDENT && tmp.val == "go" {
		tg = ImportPkgStmt
		tmp = p.nxToken()
	}
	if !tmp.AnyString() {
		return p.recordError("import filename must be a string", tmp)
	}
	lastpos := tmp.Final()
	pathname := tmp
	inx := strings.LastIndexByte(pathname.val, '/')
	dfltnm := pathname.val
	if inx > 0 {
		dfltnm = pathname.val[inx+1 : len(pathname.val)]
	}
	module := &Token{IDENT, pathname.pos, dfltnm}
	if !p.atEOL() {
		pktk := p.peekToken()
		if pktk.code == IDENT {
			if pktk.val == "as" {
				p.nxToken()
				tmp = p.nxToken()
				if !(tmp.code == IDENT) {
					return p.recordError("in import, 'as' must be followed by an identifier", tmp)
				}
				module = tmp
			} else if pktk.val == "melted" {
				p.nxToken()
				module.val = ""
			}
		}
	}
	return makeTermTT(tg, pathname, module, nil, pos0, lastpos)
}

// Method called when we see private, attribute, or method in stmt cxt. At that
// point, private can only prefix attribute or method stmts.
func (p *Parser) attribOrMethod(tkn0 *Token) Term {
	pos0 := tkn0.First()
	pvt := tkn0.val == "private"
	if pvt {
		tkn0 = p.nxToken()
		if !(tkn0.val == "method" || tkn0.val == "attribute") {
			return p.recordError("expected 'method' or 'attribute'", tkn0)
		}
	}
	// now pvt is set and tkn0 is the introductory keyword.
	tmp := int(Attribdecl)
	if tkn0.val == "method" {
		tmp += 2
	}
	if pvt {
		tmp += 1
	}
	tg := Termtag(tmp)
	// Although multiple comma-separated method defns is unexpected, it's legal stx.
	defns, err := p.bdgptlist(tkn0.val == "method")
	if err != nil {
		return err
	}
	return makeTermL(tg, defns, nil, pos0, defns[len(defns)-1].Final())
}

// D8m's case stmt/expr has 2 forms: with a gbl arg it's like go's typeswitch but
// with each clause syntax being "lbllist => rhs" or "else => rhs" where lbllist
// is a comma-separated list of label literals, corresponding to the tags of an
// ortype. Otherwise, it's like a stack of if-then-else, each clause written as
// cond => thenpart. Note that the else keyword is not useable in the second form.
// Internally, we code Ifcase as TermL with arrow expr reqmt not checked here (so must
// check in TC). However, Typecase codes as TermTL (with ortype expr in trm slot) and does
// check for arrow exprs.
func (p *Parser) parseCase(hdtkn *Token) Term {
	pktk := p.peekToken()
	if pktk.code == LBRACE {
		p.inCase += 1
		clauses := p.stmts(false, RBRACE)
		if clauses.Tag() != ErrorTag {
			clz := clauses.(*TermL)
			clz.kind = Ifcase
			clz.first = hdtkn.pos
		}
		p.inCase -= 1
		return clauses
	}
	// here for the Typecase form
	ortval := p.expr(0)
	p.must(LBRACE)
	p.inCase += 1
	clauses := p.stmts(true, RBRACE)
	p.inCase -= 1
	if clauses.Tag() == ErrorTag {
		return clauses
	}
	clauselist := clauses.(*TermL).args
	return makeTermTL(Typecase, ortval, clauselist, nil, hdtkn.pos, clauselist[len(clauselist)-1].Final())
}

// When the parser sees \ char outside of a string literal, it doesn't know whether
// it's about to parse a ftn type or a Funlit. In the worst case, it won't know until
// it reaches the end of the signature. At that point, if it's seen no ':', it can
// only be a ftn type. As soon as it sees a ':' it knows it's Funlit. As soon a it
// sees a pre-':' expr more complicated than an identifier, it knows it's not a
// Funlit. After the signature, parsing is trivial.
//
// The format of both is TermL tagged as either FunLit or FunTypeLit. For Funlit, the args consist
// of the funargs followed by 3 more: the rettype (often nil), the purity word, and the body. The funargs
// are always Symdecl tagged TermL's with 3 or 4 args: the identifier, type, a "colon indicator" (a
// synthesized Token with value either "__single" or "__double"), and optionally, the expression for
// default value.
// For FunTypeLit, they consist of type exprs for args and rettype followed by a token with the purity word.
func (p *Parser) parseLambda(hdtkn *Token) Term {
	purityWords := map[string]int{"pure": 1, "mod": 2, "imp": 3, "xprftn": 4, "typegen": 5}
	pktk := p.peekToken()
	purwd := "pure"
	pwd := 1
	if pktk.code == IDENT {
		purwd = pktk.val
		pwd = purityWords[purwd]
		if pwd == 0 {
			msg := fmt.Sprintf("illegal purity word %s", pktk.val)
			return p.recordError(msg, pktk)
		}
		p.nxToken()
	}
	p.must(LPAREN)
	isFtnlit := 0       // 0:unknown, 1 is ftnlit, 2 is ftntype
	lastNakedIndex := 0 // keep track of comma-separated symbol lists
	if pwd > 2 {
		isFtnlit = 1
	} // has to be ftnlit unless pure or mod
	symlist := []Term{}
	syminx := 0
	for {
		pktk = p.nxToken()
		if pktk.code == RPAREN {
			break
		}
		if pktk.code == COMMA {
			pktk = p.nxToken()
		}
		if pktk.code == TUPLE {
			// "tuple" has its own tokencode which is good for tuple(...) forms but not in "tt:tuple"
			pktk.code = IDENT
		}
		if pktk.code != IDENT {
			return p.recordError("identifier expected", pktk)
		}
		sym := pktk
		pktk = p.nxToken() // type expr unless one of COMMA, RPAREN, COLON, DBLCOLON
		switch pktk.code {
		case COMMA:
			symlist = append(symlist, sym)
			syminx += 1
		case RPAREN:
			if isFtnlit == 1 {
				return p.recordError("missing type", pktk)
			} else {
				symlist = append(symlist, sym)
			}
			p.pushToken(pktk) // catch on next goaround
		case EQ: // optional arg given without explicit type
			if lastNakedIndex < syminx {
				return p.recordError("optional arguments must be one at a time", pktk)
			}
			if isFtnlit == 2 {
				return p.recordError("optional arguments must be one at a time", pktk)
			}
			isFtnlit = 1
			dval := p.expr(0)
			nxtrm := makeTermL(Symdecl, []Term{sym, nil, OneColon, dval}, nil, sym.First(), dval.Final())
			symlist = append(symlist, nxtrm)
			syminx += 1
			lastNakedIndex = syminx
		case COLON, DBLCOLON:
			if isFtnlit == 2 {
				return p.recordError("unexpected symbol declaration", pktk)
			}
			isFtnlit = 1
			typx := p.expr(0)
			var colonsExpr Term // will be __single or __double Token, or expr for optional arg.
			colonsExpr = OneColon
			if pktk.code == DBLCOLON {
				colonsExpr = TwoColon
			}
			pktk = p.peekToken() // probably , or ) but could be =
			var optlvalue Term
			if pktk.code == EQ {
				if lastNakedIndex < syminx {
					return p.recordError("optional arguments must be one at a time", pktk)
				}
				p.nxToken()
				optlvalue = p.expr(0)
				pktk = p.peekToken()
			}
			for i := lastNakedIndex; i < syminx; i++ {
				sym2 := symlist[i]
				nxtrm := makeTermL(Symdecl, []Term{sym2, typx, colonsExpr}, nil, sym2.First(), typx.Final())
				symlist[i] = nxtrm
			}
			args := []Term{sym, typx, colonsExpr}
			if optlvalue != nil {
				args = append(args, optlvalue)
			}
			nxtrm := makeTermL(Symdecl, args, nil, sym.First(), typx.Final())
			symlist = append(symlist, nxtrm)
			syminx += 1
			lastNakedIndex = syminx
			if pktk.code == COMMA {
				p.nxToken()
			}
		default: // this means a type expr, which can only happen here in a FunTypeLit
			if isFtnlit == 1 {
				return p.recordError("unexpected expression", pktk)
			}
			isFtnlit = 2
			p.pushToken(pktk)
			p.pushToken(sym)
			typx := p.expr(0)
			symlist = append(symlist, typx)
		}
	}
	// at this point, we've collected the args and we know if ftnlit or ftnsig.
	// if ftnsig, the rettype is required, else optional. If ftnlit, a body is
	// required, else not possible.
	pktk = p.peekToken()
	var rettype Term
	if pktk.code == RETURNS {
		p.nxToken()
		rettype = p.expr(0)
		pktk = p.peekToken()
	}
	symlist = append(symlist, rettype, SynthToken(IDENT, purwd))
	if isFtnlit == 0 {
		if pktk.code == LBRACE {
			isFtnlit = 1
		} else {
			isFtnlit = 2
		}
	}
	if isFtnlit == 2 {
		if rettype == nil {
			return p.recordError("function type must have return type", pktk)
		}
		if pwd > 2 {
			return p.recordError("function type purity may only be 'pure' or 'mod'", pktk)
		}
		return makeTermL(FunTypeLit, symlist, nil, hdtkn.pos, rettype.Final())
	} else {
		body := p.stmts(false, RBRACE)
		if body.Tag() == ErrorTag {
			return body
		}
		symlist = append(symlist, body)
		return makeTermL(FunLit, symlist, nil, hdtkn.pos, body.Final())
	}
}

// Collect a stmtlist. The two args allow to handle different situations. The
// closer is usually RBRACE but can be RBRACK to handle multi-stmt case in dotbracket.
// If it's not RBRACE, lBraceSeen must be true.
func (p *Parser) stmts(lBraceSeen bool, closer TokenCode) Term {
	pktk := p.peekToken()
	if pktk.code == EOF {
		return p.recordError("unexpected eof", nil)
	}
	if pktk.code == RBRACE {
		return p.recordError("unexpected closing brace", nil)
	}
	pos0 := pktk.pos
	if !lBraceSeen {
		if pktk.code != LBRACE {
			single := p.stmtOrExpr()
			return makeScopedTermL(Stmts, nil, []Term{single}, nil, pos0, single.Final())
		} else {
			p.nxToken()
		}
	}
	stmtlist := []Term{}
	for {
		pktk2 := p.peekToken()
		if pktk2.code == EOF {
			return p.recordError("unexpected eof", nil)
		}
		if pktk2.code == closer {
			p.nxToken()
			return makeScopedTermL(Stmts, nil, stmtlist, nil, pos0, pktk2.Final())
		}
		s := p.stmtOrExpr()
		if s != nil {
			stg := s.Tag()
			if stg == ErrorTag {
				return s
			}
			// integrate stmtlists created for multi-decl val+var
			if stg == Stmts {
				stmtlist = append(stmtlist, s.(*TermL).args...)
			} else {
				stmtlist = append(stmtlist, s)
			}
		}
	}
}

// Parse a possibly empty expression list surrounded by parens.
func (p *Parser) exprlist(emptyOk bool) []Term {
	p.must(LPAREN)
	var xlst = []Term{}
	pktk := p.peekToken()
	if pktk.code == RPAREN {
		if emptyOk {
			p.nxToken()
		} else {
			xlst = append(xlst, p.recordError("missing expression", pktk))
		}
		return xlst
	}
	for {
		xpr := p.expr(0)
		xlst = append(xlst, xpr)
		pktk = p.peekToken()
		if pktk.code == COMMA {
			p.nxToken()
		} else {
			break
		}
	}
	p.must(RPAREN)
	return xlst
}

// Parse a litform. This is essentially exprlist without the parens, and with an
// optional type tag. Eat the RBRACK at the end and return the Litform Term.
func (p *Parser) sqbrack(pos0 Pos) Term {
	var ttag Term // will be nil in the returned Term if not explicitly given
	var xprlist = []Term{}
	pktk := p.peekToken()
	if pktk.code == RBRACK {
		p.nxToken()
		return makeTermTL(Litform, nil, xprlist, nil, pos0, pktk.pos)
	}
	xpr0 := p.expr(0)
	pktk = p.peekToken()
	if pktk.code == COLON {
		p.nxToken()
		pktk = p.peekToken()
		ttag = xpr0
		xpr0 = nil
	} else {
		xprlist = append(xprlist, xpr0)
	}
	// at top of loop, xpr0 is nil or the next expr, pktk is unconsumed next token
	for {
		if pktk.code == RBRACK {
			p.nxToken()
			return makeTermTL(Litform, ttag, xprlist, nil, pos0, pktk.pos)
		}
		if p.bailOnErrors() {
			return p.errors[0]
		}
		if xpr0 == nil {
			xpr0 = p.expr(0)
			xprlist = append(xprlist, xpr0)
			pktk = p.peekToken()
		}
		if pktk.code == COMMA {
			xpr0 = nil
			p.nxToken()
			pktk = p.peekToken()
		} else if pktk.code != RBRACK {
			msg := fmt.Sprintf("token %s not expected here", pktk.String())
			return p.recordError(msg, pktk)
		}
	}
}

// The following captures which TokenCodes are may cause a shift decision in the
// expression parser. Codes not present always reduce. Those present, reduce if
// their precedence is less than the precedence at which the parser is currently operating.
// The value slot contains both a precedence and the term tag to use when combining.
type scinfo struct {
	prec   int
	ttag   Termtag
	fnname string
}

var shiftableCode = map[TokenCode]*scinfo{
	TILDE:   &scinfo{1, TildeExpr, ""},
	ARROW:   &scinfo{1, Arrow, ""},
	TOKAT:   &scinfo{1, AtExpr, ""},
	PLUSEQ:  &scinfo{2, PluseqExpr, ""},
	MINUSEQ: &scinfo{2, MinuseqExpr, ""},
	MULEQ:   &scinfo{2, MuleqExpr, ""},
	DIVEQ:   &scinfo{2, DiveqExpr, ""},
	QUEST:   &scinfo{3, IfStmt, ""},
	ANDAND:  &scinfo{3, AndandExpr, ""},
	OROR:    &scinfo{3, OrorExpr, ""},
	TOKHAT:  &scinfo{3, HatExpr, ""},
	EQ:      &scinfo{2, AsgnStmt, ""},
	EQEQ:    &scinfo{4, EqExpr, ""},
	EQEQEQ:  &scinfo{4, EqeqExpr, ""},
	NEQ:     &scinfo{4, NeqExpr, ""},
	NEQEQ:   &scinfo{4, NeqeqExpr, ""},
	LSS:     &scinfo{5, Funcall, "<"},
	GTR:     &scinfo{5, Funcall, ">"},
	LEQ:     &scinfo{5, Funcall, "<="},
	GEQ:     &scinfo{5, Funcall, ">="},
	IN:      &scinfo{6, Funcall, "in"},
	DOTDOT:  &scinfo{7, DotdotExpr, ""},
	DOT3:    &scinfo{7, Dot3Expr, ""},
	TOKPLUS: &scinfo{8, Funcall, "+"},
	TOKSUB:  &scinfo{8, Funcall, "-"},
	TOKMUL:  &scinfo{9, Funcall, "*"},
	TOKDIV:  &scinfo{9, Funcall, "/"},
	TOKREM:  &scinfo{9, Funcall, "%"},
	LPAREN:  &scinfo{10, Funcall, ""},
	LBRACK:  &scinfo{10, IndexExpr, ""},
	DOT:     &scinfo{11, Dot, ""},
}

// Start of expression oriented parsing. Expr method takes a precedence arg.
func (p *Parser) expr(prec int) Term {
	nxtk := p.nxToken()
	var xpr Term
	switch nxtk.code {
	case IDENT:
		if nxtk.val == "true" {
			xpr = TrueLiteral
		} else if nxtk.val == "false" {
			xpr = FalseLiteral
		} else {
			xpr = nxtk
		}
	case IDENTUU:
		if p.allowUnderscores {
			xpr = nxtk
		} else {
			xpr = p.recordError("identifiers starting with __ are not allowed here", nxtk)
		}
	case ILLEGAL:
		msg := fmt.Sprintf("illegal token: %s", nxtk.val)
		xpr = p.recordError(msg, nxtk)
	case INT, HEX:
		xpr = makeIntTerm(nxtk.val, nxtk.pos)
	case FLOAT:
		xpr = makeFloatTerm(nxtk.val, nxtk.pos)
	case CHAR:
		xpr = makeTermB(Bytelit, nxtk.val, nxtk.pos)
	case STRING, RAWSTRING:
		xpr = makeStringTerm(nxtk.val, nxtk.pos, nxtk.code == RAWSTRING)
	case LABEL:
		xpr = makeLabelTerm(nxtk.val[1:], nxtk.pos)
	case LPAREN:
		xpr = p.expr(0)
		p.must(RPAREN)
	case LBRACK: // here, must be litform
		xpr = p.sqbrack(nxtk.pos)
	case TOKSUB: // here, unary minus
		xpr = p.expr(8)
		if xpr.Tag() == Intlit || xpr.Tag() == Floatlit {
			bt := xpr.(*TermB)
			bt.first = bt.first.setIndex(nxtk.pos.byteIndex())
			if bt.kind == Intlit {
				valprop := (&bt.plist).Find("intval").(int)
				(&bt.plist).Add("intval", -valprop)
			} else { // assume Floatlit
				valprop := (&bt.plist).Find("floatval").(float64)
				(&bt.plist).Add("floatval", -valprop)
			}
			bt.value = "-" + bt.value
		} else {
			xpr = makeTermT(Negate, xpr, nil, nxtk.pos, xpr.Final())
		}
	case TOKNOT:
		xpr = p.expr(8)
		xpr = makeTermT(Lognot, xpr, nil, nxtk.pos, xpr.Final())
	case ELSE: // start keywords here
		if p.inCase > 0 {
			xpr = nxtk // formally a *Term; specially noted by case
		} else {
			xpr = p.recordError("cannot use 'else' here", nxtk)
		}
	case EXTEND:
		basetype := p.expr(8)
		p.inExtend += 1
		where := p.nxToken()
		if where.code != IDENT && where.val != "where" {
			xpr = p.recordError("where clause required for extend", where)
		} else {
			body := p.stmts(false, RBRACE)
			if body.Tag() == ErrorTag {
				return body
			}
			xpr = makeTermTT(ExtendExpr, basetype, body, nil, nxtk.pos, where.Final())
		}
		p.inExtend -= 1
	case TYPEPRED:
		pos0 := nxtk.pos
		p.must(LPAREN)
		nxtk = p.nxToken()
		if nxtk.code != IDENT {
			xpr = p.recordError("basepred declaration missing", nxtk)
		} else {
			p.must(DBLCOLON)
			typx := p.expr(0)
			basepred := makeTermL(Symdecl, []Term{nxtk, typx}, nil, nxtk.pos, typx.Final())
			args := []Term{basepred}
			p.must(COMMA)
			affs := p.typedSymList(true)
			args = append(args, affs...)
			xpr = makeTermL(TypepredExpr, args, nil, pos0, args[len(args)-1].Final())
		}
		p.must(RPAREN)
	case CASE:
		xpr = p.parseCase(nxtk)
	case MAP:
		xlst := p.exprlist(false)
		body := p.stmts(false, RBRACE)
		xpr = makeTermL(MapExpr, append(xlst, body), nil, nxtk.pos, body.Final())
	case ORTYPE, TUPLE:
		tg := TupleTypeLit
		if nxtk.code == ORTYPE {
			tg = OrtypeLit
		}
		pktk := p.peekToken()
		if pktk.code == LPAREN {
			pos0 := nxtk.pos
			p.must(LPAREN)
			decls := p.symdeclList(nxtk.code)
			pktk := p.peekToken()
			p.must(RPAREN)
			fnl := pktk.Final()
			xpr = makeTermL(tg, decls, nil, pos0, fnl)
		} else {
			xpr = nxtk // return as standalone token, eg for T::tuple.
		}
	case LAMBDA:
		xpr = p.parseLambda(nxtk)
	default:
		msg := fmt.Sprintf("unexpected token %s in expression", nxtk.String())
		xpr = p.recordError(msg, nxtk)
		return xpr
	}
	// at this point, xpr represents the first part of the expression. Decide
	// whether to shift or reduce. Note that we can shift multiple times, collecting
	// partial results in xpr. Hence the loop.
	for {
		if p.bailOnErrors() {
			return p.errors[0]
		}
		// reduce unless combining opr is on same line
		if p.atEOL() {
			return xpr
		}
		pktk := p.peekToken()
		scinf := shiftableCode[pktk.code] // returns nil for not shiftable, else a precedence
		midprec := 0
		tg := NoTerm
		if scinf != nil {
			midprec = scinf.prec
			tg = scinf.ttag
		}
		// the prec != 4 thing invokes associativity.
		if midprec == 0 || midprec < prec || (midprec == prec && prec != 4) {
			return xpr
		}
		var rhs Term
		p.nxToken() // skip past, result is still in pktk
		switch pktk.code {
		case LPAREN: // at this point it means funcall
			p.pushToken(pktk)
			xprtg := xpr.Tag()
			if xprtg == Ident || xprtg == Dot || xprtg == FunLit {
				arglist := p.exprlist(true)
				lastpos := xpr.Final().plus(2)
				if len(arglist) > 0 {
					lastpos = arglist[len(arglist)-1].Final()
				}
				xpr = makeTermTL(tg, xpr, arglist, nil, xpr.First(), lastpos)
			} else {
				return xpr
			}
		case LBRACK:
			rhs = p.expr(0)
			p.must(RBRACK)
			xpr = makeTermTT(tg, xpr, rhs, nil, pktk.pos, p.curToken().pos)
		case DOT: // this includes dotbrace and dotbracket
			pktk = p.peekToken()
			if pktk.code == LBRACE || pktk.code == LBRACK {
				p.nxToken()
				want := RBRACE
				tg = Dotbrace
				if pktk.code == LBRACK {
					want = RBRACK
					tg = Dotbracket
				}
				rhs = p.stmts(true, want)
				if rhs.Tag() == ErrorTag {
					return rhs
				}
				if rhs.Tag() == Stmts && len(rhs.(*TermL).args) == 1 {
					rhs = rhs.(*TermL).args[0]
				}
				xpr = makeTermTT(tg, xpr, rhs, nil, xpr.First(), rhs.Final())
			} else if pktk.code == IDENT {
				p.nxToken()
				xpr = makeTermTT(tg, xpr, pktk, nil, xpr.First(), pktk.Final())
			} else {
				xpr = p.recordError("symbol must follow '.'", nxtk)
			}
		case ARROW:
			if p.inCase > 0 && p.peekToken().code == LBRACE {
				rhs = p.stmts(false, RBRACE)
			} else {
				rhs = p.expr(midprec)
			}
			xpr = makeTermTT(tg, xpr, rhs, nil, xpr.First(), rhs.Final())
		case IN:
			// note reversal of arg order, so this can look like a list method
			rhs := p.expr(midprec)
			xpr = makeTermTL(tg, SynthToken(IDENT, "in"), []Term{rhs, xpr}, nil, xpr.First(), rhs.Final())
		case QUEST:
			rhs = p.expr(0)
			p.must(COLON)
			epart := p.expr(midprec)
			rhs0 := makeScopedTermL(Stmts, nil, []Term{rhs}, nil, rhs.First(), rhs.Final())
			epart0 := makeScopedTermL(Stmts, nil, []Term{epart}, nil, epart.First(), epart.Final())
			xpr = makeTermL(IfStmt, []Term{xpr, rhs0, epart0}, nil, xpr.First(), epart.Final())
		case ANDAND, OROR:
			ttag := AndandExpr
			if pktk.code == OROR {
				ttag = OrorExpr
			}
			rhs := p.expr(midprec)
			if xpr.Tag() == ttag {
				xpr0 := xpr.(*TermL)
				xpr0.args = append(xpr0.args, rhs)
			} else {
				xpr = makeTermL(ttag, []Term{xpr, rhs}, nil, xpr.First(), rhs.Final())
			}
		default:
			rhs := p.expr(midprec)
			if scinf.fnname != "" {
				fnnmtk := SynthToken(IDENT, scinf.fnname)
				xpr = makeTermTL(tg, fnnmtk, []Term{xpr, rhs}, nil, xpr.First(), rhs.Final())
			} else {
				xpr = makeTermTT(tg, xpr, rhs, nil, xpr.First(), rhs.Final())
			}

		}
	}
}
