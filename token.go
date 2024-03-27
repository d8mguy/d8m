// tokenizer, heavily adapted from the token package of go

package main

import (
	"bufio"
	"bytes"
	"fmt"
)

// 20 bits of byte index in file, and above that, index into table of files read.
type Pos int64

func (p Pos) fileIndex() int       { return (int(p) >> 20) }
func (p Pos) byteIndex() int       { return int(p) & 0xfffff }
func (p Pos) plus(other int) Pos   { return Pos(int(p) + other) }
func (p Pos) setIndex(inx int) Pos { return Pos((int(p) &^ 0xfffff) | inx) }

// Token is the set of lexical tokens of the D8m programming language.
type TokenCode int

// All the tokens
const (
	// Special tokens
	ILLEGAL TokenCode = iota
	EOF
	COMMENT
	LINECOMMENT

	literal_beg
	// Identifiers and basic type literals
	// (these tokens stand for classes of literals)
	IDENT     // main
	IDENTUU   // An identifier starting with 2 underscores
	INT       // 12345
	FLOAT     // 123.45
	HEX       // 0xdeadbeef
	CHAR      // 'a'
	STRING    // "abc"
	RAWSTRING // `abc` (must be diff because interpolation)
	LABEL     // :welrkj
	literal_end

	operator_beg
	// Operators and delimiters
	TOKPLUS // +
	TOKSUB  // -
	TOKMUL  // *
	TOKDIV  // /
	TOKREM  // %
	TOKAT   // @
	TOKHAT  // ^
	TILDE   // ~
	QUEST   // ?
	TOKNOT  // !

	PLUSEQ  // +=
	MINUSEQ // -=
	MULEQ   // *=
	DIVEQ   // /=

	ANDAND  // &&
	OROR    // ||
	ARROW   // =>
	RETURNS // ->
	LAMBDA  // \ and \\

	EQ     // =
	EQEQ   // ==
	EQEQEQ // ===
	LSS    // <
	GTR    // >

	NEQ   // !=
	NEQEQ // !==
	LEQ   // <=
	GEQ   // >=

	LPAREN    // (
	LBRACK    // [
	LBRACE    // {
	COMMA     // ,
	DOT       // .
	DOTDOT    // ..
	DOT3      // ...
	RPAREN    // )
	RBRACK    // ]
	RBRACE    // }
	SEMICOLON // ;
	COLON     // :
	DBLCOLON  // ::
	operator_end

	keyword_beg
	// Keywords
	ASSERT
	BREAK
	CASE
	CONTINUE
	EACH
	ELSE
	EXPORT
	EXTEND
	GIVEN
	IF
	IMPORT
	IN
	LOOP
	MAP
	ONCONDITION
	ONTIME
	ORTYPE
	RETURN
	STMAP
	TUPLE
	TYPEPRED
	UNLESS
	VAL
	VAR
	WHILE
	keyword_end
)

// string form of op tokencodes; since it's a map, order doesn't matter
var opstringMap = map[TokenCode]string{
	TOKPLUS:   "+",
	TOKSUB:    "-",
	TOKMUL:    "*",
	TOKDIV:    "/",
	TOKREM:    "%",
	TOKAT:     "@",
	TOKHAT:    "^",
	TILDE:     "~",
	QUEST:     "?",
	TOKNOT:    "!",
	PLUSEQ:    "+=",
	MINUSEQ:   "-=",
	MULEQ:     "*=",
	DIVEQ:     "/=",
	ANDAND:    "&&",
	OROR:      "||",
	ARROW:     "=>",
	RETURNS:   "->",
	LAMBDA:    "\\",
	EQ:        "=",
	EQEQ:      "==",
	EQEQEQ:    "===",
	LSS:       "<",
	GTR:       ">",
	NEQ:       "!=",
	NEQEQ:     "!==",
	LEQ:       "<=",
	GEQ:       ">=",
	LPAREN:    "(",
	LBRACK:    "[",
	LBRACE:    "{",
	COMMA:     ",",
	DOT:       ".",
	DOTDOT:    "..",
	DOT3:      "...",
	RPAREN:    ")",
	RBRACK:    "]",
	RBRACE:    "}",
	SEMICOLON: ";",
	COLON:     ":",
	DBLCOLON:  "::",
}

// inverse of opstringMap for just the opr codes that are treated as functions.
var stringopMap = map[string]TokenCode{
	"+":  TOKPLUS,
	"-":  TOKSUB,
	"*":  TOKMUL,
	"/":  TOKDIV,
	"%":  TOKREM,
	"<":  LSS,
	">":  GTR,
	"<=": LEQ,
	">=": GEQ,
	"in": IN,
}

// separate list of those operator strings that can form labels, linearly searched and so, ordered longest first.
var opstringLabels = []string{
	"+=", "-=", "*=", "/=", ">=", "<=", "==", "=", "<", ">", "+", "-", "*", "/", "!", "%",
}

var tkcdStringsLow = []string{
	"ILLEGAL", "EOF", "COMMENT", "LINECOMMENT", "", "IDENT", "IDENTUU", "INT", "FLOAT", "HEX",
	"CHAR", "STRING", "RAWSTRING", "LABEL",
}

// Tokens that can be passivated, ie accept $ in front of them
var passivatable = []TokenCode{TOKPLUS, TOKSUB, TOKMUL, TOKDIV, TOKREM, EQEQ, LSS, LEQ, GTR, GEQ}

var kwstrings = []string{
	"assert",
	"break",
	"case",
	"continue",
	"each",
	"else",
	"export",
	"extend",
	"given",
	"if",
	"import",
	"in",
	"loop",
	"map",
	"oncondition",
	"ontime",
	"ortype",
	"return",
	"STMap",
	"tuple",
	"typepred",
	"unless",
	"val",
	"var",
	"while",
}

var keywords map[string]TokenCode

func init() {
	keywords = make(map[string]TokenCode)
	tc := keyword_beg + 1
	for _, s := range kwstrings {
		keywords[s] = tc
		tc += 1
	}
}

type Token struct {
	code TokenCode
	pos  Pos
	val  string
}

var EOFToken = &Token{EOF, Pos(-1), ""}

func (tk *Token) Length() int {
	if operator_beg < tk.code && tk.code < operator_end {
		return len(opstringMap[tk.code])
	} else {
		if tk.val == "" {
			panic("did not expect empty token here")
		}
		return len(tk.val)
	}
}

func (tkn *Token) String() string {
	var tcdstring string
	if tkn.code < literal_end {
		tcdstring = tkn.val // tkcdStringsLow[int(tkn.code)]
		if tkn.code == STRING {
			tcdstring = fmt.Sprintf("\"%s\"", tcdstring)
		} else if tkn.code == RAWSTRING {
			tcdstring = fmt.Sprintf("`%s`", tcdstring)
		}
	} else if tkn.code < operator_end {
		tcdstring = opstringMap[tkn.code]
	} else {
		tcdstring = tkn.val
	}
	return tcdstring
}

// make a "synthetic token" that has no position
func SynthToken(cd TokenCode, strg string) *Token {
	return &Token{cd, Pos(-1), strg}
}

func (tkn *Token) AnyString() bool {
	return tkn.code == STRING || tkn.code == RAWSTRING
}

type Scanner struct {
	Linesrc    *bufio.Scanner // source of lines
	Linestarts []int          // provided by user, filled in as lines are read
	tokbufp    *[]*Token      // last line scanned, storage reused per line
	curpos     Pos
	genstate   int // will be 0:normal, 1:in multiline comment; 2:in raw string
	errcount   int
	fname      string
}

// newScanner is the standard way to create a token scanner for d8m text, typically from a file, but
// strictly speaking, from a bufio.Scanner.
func newScanner(lnsrc *bufio.Scanner, filename string, filenum int, genstarts bool) *Scanner {
	var lstarts []int
	if genstarts {
		lstarts = make([]int, 0, 10)
		lstarts = append(lstarts, 0) // first line starts at index 0
	}
	tkbuf := make([]*Token, 50)
	return &Scanner{lnsrc, lstarts, &tkbuf, Pos(filenum << 20), 0, 0, filename}
}

type lineCharInfo struct {
	fname   string
	linenum int
	colnum  int
}

func (s *Scanner) getLineCharInfo(charnum int, fn string) lineCharInfo {
	lnum := binsearchInts(s.Linestarts, charnum)
	cnum := -1
	if lnum > 0 {
		cnum = charnum - s.Linestarts[lnum-1]
	}
	return lineCharInfo{fn, lnum, cnum}
}

func pushToken(tokbufp *[]*Token, tk *Token) {
	*tokbufp = append(*tokbufp, tk)
}

// Format the token nicely
func (s *Scanner) prToken(tkn *Token) string {
	ofs := tkn.pos.byteIndex()
	if ofs == 0 {
		ofs = 1
	}
	linenum := binsearchInts(s.Linestarts, ofs) // note: cannot return -1
	col := ofs - s.Linestarts[linenum-1]
	tcdstring := tkn.String()
	if tkn.val == "" || tkn.code > operator_end {
		return fmt.Sprintf("[%s @ %d:%d]", tcdstring, linenum, col)
	} else {
		return fmt.Sprintf("[%s @ %d:%d = %s]", tcdstring, linenum, col, tkn.val)
	}
}

// unlike all others, the val slot of an ILLEGAL token is an error message
func (s *Scanner) errorToken(pos Pos, info string) {
	errtok := Token{ILLEGAL, pos, info}
	pushToken(s.tokbufp, &errtok)
	s.errcount += 1
}

func skipspace(line []byte, inx int) int {
	for {
		if inx >= len(line) {
			break
		}
		c := line[inx]
		if c != ' ' && c != '\t' {
			break
		}
		inx += 1
	}
	return inx
}

func lower(ch byte) byte   { return ('a' - 'A') | ch } // clever. stolen from go code
func alpha(ch byte) bool   { return ('a' <= lower(ch) && lower(ch) <= 'z') || ch == '_' }
func isDigit(ch byte) bool { return '0' <= ch && ch <= '9' }

// line[inx] begins a numeric literal (usually a digit but possibly a dot)
// Figure out what kind and how long it is. When returning an error token, we
// don't have a way to infer its length so we pass that back. The second retval
// may be ignored in other cases
func number(line []byte, pos Pos, inx int) (*Token, int) {
	if inx+1 == len(line) {
		return &Token{INT, pos, string(line[inx : inx+1])}, inx + 1
	}
	first := line[inx]
	firstdot := 0
	floatlit := first == '.'
	exponlit := false
	inx0 := inx
	inx += 1
	nx := line[inx]
	if first == '0' && lower(nx) == 'x' {
		inx += 1
		for {
			if inx == len(line) {
				break
			}
			nx = line[inx]
			if !(isDigit(nx) || 'a' <= lower(nx) && lower(nx) <= 'f') {
				break
			}
			inx += 1
		}
		return &Token{HEX, pos, string(line[inx0:inx])}, inx
	}
	for {
		if nx == '.' {
			if floatlit { // whoops, multiple decimal points
				if inx == firstdot+1 && !exponlit { // .. means back up
					return &Token{INT, pos, string(line[inx0:firstdot])}, firstdot
				}
				return &Token{ILLEGAL, pos, "multiple decimal points in numeric literal"}, inx + 1
			}
			floatlit = true
			firstdot = inx
			inx += 1
		} else if lower(nx) == 'e' {
			if exponlit {
				return &Token{ILLEGAL, pos, "multiple exponents in numeric literal"}, inx + 1
			}
			exponlit = true
			inx += 1
			if inx == len(line) {
				return &Token{ILLEGAL, pos, "numeric literal cannot end in 'e'"}, inx
			}
			if line[inx] == '-' {
				inx += 1
				if inx == len(line) {
					return &Token{ILLEGAL, pos, "numeric literal cannot end in 'e-'"}, inx
				}
			}
			if !isDigit(line[inx]) {
				return &Token{ILLEGAL, pos, "exponent form must have exponent digit(s)"}, inx
			}
		} else if isDigit(nx) {
			inx += 1
		} else {
			break
		}
		if inx == len(line) {
			break
		}
		nx = line[inx]
	}
	if floatlit || exponlit {
		return &Token{FLOAT, pos, string(line[inx0:inx])}, inx
	} else {
		return &Token{INT, pos, string(line[inx0:inx])}, inx
	}
}

// line[inx-1] begins an identifier; return the index of the first byte beyond it
func ident(line []byte, inx int) int {
	for {
		if inx == len(line) {
			break
		}
		c := line[inx]
		if !(alpha(c) || isDigit(c)) {
			break
		}
		inx += 1
	}
	return inx
}

// line[inx-1] begins a single quoted string; return the byte literal it contains,
// along with the index of the byte with closing quote or -1 if unterminated on
// the line (which caller must treat as error)
func snglquotes(line []byte, inx int) (byte, int) {
	if inx >= len(line) {
		return 0, -1
	}
	ch := line[inx] // probably the retval
	if ch == '\\' {
		inx += 1
		if inx == len(line) {
			return 0, -1
		}
		switch line[inx] {
		case 't':
			ch = '\t'
		case 'r':
			ch = '\r'
		case 'n':
			ch = '\n'
		case '\\':
			ch = '\\'
		case '\'':
			ch = '\''
		default:
			panic("unwritten")
		}
	}
	inx += 1
	if line[inx] != '\'' {
		return 0, -1
		inx += 1
	}
	return ch, inx + 1
}

// line[inx-1] begins a double quoted string; return a string along with the index
// of the byte with closing quote or -1 if unterminated on the line (which caller must treat as error)
// Note that we assume the code supplying line has ensured it contains no line termination chars.
func dblquotes(line []byte, inx int) ([]byte, int) {
	var strg = make([]byte, 0, len(line)-inx) // make a copy with a guess at size needed
	for {
		if inx == len(line) {
			return []byte{}, -1
		}
		ch := line[inx]
		if ch == '"' {
			return strg, inx
		}
		if ch == '\\' {
			inx += 1
			if inx == len(line) {
				return []byte{}, -1
			}
			switch line[inx] {
			case 't':
				strg = append(strg, '\t')
			case 'r':
				strg = append(strg, '\r')
			case 'n':
				strg = append(strg, '\n')
			case '\\':
				strg = append(strg, '\\')
			case '"':
				strg = append(strg, '"')
			default:
				strg = append(strg, ch, line[inx])
			}
			inx += 1
		} else if ch == '#' {
			// if next char is left brace, switch to "follow balanced braces" mode
			inx += 1
			if inx == len(line) {
				return []byte{}, -1
			}
			if line[inx] == '{' {
				strg = append(strg, ch, '{')
				inx += 1
				brccount := 1
				for brccount > 0 {
					if inx == len(line) {
						return []byte{}, -1
					}
					ch = line[inx]
					if ch == '{' {
						brccount += 1
					} else if ch == '}' {
						brccount -= 1
					}
					strg = append(strg, ch)
					inx += 1
				}
			} else {
				strg = append(strg, ch)
			}
		} else {
			strg = append(strg, ch)
			inx += 1
		}
	}
}

// Main entry point for token scanning. The line arg is the input; tokenize
// and push info into s.tokbufp. Return values are
// 0 --> normal
// -1 --> final token is (incomplete) multiline comment
// 1 --> final token is (incomplete) multiline string
// In both non-zero cases, the first part of the corresponding token is in tokbuff.
func (s *Scanner) TokenScan(line []byte) int {
	inx := 0  // index in line
	inxn := 0 // also index in line, usually end of token
	pos0 := s.curpos.byteIndex()
	pos := s.curpos
	var c, nx, nxnx byte
	var cd TokenCode
	var tkn *Token
	colonHack := false // true if last ident terminated by colon
	// each pass through this loop normally gets one token
	for ; ; inx = inxn {
		inx = skipspace(line, inx)
		inx0 := inx
		pos = pos.setIndex(pos0 + inx)
		if inx >= len(line) {
			break
		}
		c = line[inx]
		passivating := false
		if c == '$' {
			passivating = true
			inx0 += 1
			inx += 1
			if inx == len(line) {
				s.errorToken(pos, "line may not end with $")
				return 0
			}
			c = line[inx]
		}
		// c is good; initz the next 2 to drive the FSM
		if inx < len(line)-1 {
			nx = line[inx+1]
		} else {
			nx = 0
		}
		if inx < len(line)-2 {
			nxnx = line[inx+2]
		} else {
			nxnx = 0
		}
		if isDigit(c) {
			tkn, inxn = number(line, pos, inx)
			pushToken(s.tokbufp, tkn)
		} else if alpha(c) {
			inxn = ident(line, inx+1)
			if inxn < len(line) && line[inxn] == ':' {
				colonHack = true
			}
			ident := string(line[inx0:inxn])
			rsvcode := keywords[ident]
			cd = IDENT
			if rsvcode != ILLEGAL {
				cd = rsvcode
				if passivating && rsvcode == IN {
					cd = IDENT
				}
			} else if len(ident) >= 2 && line[inx0] == '_' && line[inx0+1] == '_' {
				cd = IDENTUU
			}
			tkn = &Token{cd, pos, ident}
			pushToken(s.tokbufp, tkn)
		} else {
			switch c {
			// start with (most of) the literals
			case '`', '"':
				var strg []byte
				if c == '`' {
					restOfLine := line[inx0+1 : len(line)]
					inxn = bytes.IndexByte(restOfLine, '`')
					if inxn < 0 {
						strg = restOfLine
					} else {
						inxn += inx0 + 1
						strg = line[inx0+1 : inxn]
					}
					cd = RAWSTRING
				} else {
					strg, inxn = dblquotes(line, inx0+1)
					if inxn < 0 {
						s.errorToken(pos, "unterminated string")
						return 0
					}
					cd = STRING
				}
				if inxn < 0 { // can only be raw
					strg = append(strg, '\n')
					tkn = &Token{cd, pos, string(strg)}
					pushToken(s.tokbufp, tkn)
					return 1
				} else {
					tkn = &Token{cd, pos, string(strg)}
					inxn += 1
					pushToken(s.tokbufp, tkn)
					continue
				}
			case '\'':
				c, inxn = snglquotes(line, inx0+1)
				if inxn < 0 {
					s.errorToken(pos, "unterminated byte literal")
					return 0
				}
				tkn = &Token{CHAR, pos, string(c)}
				pushToken(s.tokbufp, tkn)
				continue
				cd = STRING
			case ',':
				cd = COMMA
			case '(':
				cd = LPAREN
			case ')':
				cd = RPAREN
			case '{':
				cd = LBRACE
			case '}':
				cd = RBRACE
			case '[':
				cd = LBRACK
			case ']':
				cd = RBRACK
			case ';':
				cd = SEMICOLON
			case '@':
				cd = TOKAT
			case '%':
				cd = TOKREM
			case '?':
				cd = QUEST
			case '^':
				cd = TOKHAT
			case '~':
				cd = TILDE
			// that's it for pure single char tokens, next consider chars that combine
			case '!':
				if nx == '=' {
					if nxnx == '=' {
						cd = NEQEQ
					} else {
						cd = NEQ
					}
				} else {
					cd = TOKNOT
				}
			case '+':
				if nx == '=' {
					cd = PLUSEQ
				} else {
					cd = TOKPLUS
				}
			case '-':
				if nx == '=' {
					cd = MINUSEQ
				} else if nx == '>' {
					cd = RETURNS
				} else {
					cd = TOKSUB
				}
			case '*':
				if nx == '=' {
					cd = MULEQ
				} else {
					cd = TOKMUL
				}
			case '/':
				if nx == '=' {
					cd = DIVEQ
				} else if nx == '/' { // line comment
					tkn = &Token{LINECOMMENT, pos, string(line[inx0:len(line)])}
					pushToken(s.tokbufp, tkn)
					return 0
				} else if nx == '*' { // std comment
					for {
						if inx+2 == len(line) {
							inxn = -1
						} else {
							inxn = inx + 2 + bytes.IndexByte(line[inx+2:len(line)], '*')
						}
						if inxn < 0 || inxn == len(line)-1 {
							line = append(line, '\n')
							tkn = &Token{COMMENT, pos, string(line[inx:len(line)])}
							pushToken(s.tokbufp, tkn)
							return -1
						} else if line[inxn+1] == '/' {
							inxn += 2
							tkn = &Token{COMMENT, pos, string(line[inx0:inxn])}
							pushToken(s.tokbufp, tkn)
							break
						}
						inx = inxn
					}
					continue
				} else {
					cd = TOKDIV
				}
			case '=':
				if nx == '=' {
					if nxnx == '=' {
						cd = EQEQEQ
					} else {
						cd = EQEQ
					}
				} else if nx == '>' {
					cd = ARROW
				} else {
					cd = EQ
				}
			case '<':
				if nx == '=' {
					cd = LEQ
				} else {
					cd = LSS
				}
			case '>':
				if nx == '=' {
					cd = GEQ
				} else {
					cd = GTR
				}
			case ':':
				if nx == ':' {
					cd = DBLCOLON
				} else if alpha(nx) && !colonHack {
					// standard label: ident happens to work
					inxn = ident(line, inx+1)
					tkn = &Token{LABEL, pos, string(line[inx0:inxn])}
					pushToken(s.tokbufp, tkn)
					continue
				} else {
					// check for nonstd label, else resolve to COLON
					// this is an obscure case, not worth making it fast
					top := inx + 3
					if top > len(line) {
						top = len(line)
					}
					possibleOp := line[inx+1 : top]
					found := ""
					if len(possibleOp) > 0 && possibleOp[0] != ' ' && bytes.ContainsAny(possibleOp, "+-*/%=<>!") {
						for _, opstrg := range opstringLabels {
							if bytes.HasPrefix(possibleOp, []byte(opstrg)) {
								found = opstrg
								break
							}
						}
					}
					if found != "" {
						inxn = inx0 + len(found) + 1 // don't forget to count the ':'
						tkn = &Token{LABEL, pos, string(line[inx0:inxn])}
						pushToken(s.tokbufp, tkn)
						continue
					} else {
						cd = COLON
					}
					colonHack = false
				}
			case '&':
				if nx == '&' {
					cd = ANDAND
				} else {
					s.errorToken(pos, "single & is not a legal token")
					inx = len(line) - 1
					break
				}
			case '|':
				if nx == '|' {
					cd = OROR
				} else {
					s.errorToken(pos, "single | is not a legal token")
					inx = len(line) - 1
					break
				}
			case '.':
				if nx == '.' {
					if nxnx == '.' {
						cd = DOT3
					} else {
						cd = DOTDOT
					}
				} else if isDigit(nx) {
					tkn, inxn = number(line, pos, inx)
					pushToken(s.tokbufp, tkn)
					continue
				} else {
					cd = DOT
				}
			case '\\':
				inxn = inx + 1
				if nx == '\\' {
					inxn += 1
				}
				tkn = &Token{LAMBDA, pos, ""}
				pushToken(s.tokbufp, tkn)
				continue
			default:
				s.errorToken(pos, fmt.Sprint("illegal character", c))
				inx = len(line) - 1
				break
			}
			// We only have punctuation/opr tokens here. Take acct of possible passivation though.
			tkn = &Token{cd, pos, ""}
			inxn = inx + len(opstringMap[cd])
			if passivating {
				found := false
				for _, pcd := range passivatable {
					if cd == pcd {
						found = true
						break
					}
				}
				if found {
					tkn = &Token{IDENT, pos, opstringMap[cd]}
				}
			}
			pushToken(s.tokbufp, tkn)
		}
	}
	return 0
}

// We're in a multiline raw string. Scan the given line for end of string, returning
// -1 --> entire line is in string
//
//	0 --> string ends at end of line (possible whitespace checked here)
//
// >0 --> index in line of first byte after the string
// In all cases, update the last token in tokbuff, which is the first part of the string.
func (s *Scanner) RawScan(line []byte) int {
	final := bytes.IndexByte(line, '`')
	strtok := (*s.tokbufp)[len(*s.tokbufp)-1]
	if final < 0 {
		line = append(line, '\n')
		strtok.val += string(line)
		return -1
	}
	// check if anything after the string
	strtok.val += string(line[0:final])
	for inx, c := range line[final:len(line)] {
		if c != ' ' && c != '\t' {
			return inx + final + 1
		}
	}
	return 0
}

// We're in a multiline comment (hence, using /* ... */ style). Scan the given
// line for end of comment, returning
// -1 --> entire line is in comment
//
//	0 --> comment ends at end of line
//
// >0 --> index in line of first byte after the comment
// As with RawScan, the partly read comment is the final tokbuff item; update it.
func (s *Scanner) CommentScan(line []byte) int {
	inx := 0
	cmttok := (*s.tokbufp)[len(*s.tokbufp)-1]
	for {
		star := bytes.IndexByte(line[inx:len(line)], '*')
		if star < 0 || inx+star+1 == len(line) {
			line = append(line, '\n')
			cmttok.val += string(line)
			return -1
		}
		star += inx
		if line[star+1] == '/' {
			cmttok.val += string(line[0 : star+2])
			star += 2
			if star == len(line) {
				star = 0
			}
			return star
		}
		inx = star + 1
	}
}

// This is the usual entry point; read enough lines to get at least one token
// Usually, this means one line but when multiline comments+strings are encountered,
// it means more. Update all relevant state and return the token buffer.
// On entry, we assume the tokbufp can be cleared.
func (s *Scanner) GetTokens() ([]*Token, error) {
	*s.tokbufp = (*s.tokbufp)[0:0] // clear buffer
	for {
		// each pass reads a line, and we keep it up until we get the result we want or
		// get to the end of the file
		var line []byte
		if s.Linesrc.Scan() {
			line = s.Linesrc.Bytes()
			if s.Linestarts != nil {
				// the +1 adds back the newline; this assumes a single line sep char
				posn := s.curpos.byteIndex() + len(line) + 1
				s.Linestarts = append(s.Linestarts, posn)
			}
			if s.genstate == 0 {
				nxpos := s.curpos.plus(len(line) + 1)
				dispo := s.TokenScan(line)
				s.curpos = nxpos
				if dispo == 0 {
					if len(*s.tokbufp) > 0 {
						return *s.tokbufp, nil
					}
				} else if dispo < 0 {
					s.genstate = 1
				} else {
					s.genstate = 2
				}
			} else {
				var dispo int
				if s.genstate == 1 {
					dispo = s.CommentScan(line)
				} else {
					dispo = s.RawScan(line)
				}
				if dispo <= 0 {
					s.curpos = s.curpos.plus(len(line))
				}
				if dispo >= 0 {
					s.genstate = 0
					if dispo == 0 {
						return *s.tokbufp, nil
					}
					nxpos := s.curpos.plus(len(line) + 1)
					dispo := s.TokenScan(line[dispo:])
					s.curpos = nxpos
					if dispo == 0 {
						if len(*s.tokbufp) > 0 {
							return *s.tokbufp, nil
						}
					} else if dispo < 0 {
						s.genstate = 1
					} else {
						s.genstate = 2
					}
				}
			}
		} else {
			if s.genstate == 0 {
				return []*Token{EOFToken}, nil
			}
			if err := s.Linesrc.Err(); err != nil {
				fmt.Println("returning ", err)
				return []*Token{}, err
			}
			// here, we've pushed a to-be-completed comment or rawstring token onto tokbufp
			// but now it turns out to be unterminated. To fix, clear tokbufp then push
			// and return the error token
			*s.tokbufp = (*s.tokbufp)[0:0]
			msg := "unterminated comment"
			if s.genstate == 2 {
				msg = "unterminated string"
			}
			s.errorToken(s.curpos, msg)
			return *s.tokbufp, nil
		}
	}
}
