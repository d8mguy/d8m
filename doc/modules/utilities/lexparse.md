## Lexparse

Lexparse is a "parameterized lexer and parser". One way to think of it as a parser generator that's specialized for programming languages and easier to use than the traditional tools in this category. Considering this perspective you should take a moment to be amazed that lex and yacc are 50 years old; they were so advanced a technology for the time that parser generators haven't changed much since. Lexparse is significantly different in design, with a mix of prebuilt and extensible parts. We'll discuss what that means.

There are negatives in the "user experience" of yacc and its progeny &mdash; lex and yacc are distinct programs requiring distinct specifications, which are in files distinct from your program. You need to write actions that integrate these pieces into the rest your program. You need to run the specifications through the generator programs and integrate the resulting files into the rest of your program. So the build process is complicated, and it's difficult to avoid structuring your program around the parser (because of actions). Lexparse aims for a much simpler UX: to use the parser you create a `parseState` entity from a pre-processed grammar (`langDefn`) and you're ready to go. You construct a `langDefn` from a set of simple d8m entities that describe aspects of both lexing and parsing. The parser requires no direct user input for actions; it produces a `Term` describing your parse tree, giving you some control over the form of that parse tree in your grammar definition. Lexparse allows you to bundle the lexer and parser but you can also import the lexparse module for lexing only. The parser has an option for "fast scanning" whereby expressions delimited by brackets are not parsed in detail but scanned for balanced brackets. This can speed things up and reduce memory usage for certain applications where you don't need the full parse tree.

Technically, yacc-inspired parser generators use a very simple stack-augmented finite state machine driven by a big table, with lookahead compiled into split states, and so on. Thematically, we can say that the tradeoff it makes is maximum parsing speed at a cost of more complicated pre-processing (and larger tables). Lexparse does pre-processing as well, but the internal form is more compact and the parsing code more complex. Like yacc, the actual parser in lexparse is LR; LR(1) to be more specific. Lexparse has O(n) behavior, like yacc; the constant may be larger or smaller than yacc, but I assert that this matters very little.

The specific form of the grammar for lexparse is, as we'll see, heavily inspired by the structures found in programming languages. Lexparse has several built in nonterminals, including one named `stmt`, and it assumes that its language consists of a sequence of `stmt`. Another built in nonterminal is `expr` which involves operator tokens with precedences. Lexparse also assumes that brackets are used in a balanced way &mdash; that `[` opens some kind of syntactic structure that's closed with `]` and not `}` for example. Finally, it assumes that one rule for `stmt` is `expr`, i.e. that expressions can be statements.

### Specifying Grammars

We start with lexing, also known as tokenizing or "lexical scanning". Formally, programming language grammars are defined over an alphabet not of characters but of tokens; the lexer's job is to create tokens from characters. Lexparse defines a type `lexerT` that's defined by 4 lists whose details we'll discuss shortly; these provide the information needed to scan tokens, although the actual scanning code is in another type called `parseCxt`. The parser uses the type `langDefn` mentioned in the introduction that handles both lexing and parsing.

The parser's terminal symbols, which are produced by the lexer, are classified as either _fixed tokens_ or members of _token classes_. In explaining the concepts, we will also speak about _token categories_. Token classes are things like identifiers and numeric literals &mdash; instances of token classes automatically become grammar terminals and hence, leaf nodes in parser output. The idea of token categories helps us talk about the structure of the lexer's output; specficially, our token categories are `separator`, `bracket`, and `opr`. We'll discuss these first.

The `separator` category consists of single character tokens; it's defined with a list of (single character) strings. Comma and semicolon would commonly be defined as separators. The single character nature of separator tokens means that every separator is necessarily a token by itself, it cannot combine with any other character. However, there are subtleties here, since this rule is not applied globally. Further details are at the end of this section, but the correct intuition is that such characters can appear within token classes such as string literals.

The `bracket` category consists of pairs of tokens that form opening and closing brackets; it is defined with a list of strings. For example

    ["(", ")", "[", "]", "{", "}"]

would be a definition of brackets for d8m (or many other languages). Tokens defined as brackets will be returned verbatim by the tokenizer, never combined with other characters. In other words, single character bracket tokens act also as separators. There is no requirement that bracket tokens be single characters: `"<<", ">>"` is a perfectly acceptable bracket pair. It's allowed to define bracket pairs that are prefixes of other bracket pairs and the lexer will pick the longest possible bracket in such cases. It is not allowed for the characters of bracket pairs to overlap in any other way. For example, if `"<<", ">>"` and `"[", "]"` are bracket pairs, then one can also define `"<<<", ">>>"` and `"<<*", "*>>"` as bracket pairs, but not `"[<", ">]"` or `"<<[", "]>>"`.

As we'll see in more detail below, the `:opr` category is defined operationally (!) as a kind of leftover set of tokens. Generally speaking, neither `separator` nor `bracket` tokens appear in parser output. When `opr` tokens are defined as expression operators, the grammar specification tells how to label them in parser output.

We stated earlier that token classes form the leaf nodes of parser output. Every token class is automatically defined as a kind of `expr`. Four token classes are built in and supplied with default definitions. The following table gives their names, and an indication of their default definitions:

| class      | default definition                                                                                   |
| :--------- | :--------------------------------------------------------------------------------------------------- |
| `"ident"`  | First character alphabetic (including `_`) then alphanumeric                                         |
| `"int"`    | digits                                                                                               |
| `"flt"`    | digits with fraction part and/or exponential form                                                    |
| `"string"` | strings surrounded by single or double quotes plus backticks, sort of like d8m or go plus javascript |

The "tokenclass" part of the lexer specification is defined as

    list(tuple(tknclass:string, regexp:string))

It can be empty, in which case the token classes are just the four given above, with their default definitions. Otherwise, each element of the "tokenclass" list defines a token class whose name is the value of the `tknclass` attribute and whose definition is the given `regexp`. If the `tknclass` is one of the built in token classes, the `regexp` replaces the default definition. Otherwise, they name new token classes. The `regexp` strings have the standard form accepted by go's regexp package and since the tokenizer tries the patterns at all possible starting points, the patterns should start with `^`. As a special case, an empty string for a built in token class disables it. For example, to disable "flt", include in the "tokenclass" list a tuple `[tknclass~"flt", regexp~""]`.

As an example, a parser for go would require redefining `"int"` to accommodate numerous features of go's integer literals &mdash; hex, octal, and binary syntax, the option to include underscore spacers, etc. Alternatively, one could add token classes with names like `octalInt`, `hexInt`, and so on. This approach would greatly increase the number of leaf node types and thereby complicate the code that interprets parser output. It's probably a bad idea for go integer literals, but in other situations it can be a good approach.

The `:opr` (operator) token category consists of the tokens obtained by accreting characters that include no whitespace, are not separators or brackets, and that match none of the regular expressions for token classes. Such tokens may be defined as expression operators or occur in production rules. Otherwise, they'll be tagged by the parser as illegal tokens.

In addition to the token categories covered so far, the lexer provides for stripping comments. Comment boundaries are defined in essentially the same way as brackets, using pairs of strings. For example,

    "/*", "*/", "//", "\n"

defines two pairs of comment brackets representing the comments for C++, go, d8m, and so on.

To summarize, the lexer scans the string being parsed from beginning to end, at each step it first skips whitespace and comments, then applies rules for brackets, separators, and token classes to find the next token, which is `:opr` if nothing else matches. Bracket and separator characters may be embedded in token classes as long as they cannot be the initial character. For example, one can define comma as a separator and simultaneously define the `int` token class with a regular expression that accepts separator commas embedded in integer tokens, as long as the comma is not allowed to be the first character. Similarly, the default definition of the `string` token class admits all sorts of characters, including ones that might well be defined as separator and bracket characters, but there's no ambiguity because string tokens begin with some kind of quote character.

Occasionally, tokenization rules for programming languages don't work with the rules given so far, because there are situations where token boundaries depend on surrounding context. Lexparse has a way to handle these situations but it's not strictly part of the language definition. Instead, it uses the `parseState` which is described in the upcoming section on [using the parser](#using-the-parser). ParseState has a `tokenCheck` attribute whose type is `nilPossible(\mod(parseState)->nothing)`. If you assign a function to this attribute, it will be called after each call to `getToken`. As a mod function of parseState, it has full access to the token state and characters in the text being parsed. You'll need to know the internal representation of `parseState` in order to use this, but it allows you to do essentially anything you want as part of the tokenizing process.

### Terms

Next, we consider what the parser produces; the lexparse module defines and exports a type `Term`, defined as follows:

    tuple(kind:string, arg:ortype(terms:list(Term), strg:string), loc:integer)

See also the documentation for the Term module.

The `kind` slot's value arises from either the token class or the name of the nonterminal that it represents. Leaf terms set the `kind` slot to the name of the token class and use the `strg` variant of the `arg` attribute, setting the string value to that of the leaf token. As an example, the integer literal `123` becomes a `[Term: "int", "123"]`. Non-leaf Terms set `kind` to the name of the nonterminal that creates them, use the `terms` variant, and set the values in the list from the defining production in order, omitting any literal strings therein, unless all the productions consist of a single terminal. As an example, suppose we have a `stmt` production named `"ifstmt"` defined as

    "if ( expr ) stmt"

Reducing on this rule produces a Term of the form

    [Term: "ifstmt", [T1, T2], loc]

where T1 is an `expr` Term and T2 a `stmt` Term. (And `loc` stands for a location.) Three tokens in this production rule are literal: `if` plus the parentheses. These are omitted from the resulting Term. On the other hand, if you define a nonterminal that just enumerates terminals, the resulting Term works like a leaf node. So given

    ["alt0", :general, "%%"]
    ["alt0", :general, "%%%"]

This will reduce to a Term with `kind` set to `alt0` and `arg` set to either `%%` or `%%%`. So the literal tokens are preserved in the resulting Term when `alt0` is used in a production, e.g.

    ["useAlt0", :general, "A alt0 B"]

produces useAlt0 Terms with three items:

    [Term: "useAlt0", [[Term: "A", ...], [Term: "alt0", "%%"], [Term: "B", ...]]]

It's worth emphasizing that the structure of the grammar is essentially the only tool you have to control the form of the Terms in parse trees. The just-described special treatment of literal-only nonterminals is designed to help. One other feature is detailed shortly that lets you selectively keep nonterminals unreduced in certain contexts.

The string "error" is reserved in `kind` for error Terms. When an error occurs, the parser supplied with lexparse returns a Term whose `kind` is "error" and whose `arg` is an error message containing information about the location of the error.

### Productions

The bulk of a typical grammar specification consists of production rules, in a BNF-like form. These define both statements and (aspects of) expressions. Separate aspects of the grammar specification define how operators work in expressions. We cover production rules next.

A production rule consists of a name, a `ruleType` and a defining string or _rule body_. We can define this in d8m as

    val rawProdn = tuple(name: string, rtyp: ruleType, body: string)

where the `ruleType` is an ELT:

    val ruleType = label(:stmt, :expr, :general)

The `stmt` and `expr` ruleTypes cause the nonterminals they define to be added to the definition of the "stmt" and "expr" nonterminals (respectively).

The rule body is a list of tokens separated by spaces. The tokens may be names of token classes or nonterminals, or they may be tokens in the bracket or separator classes. If they are none of the above, they are taken as _keywords_, like the `if` token in the above example, and they must match literally. The name attribute of a production rule tells which nonterminal it applies to &mdash; it's the left hand side of the BNF for that production rule. Nonterminals are defined by one or more production rules, as usual.

Thus, a rule like the one given as an example above would be written

    ["ifstmt", :stmt, "if ( expr ) stmt"]

and this would effectively add two rules to the BNF form of the grammar:

    ifstmt ::= "if" "(" expr ")" stmt
    stmt   ::= ifstmt

Production rules typed as `:general` stand by themselves and should be referred to by production rules typed as either `:stmt` or `:expr`.

Although I just suggested that there is a "virtual" production rule defining `ifstmt` as a kind of `stmt`, the output Terms never reflect such rules. There will never be a Term whose `kind` is set to `stmt`, or `expr`. Nonterminals of type `:stmt` always have a name like `ifstmt` and this is what appears in Terms. Those of type `:expr` either have such a name or are defined as operators; as we'll see, the grammar specification provides for defining the `kind` of expression operators in Terms.

Nonterminals may be used recursively, but left recursion is generally not allowed; the exception is that productions of type `:expr` may start with `expr`. Thus, a rule like

     ["exprlist", :general, "exprlist , expr"]

is always illegal, and will cause an error. Whereas a rule like

     ["funcall", :expr, "expr ( exprlist )"]

fits into the exception for `expr` and is allowed.

A rule body may be empty. For example, a good definition of optional comma-separated expression lists is:

    ["exprlist", :general, ""]
    ["exprlist", :general, "exprlist0"]
    ["exprlist0", :general, "expr"]
    ["exprlist0", :general, "expr , exprlist0"]

This uses `exprlist0` to comply with the rule about left recursion. `Exprlist0` defines lists that aren't allowed to be empty.

A nonterminal `S` of type `:stmt` will usually start with a fixed token, but may start with `expr`, or with a `:general` nonterminal. A nonterminal `E` of type `:expr` should begin either with a fixed token or with the nonterminal `expr`. There are no restrictions for nonterminals of type `:general` on the initial item of their production rules, except for the previously mentioned limitation on left recursion.

One typically encounters the idea of _reserved words_ in programming language grammars. That idea is not built into lexparse in any way, but since the fixed tokens in rule bodies aren't quoted, you must ensure that the keywords of your language do not overlap with names of nonterminals. For example, a production rule like

    ["while", :stmt, "while ( expr ) stmt"]

is wrong since it attempts to define a nonterminal named "while" that is also present in the rule. Therefore, it violates the left recursion restriction and will be flagged as an error. Give the nonterminal a different name, such as "whilestmt" to avoid the problem.

In addition, the fact that keywords are not quoted in rules makes it fairly easy to create an erroneous grammar by forgetting to define a nonterminal, misspelling it, etc. In order to avoid this, the grammar specification requires a list of the keywords you intend to have in the grammar. The preprocessor checks this against the list it generates and reports on any discrepancies. (Discrepancies are not considered an error, but you should pay attention to this warning while debugging grammars.)

Another thing commonly found in writings about parsers and parser generators is the notion of a _start symbol_. This wraps the grammar; the parser's objective with the input string is to reduce the start symbol. This idea is not formally present in lexparse; the parsing function you call plays the role of the start symbol: `parseStmt` attempts to parse a `stmt`, `parseExpr` an `expr`. Other parser objectives can be created if needed. None of these functions enforce the rule that the entire string is consumed, but this is easily checked from the parser state after the call.

In the body of rules, tokens that designate nonterminals may be written with a `*` suffix (we'll called these _starred nonterminals_). This means that the nonterminal should not be reduced when parsed in this context. For example, let's reconsider the definition of `exprlist` given above. As given, it will reduce each `expr` separately. As an example, if the string `1, 2, 3` is presented to the parser where `exprlist` is expected, the resulting `Term` will look Like

    [Term: "exprlist", [[Term: "int", "1"], [Term: "exprlist", [[Term: "int", "2"], [Term: "exprlist", [[Term: "int", "3"]]]]]]]

(Actually, assuming the example grammar given above, there will be reductions of `exprlist0` as well, making the output even more complicated than what's shown here.) Alternatively, you can write two of the production rules given above as

    ["exprlist", :general, "exprlist0*"]
    ["exprlist0", :general, "expr , exprlist0*"]

to suppress intermediate reductions of `exprlist0`. This causes the `expr` terms to accumulate on the parser's `Term` stack until the `exprlist0` completes, so that the resulting `Term` is

    [Term: "exprlist", [[Term: "int", "1"], [Term: "int", "2"], [Term: "int", "3"]]]

### Expressions

We turn now to expressions in detail. These are defined in part with productions labeled as `:expr`, as mentioned earlier, but also with parts of the grammar specification that define operators, give them precedences, and state their `kind` in output Terms. Define operators with a `list(list(string))`, for example

    [[". dot"],
     ["- unopminus prefix", "! unopbang prefix", "( funcall", "[ index"],
     ["* mult", "/ div", "% pcnt"],
     ["+ plus", "- sub"],
     [".. dotdot", "... dot3", "in in"]
     ["< lt", "> gt", "<= le", ">= ge"],
     ["? qcolon", ": qcolon"],
     ["|| oror", "&& andand"],
     ["@ atsign", "^ caret", "~ tilde"],
     ["= eq", "+= pluseq", "-= subeq", "\*= multeq", "/= diveq"]]

Each string consists of two or three tokens separated by spaces; if three, the last should be one of two words: `prefix` or `postfix`. The first token is usually in the `:opr` category (but is allowed to be `ident`), the second must be an `ident`. Usually, but not always, the string defines the first token as an expression operator. The index of the list in which the string occurs defines the precedence. Thus, in the above definition, the string `". dot"` assigns precedence 0 to token `.`, precedence 1 to tokens `-`, `!`, and so on, precedence 2 to `*`, etc. Operators of lower precedence bind more tightly.

Strings with 3 tokens define the first token as a unary operator, and the third token tells whether it is prefix or postfix. Strings with 2 tokens have two possible meanings, depending on whether the second token is the name of a nonterminal symbol or not. Usually, the second token is _not_ the name of a nonterminal symbol, and in this case the first token becomes defined as a binary operator with the given precedence. Otherwise, the token occurs in production rules that involve `expr` and produce expressions; these definitions assign a precedence to the token when the expression parser encounters it. For example, the above definition has four strings where we assume the tokens occur in nonterminals, their definitions are

    ["funcall", :expr, "expr ( exprlist* )"]
    ["index", :expr "expr [ expr ]"]
    ["qcolon", :expr, "expr ? expr : expr"]

and the strings establish precedences for these operator/bracket tokens: 1 for paren and square bracket, 6 for ? and :. Thus, when parsing a `qcolon` production the parser will reduce any expression already on the term stack with precedence <= 6 when it encounters either ? or :. On the other hand, if it sees left parenthesis after `expr`, it will reduce only expressions with precedence <= 1, that is, dot expressions.

The just made statement says that when the next operator has precedence K, we reduce if the stacked operator has precedence K or less. This means every operator is _left associative_. Lexparse currently has no way to change this.

It's ok for a given token to occur as both a binop and a `prefix` unop (see `-`), but any other duplication causes an error.

### Summary of Grammar Specification

One feature of the grammar specification fails to fit into any of the topics we've divided the discussion into so far. A boolean value named `newlineTermination` enables or disables a parser behavior that supports the now common pattern of using semicolon as a statement separator but allowing newline to stand in for semicolon. Specifically, if `newlineTermination` is true, then in any parse state where semicolon is a possible next token, the parser will accept newline as a substitute if the next token is a possible initial token for `stmt`. Otherwise, newline is ignored. (And if `newlineTermination` is false, newline is always ignored.) Note that formally, this behavior makes no reference to the built in `stmt` nonterminal. Since `expr` is a `stmt`, if semicolon is the statement separator and `newlineTermination` is true, expressions written across multiple lines must be arranged so they cannot legally end at line boundaries. In practice, this means writing binary operators at the end of a line rather than the beginning of the next one.

We can now say how to form a `langDefn` entity, which holds the internal form of a grammar. Its constructor is defined as

    method lithook = \mod(
      operators: list(list(string)),
      rawProductions: list(list(string)),
      tokenClasses: list(tknclassSpec),
      brackets: list(string),
      comments: list(string),
      separators: list(string),
      newlineTermination: boolean)

where `tknclassSpec` is `tuple(name, regexp:string)`. The `operators` argument defines expression operators, `rawProductions` defines the production rules. The last five arguments define the various parameters affecting the behavior of the tokenizer.

The lexparse module includes a grammar definition for d8m, which can serve as an extended example.

Similarly, the constructor for a `lexerT` entity looks like

    method lithook = \mod(
      brackets, comments, separators: list(string),
      tokenClasses: list(tknclassSpec))

### Using the Parser

The following table gives the export list for lexparse:

| Symbol         | description                              |
| :------------- | :--------------------------------------- |
| `Term`         | [See Terms section](#terms)              |
| `langDefn`     | Defined in grammar spec summary          |
| `lexerT`       | Type for lexer definition                |
| `tknclassSpec` | Defined in grammar spec summary          |
| `rawProdn`     | [See Productions section](#Productions)  |
| `parseCxt`     | Holds lexer state; see discussion below  |
| `parseState`   | Holds parser state; see discussion below |
| `parseStmt`    | Parser entry point                       |
| `parseStmts`   | Parser entry point                       |
| `parseExpr`    | Parser entry point                       |
| `scanStmt`     | Parser entry point                       |
| `d8mDefn`      | langDefn for d8m                         |
| `fixD8mLabels` | tokenCheck function useful for d8m       |

A grammar is given for d8m, both as an example and to help define grammars that extend d8m. For extending d8m, you'll need to load lexparse instead of importing it, so you get access to various unexported symbols, such as the list of d8m production rules, that you will need to combine with your extensions. Use the type `tknclassSpec` to help define lexers; use types `rawProdn` and `langDefn` to help define grammars; and use the type `Term` to interpret results or, for that matter, to define the signatures of the parsing functions. For parsing, there are four entry points: `parseStmt`, `parseStmts`, `parseExpr`, `scanStmt`. The first three of these correspond to grammars whose start symbols are `stmt`, a list of `stmt`, and `expr`, respectively. These have signatures that are similar but not identical, as shown in the table:

| Function     | Signature                                   |
| :----------- | :------------------------------------------ |
| `parseStmt`  | `\mod(ps: parseState) -> nilPossible(Term)` |
| `parseExpr`  | `\mod(ps: parseState) -> nilPossible(Term)` |
| `parseStmts` | `\mod(ps: parseState) -> list(Term)`        |

Since `parseStmts` parses and returns a list, the empty list indicates that nothing was found. In all cases, check the `ps.error` attribute after each call; it has type `nilPossible(Term)` and if non-nil will be a `Term` whose `kind` is "error".

The `parseState` tuple is constructed with a `langDefn`; it holds two `parseCxt` attributes named `cxt` and `prevTkn` where `prevTkn` is a copy of the previous `cxt` for lookahead/backup purposes. Other useful attributes include `error: nilPossible(Term)` and `bytes: list(byte)`, which is the string to parse. (Lexparse must get the entire string to be parsed before it starts.) As constructed, the `error` attribute is nil and `bytes` is empty. Thus, to get started, you must first set `bytes` to something; if you wish to reuse a `parseState` you should also call `ps.cxt.clear` to reset the `cxt` attribute to its initial state.

The purpose of the `scanStmt` function is to identify statement boundaries, which will normally allow you to also identify statements from their initial keywords. As such, its signature is `\mod(ps: parseState) -> integer` and it only updates the `parseCxt` in the `ps.cxt` attribute. When called, `ps.cxt` should hold a token that's legal as an initial statement token; it detects and skips tokens, updating `ps.cxt`, until the parsing rules indicate that the statement is complete. At this point, `ps.cxt` will either hold the first token of the next statement, or will indicate that its input is exhausted. The integer returned is the offset into `ps.bytes` of the byte just before the token in `px.cxt` or the length of `ps.bytes`. Thus, you can use `scanStmt` to robustly separate a string into statement boundaries with code like the following:

    ps.bytes = theString
    ps.nxToken()
    var position = ps.cxt.ofs
    while(ps.cxt.toktype != "end") { recordBoundary(position); position = scanStmt(ps) }
    // final boundary is at ps.bytes.count

ScanStmt assumes the grammar "balances brackets", i.e. that every production rule in which an opening bracket occurs has a closing bracket to match it. Between such brackets, it pays no attention to production rules at all, simply lexing tokens until the matched closing bracket is found.

A general characteristic of query compilation is that unused definitions are culled.
For example, the d8m grammar, included as an example, won't appear in any program that imports lexparse but doesn't use it. It can be quite useful to use only the lexer, it can simplify many string pattern matching problems. In such cases, the parser is omitted as well.

### Status

It is worth mentioning that, at least as of the first release, lexparse is far from bug-free. The lexer is pretty solid, and the parser works well enough to parse d8m and other languages of similar complexity, but the parser will no doubt crash in complicated and/or buggy grammars. 

