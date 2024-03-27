## Regexp

Module regexp provides access to a subset of the functions in Go's `regexp` package, plus a d8m type `scanexpT` that can match multiple regular expressions and thereby serve as a simple lexical scanner. 

#### Type regexpT

    [regexpT: pattern]
where `pattern: string`.

    val regexpT = extend tuple(private goptn: rxpkg.Regexp, strg: string) where {
        method find = \(s: list(byte)) -> list(byte) {...}
    	method findIndex = \(s: list(byte)) -> list(integer) {...}
    	method findAllIndices = \(s: list(byte)) -> list(list(integer)) {...}
	    method split = \(s: list(byte)) -> list(list(byte)) {...}
    }

Method `find`: returns the first sequence of bytes matching the pattern, `[]` for no match. 

Method `findIndex`: returns two integers indexing start and end of the first sequence of bytes matching the pattern, `[]` for no match. 

Method `findAllIndices`: returns two integers indexing start and end of the first sequence of bytes matching the pattern, `[]` for no match. 

Method `split`: does a version of split where the separator is the regexp `self` rather than a fixed `list(byte)`. Returns the `list(byte)` between each separator. 

#### Type scanexpT

A `scanexpT` is analogous to a `regexpT` but scans multiple regexps, given in the constructor. This assumes the given strings are mutually exclusive wrt matching, ie at most one can match.

    [scanexpT: ss]
where `ss:list(string)` is a list of pattern strings which get wrapped in () and concatenated with | so as to make a single pattern that can be submatched and post-processing can find which pattern matched each part of the original.

    val scanexpT = extend tuple(strgs: list(string)) where {
        method scan = \imp(bytes: list(byte)) -> list(integer) {...}
        method done = \(bytes: list(byte), ofs: integer) -> boolean {...}
        method next = \imp(bytes: list(byte), ofs: integer) -> list(integer) {...}
        method checkWhite = \imp(bytes: list(byte), indices: list(integer)) -> integer {...}
    }
Method `scan`: runs FindAllSubmatchIndex and processes the result into a list of triples, the match indices plus index of the submatch (i.e., the index of the match in the list argument to the constructor).

Method `done`: `done` and `next` provide a way to scan a scanexpT incrementally.

Method `next`: returns a 3-elt list in the same format as scan, or an empty list if done.

Method `checkWhite`: The indices arg should have been returned from scan. Return index of the first non-whitespace byte between tokens or -1 if no such. Thus, -1 is the expected return if the tokens cover the line, except for whitespace. Whitespace is defined as space and tab characters. 