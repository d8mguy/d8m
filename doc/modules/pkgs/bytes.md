## Bytes

This module provides access to a subset of the functions in the corresponding go pkg, plus a few others. Since d8m doesn't adopt Go's conventions about initial capitalization in identifiers, the Go package functions are not capitalized in this module. The module also includes some of the basic byte-based character range checks, and miscellaneous useful byte-oriented functions. 

    val split = \(self, sep: list(byte)) -> list(list(byte)) {...}
    val join = \(strgs: list(list(byte)), inter: list(byte)) -> list(byte) {...}
    val toLower = \(lb: list(byte)) -> list(byte) {...}
    val toUpper = \(lb: list(byte)) -> list(byte) {...}
Split, join, toLower, and toUpper just call the bytes package functions.

    val index = \(lb, of: list(byte)) -> integer {...}
Index just calls gb.Index, preserving its use of -1 as the fail value.

    val toCap = \imp(lb: list(byte)) -> list(byte) {...}
Return a copy of lb with its first char capitalized; use gb.ToUpper for this.

    val toLiteral = \imp(b: byte) -> string {...}
Create a 3-character string that designates b in d8m text, including the single quotes. This doesn't do escapes, nonprintable bytes, etc. For example, `toLiteral(97)` (the ascii value for lower case 'a') is `"'a'"`.

    val transformSingleSingle = \imp(contents: list(byte), from: list(byte), to: list(byte)) -> list(byte) {...}
Copy a given `list(byte)` and substitute bytes for corresponding bytes.
For example, if `from`, `to` are `['\r']`, `['\n']` you'd change all carriage returns to linefeeds. 

It is required that `from.count == to.count`, that is, there must be a "to" byte for each "from" byte.

    val transformSingleMulti = \imp(contents: list(byte), from: list(byte), to: list(list(byte))) -> list(byte) {...}
Copy a given `list(byte)` and substitute one or more `list(byte)` for corresponding bytes. For example, if from, to are `['-']`, `[[cvt("&mdash;", list(byte))]]` you'd change all dashes to the string "&mdash;" __Attention:__ this may fail if any byte of `from` occurs in `to`. Because we search in the copy, which is also changing with each substitution.

It is required that `from.count == to.count`, that is, there must be a "to" list(byte) for each "from" byte.

    val transformMultiMulti = \imp(contents: list(byte), from, to: list(list(byte))) -> list(byte) {...}
Copy a given `list(byte)` and substitute one or more `list(byte)` for corresponding `list(byte)`. For example, if from, to are `[['-', '-']]`, `[['&', 'm', 'd', 'a', 's', 'h', ';']]` you'd change all "--" to "&mdash;" __Attention:__ this may fail if any complete element of `from` occurs as a sub-sequence of any element of `to`. Because we search in the copy, which is also changing with each substitution.

It is required that `from.count == to.count`, that is, there must be a "to" `list(byte)` for each "from" `list(byte)`.

    val upper = \(b: byte) {...}
True if b is uppercase

    val lower = \(b: byte) {...}
True if b is lowercase

    val digit = \(b: byte) {...}
True if b is a digit

    val alpha = \(b: byte) {...}
True if b is alphabetic (or '_')

    val alphanum = \(b: byte) {...}
True if b is alphanumeric

    val hexdigit = \(b: byte) {...}
True if b is a hex digit

    val spacetab = \(b: byte) {...}
True if b is space or tab

    val toHex = \(b: byte) -> integer {...}
Arg should be a hex digit, i.e. a digit or alpha char between A and F. Return the digital value in hex coding.

    val strip = \imp(strg: list(byte), rmv: list(byte)) {...}
Arg is a string as `list(byte)` and a `list(byte)` of characters to remove; return a copy of the string without those characters.

