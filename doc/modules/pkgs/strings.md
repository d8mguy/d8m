## Strings

This module provides access to a subset of the functions in the corresponding go package, plus a few others.


#### function split

    val split = \(strg, sep: string) -> list(string) {...}
Be careful: go `Split` doesn't return an empty string if no instances of `sep` in `strg`, it returns a singleton list of strg. Thus, the correct test for no separators is split.count == 1, not 0.

#### function join

    val join = \(strgs: list(string), inter: string) -> string {...}


#### function toLower

    val toLower = \(strg: string) -> string {...}

#### function toUpper

    val toUpper = \(strg: string) -> string {...}

#### function toCap

    val toCap = \(strg: string) -> string {...}

#### function toCamel

    val toCamel = \(strgs: list(string), initial: boolean) -> string {...}
Generate a camel case string from the input strgs; `initial` says whether to capitalize the first word.

#### function genUpper

    val genUpper = \imp(inx: integer) -> string {...}
`Inx` encodes a base-26 number; generate an upper case letter sequence from it.

#### function genLower

    val genLower = \imp(inx: integer) -> string {...}
`Inx` encodes a base-26 number; generate a lower case letter sequence from it.

#### function toOrdinal

    val toOrdinal = \imp(xxx: integer) -> string {...}
This isn't the only way to generate ordinals but it's reasonable.

#### function startsWith

    val startsWith = \(strg, ptn: string) -> boolean {...}
True if `strg` starts with `ptn`.

#### function endsWith

    val endsWith = \(strg, ptn: string) -> boolean {...}
True if `strg` ends with `ptn`.

#### function trimSpace

    val trimSpace = \imp(strg: string) -> string {...}
Trim whitespace from both ends. This is done in a go pkg so we use that.

