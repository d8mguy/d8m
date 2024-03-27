## Codecs/base

Defines `baseStreamer` and friends to provide basic support for codecs like json and others. 

    [baseStreamer: lb]
where `lb:list(byte)`

`BaseStreamer` holds the stream we're encoding or decoding, plus error information.

    val baseStreamer = extend tuple(content: list(byte), 
        index, errcount: integer, 
        errorList: list(errInfo)) where {
        method out = \() -> string {...}
        method reset = \mod() -> nothing {...}
        method error = \mod(errmsg:string) -> nothing {...}
        method errors = \() -> string {...}
        method cur = \() -> byte {...}
        method next = \mod() -> byte {...}
        method whitespace = \mod() -> nothing {...}
        method skipThrough = \mod(sought: byte) -> nothing {...}
        method optional = \mod(sought: byte) -> boolean {...}
        method expect = \mod(sought: byte) -> nothing {...}
        method add = \mod(strg: string) -> nothing {...}
        method addbytes = \mod(bytes: list(byte)) -> nothing {...}
        method delete = \mod(k: integer) -> nothing {...}
    }

Method `out` produces the current content as a string, after the `toX` method of a codec has generated it.

Method `reset` clears errors and resets index to the beginning of content.

Method `error` records an error.

Method `errors` returns a string describing one or several errors, `""` if there are no errors. 

Method `cur` returns the next byte of content.

Method `next` returns the value of cur before incrementing index.

Method `whitespace` skips whitespace.

Method `skipThrough` skips content through sought (returns at EOF or just past sought).

Method `optional` checks for an optional string by skipping whitespace then checking for a match, skipping past it if present. 

Method `expect` skips whitespace then verifies that byte b is next, setting an error if not. 

Method `add` just blindly copies bytes.

Method `addbytes` also just blindly copies bytes.

Method `delete` removes the last k bytes from the output.
