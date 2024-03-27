## Codecs/json

A `jsonStreamer` holds bytes that can be decoded as json into a particular type (using the `fromJson` method) or encoded
into json (using the `toJson` method).

A json codec is an exotic beast in a statically typed language, since javascript is dynamically typed. Certain things
that are trivially codeable in json are essentially unrepresentable in d8m, go, or any other statically typed language.
For the common case of using json to communicate with a web browser, a website designer may be able to ensure that
the things the browser needs occupy the subset of javascript compatible with static typing (mainly that list elements
are all of the same type). But pre-existing website designs may not allow that, or other issues may intervene.

The go solution for this is a json coding package that uses `interface{}` extensively, essentially bypassing static
typing and/or allowing you to query what's in the json. This approach works and can be useful, but if you are able to
use static types, there's a performance penalty for dynamic typechecking. The solution adopted here is different and in fact,
in the initial release, the d8m compiler can't generate code for a fully generic json interface. Instead, `jsonStreamer`, the type defined in the module,
can decode to well defined d8m types and encode both static and dynamic types. If you need to send json coded dynamic
types to a d8m-based program, you can receive the bytes but can't decode them with the tools provided here.

The flip side of these limitations is that in both directions, the d8m compiler generates code for the specific type
you're encoding or decoding, so it's fast. This is done with rewrite rules that are included in this module.



    [jsonStreamer: lb]

where `lb:list(byte)`

The type `jsonStreamer` extends `baseStreamer` and includes all of its methods; see documentation for `baseStreamer` [here](xxx).

    val jsonStreamer = extend baseStreamer where {
        method readFloat = \mod() -> float {...}
        method readString = \mod() -> string {...}
        method readLabel = \mod() -> label {...}
        method writeString = \(bytes: string) -> string {...}
        method fromJson = \mod() -> entity {...}
        method toJson = \mod(x:entity) -> nothing {...}
        method toJsonStartObject = \mod() -> nothing {...}
        method toJsonFinishObject = \mod() -> nothing {...}
        method toJsonStartList = \mod() -> nothing {...}
        method toJsonFinishList = \mod() -> nothing {...}
        method toJsonAttributeOpen = \mod(lbl: label) -> nothing {...}
        method toJsonComma = \mod() -> nothing {...}
        method toJsonInteger = \mod(x:integer) -> nothing {...}
        method toJsonFloat = \mod(x:float) -> nothing {...}
        method toJsonLabel = \mod(x:label) -> nothing {...}
        method toJsonString = \mod(x:string) -> nothing {...}
        method toJsonListInteger = \mod(lst: list(integer)) -> nothing {...}
        method toJsonListFloat = \mod(lst: list(float)) -> nothing {...}
        method toJsonListLabel = \mod(lst: list(label)) -> nothing {...}
        method toJsonListString = \mod(lst: list(string)) -> nothing {...}
    }

method `readFloat` copies so we know where the float ends and call fmt to convert.

method `readString` does a copy, in case there are embedded backslashes.

method `readLabel` checks for label content restrictions and calls the builtin cvt method.

method `writeString` creates a string literal by inserting backquotes as needed, and putting double quotes around it. This method doesn't mod self, use add for that.

method `fromJson` decodes a json buffer into an expected type and returns the resulting entity. The compiler will inline it to avoid interface types if conditions are right.

method `toJson` encodes any entity of a known, non-recursive, type. Note that the body of this definition is empty; it works by rewrite rules defined below that match funcalls to this function and change them into calls to "toJsonSynthdN" for N a sequence of integers; each of these synthesized functions encodes a type involved in encoding the full type and is memoized for possible reuse. The upshot is that the type to be encoded is unrolled into a set of calls for each part, with lists and tuples orchestrated to is the "general purpose" encoder. If called with an entity of a specific non-recursive type, the generated code will unroll to avoid interface types.

method `toJsonStartObject` and the methods that follow are intended to aid in hand coding of toJson for dynamic types.
