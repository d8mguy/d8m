## Bitset

Module bitset defines the `bitset0` and `smallint64` types. `Bitset0` provides bitsets in single 64-bit integer words; `smallint64` defines integers taking values between 0 and 63.

The compiler interacts indirectly with these definitions by converting ELTs (enumerated literal types) internally to integer and asserting of them the max values they can take. So ELTs with fewer than 64 elements will work as bitset0. The module defines a STMapping for `set(smallint64)` and ensures that it's defined whenever type `set` is available. However, to use it you must `applyST` yourself.

You can enumerate a `bitset0`; the definition uses the eachstart protocol.

    [bitset0: lst]

where `lst: list(integer)`.

    val bitset0 = extend tuple(...private...) where {
        method setbit = \mod(inx: integer) -> nothing {...}
        method clearbit = \mod(inx: integer) -> nothing {...}
        method empty = \() -> boolean {...}
        method inBitSet = \(inx: integer) -> boolean {...}
        method count = \() -> integer {...}
        method union = \(other: bitset0) -> bitset0 {...}
        method intersection = \(other: bitset0) -> bitset0 {...}
        method subset = \(other: bitset0) -> boolean {...}
    }

Method `subset` tells if self is a subset of other. All the operations here are O(1); count uses the `OnesCount` function in go's `bits` package which in turn uses a hardware instruction if available, so it should be O(1) as well. 