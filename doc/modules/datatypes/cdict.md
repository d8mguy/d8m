## Cdict

A countingdict maps `T -> integer`, representing a collection of `T` with the multiplicity of each element. This module also defines a STMap from countingdict to go's map type. 

    [countingdict(T): xx]

where `xx: list(T)`.

    val countingdict = \typegen(T::entity) {
        val pairT = tuple(key: T, count: integer)
        extend tuple(private x: list(pairT)) where {
            method add = \mod(k: T) -> nothing {...}
            method count = \() -> integer {...}
            method eltcount = \(v: T) -> integer {...}
            method keys = \() -> list(T) {...}
            method counts = \() -> list(integer) {...}
        }
    }

    val cdictSmallint = extend list(integer) where {
        method add = \mod(key: integer) -> nothing {...}
        method eltcount = \(v: integer) -> integer {...}
        method keys = \() -> list(integer) {...}
        method counts = \() -> list(integer) {...}
    }

CdictSmallint is an implementation of countingdict, only suitable for non-negative small integers but very efficient for those. Use as is, or create an stmapping for it. It allocate a `list(integer)` for each index in the range, starting from 0.
// The `eltcount` operation is O(1) but `count` must search the range.
