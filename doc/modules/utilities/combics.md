## Combics

Combics provides a selection of useful combinatorial algorithms. The idea is to add more definitions over time.

#### enumCombns:function

    val enumCombns : \(n, k, totcount: integer) -> list(list(integer))

`enumCombns` returns a maximum of `totcount` of the combinations of n choose k. For this to make sense, we need `k >= 1` and `k < n`; `enumCombns` panics unless these conditions are met. Each combination is a list(integer) of length k of integers between 0 and n (exclusive); in addition, each combination will have strictly increasing values and the list of combinations is ordered lexicographically.

#### randperm:function

    val randperm : \mod(list(T)) -> list(T) for any T::entity

Randomly permute a list of type `T`, using go package `math/rand` with implicit seed to generate the random numbers. It modifies and returns the list. Note that repeated calls to this function will not generate all permutations of the list without repetition. Note also that since it uses the PRNG state built into `math/rand`, using this may change the behavior of other program elements that also use that PRNG. If this is a problem, you can easily write
a version with its own state; the code is just 8 lines long. The algorithm used here is called the Fisher-Yates shuffle; as explained in the wikipedia, a version was published in 1938 by Fisher and Yates, and popularized in vol 2 of Knuth's The Art of Computer Programming.

#### streamIndices:type

    val streamIndices = extend tuple(cur, limits: list(integer)) with {
        method lithook : \mod(lims: list(integer))
        method eltcount : \() -> integer
        method nDimensions : \() -> integer
        method restart : \mod() -> nothing
        method done = \() -> boolean
        method current = \() -> list(integer)
        method next = \mod() -> list(integer)
    }

A stream type that accepts a list of lengths and generates all the indices up to each of those lengths, in
lexicographic order. Thus, given [1,2,3] it will generate [[0,0,0],[0,0,1],[0,0,2],[0,1,0],[0,1,1],[0,1,2]].
Note that `[streamIndices: ]` is immediately done, it generates nothing. This is the right way to handle the situation where you need to enumerate all the elements of a variable number of lists. It's sensible to think of the k
values that initialize a `streamIndices` as the extents of a k-dimensional cube. From this perspective, the stream generates all vertices of the cube in lexicographic order. A  `streamIndices` will panic if any initializing length
is <= 0 or `next` is called when `done`.

A `stream type` by convention has methods named `current`, `next`, and `done`; you call `next` until `done` becomes true. 