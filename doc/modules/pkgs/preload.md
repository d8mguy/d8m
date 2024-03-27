## Preload


Preload is not precisely a module; the compiler loads it as part of its initialization sequence. Thus, preload provides definitions that are "built in" but not too deeply. Many of these definitions are 
- important for "d8m style"
- one liners

Thus, it's a great idea to get familiar with the functions defined here.

Preload also defines a set of rewrite rules that are absolutely critical to the optimization process; in some cases programs will actually fail to compile if these rewrite rules aren't present. However, they are not important to understanding how to use the language, and are not documented here.

Note that in general, you can change preload in any way you please. The exception is the rewrite rules, which you should not change unless you know what you're doing. 

#### typegen nilPossible

    val nilPossible = \typegen(T::entity) { ortype(main: T, nil: nil) }

#### typepred ordered

    val ordered = typepred(T::entity, $<:\(T, T)->boolean )

#### function reduce

    val reduce = \imp(lst: list(T), fn:\(T,T)->T, initv: T) -> T {...}

for `T::entity`. This is the single-type version of `reduce`.

#### function accumulate

    val accumulate = \imp(lst: list(T), fn:\(T,T)->T, initv: T) -> list(T) {...}

for `T::entity`. This is `reduce` with the intermediate results recorded in a list. In python it's apparently called `itertools.accumulate`.

#### function split

    val split = \imp(lst: list(T), pred:\(T)->boolean) -> tuple(pos, neg:T) {...}
for `T::entity`. Given a list and a predicate, split the list into pos and neg elts and return as a tuple.

#### function concatPairs

    val concatPairs = \imp(A, B: list(list(T))) -> list(list(T)) -> list(list(T)) {...}
for `T::entity`. Given two lists of lists, return a list concatenating all pairs of them. Aka cross-product of lists.

#### function delta1

    val delta1 = \imp(lst:list(T), fn:\(T, T) -> U) -> list(U) {...}
for `T::entity`, `U::entity`. Generate the list of values returned by the function on successive pairs of shifted values in lst, in other words `[fn(lst[0], lst[1]),` `fn(lst[1], lst[2]),` `...` Will crash if `lst == []`. Can be generalized to larger deltas, more args, subsequences, etc. But gotta stop somewhere.

#### function flatten1

    val flatten1 = \(lst: list(list(L))) -> list(L) {...}
for `L::entity`. Flatten by 1 level.

#### function sigma

    val sigma = \(lst: list(integer)) { reduce(lst, $+, 0) }
    val sigma = \(lst: list(float)) { reduce(lst, $+, 0.0) }
These are the actual definitions. 

#### function min

    val min = \(a, b: OT) { a < b ? a : b }
    val min = \(lst: list(OT), iv: OT) { reduce(lst, min, iv) }
for `OT::ordered`.  These are the actual definitions; you can apply min to ordered things or lists of ordered things. 

#### function max

    val max = \(a, b: OT) { a < b ? b : a }
    val max = \(lst: list(OT), iv: OT) { reduce(lst, max, iv) }
for `OT::ordered`.  These are the actual definitions; you can apply min to ordered things or lists of ordered things. 

#### function inRange

    val inRange = \(x, lo, hi: OT) { lo <= x && x < hi }
for `OT::ordered`. 

#### function inOrder

    val inOrder = \(lst: list(OT)) -> boolean {...}
    val inOrder = \(lst: list(T), fn:\(T,T)->boolean) -> boolean {...}
for `T::entity`, `OT::ordered`. True if elements are non-decreasing with respect to `$<` (for `list(OT)`) or `fn` (for `list(T)`).

#### function all

    val all = \imp(lst: list(boolean)) -> boolean {...}
True if all elements of `lst` are true.

#### function any

    val any = \imp(lst: list(boolean)) -> boolean {...}
True if any elements of `lst` are true.

#### function stringIndex

    val stringIndex = \(strg, ptn: string) -> integer {...}
Search for `ptn` in `strg`; return an index or -1 if not found.

#### function byteIndex

    val byteIndex = \(strg: string, ptn: byte) -> integer {...}
Search for single byte `ptn` in `strg`; return an index or -1 if not found.

#### function to_i

    val to_i = \imp(istrg: string) -> integer {...}
Low level string->integer conversion. Decimal only; handles initial whitespace, + and -, but bails at first non-digit so a non-number will return 0. Therefore, should only be used on strings known to be integers.

#### function to_f

    val to_f = \imp(fstrg: string) -> float {...}
Low level string->float conversion. Uses `fmt.Sscanf` with `%f`. 

#### function to_s

    val to_s = \(flt: float, nfract: integer) -> string {...}
Return a string version of `flt` to `nfract` digits of precision.

#### function to_hex

    val to_hex = \(v: integer) {...}
Return a hex string version of `v`.

