## OlistCached

`OlistCached` is a variant of `olist`; it makes the same assumptions about its extraction function as `olist`; see that documentation for details. What's different is an internal cache named `recent`; the full list in this cache plus the slot named `official`. Code to merge `recent` into `official`, either on demand or when `recent` gets full. There's a fairly huge advantage to this when inserting new elements, since inserting a single element into an olist requires moving on average half of the existing list (assuming elements are entered in random order). With the cache, you insert into a small olist (`recent`) and only change the large olist (`official`) when flushing. This is done in a single pass through `official` which inserts all the new elements in the correct place. 

On the other hand, lookup in general requires searching in two olists, which takes longer than searching in one. If you have a usage pattern with a lot of insertions followed by a lot of lookups, a flush operation at the beginning of the lookups phase should get lookup timing very similar to a regular olist while retaining the advantage of cached insertions. 

Removing elements from an `olistCached` takes as long or longer than removing from an `olist`, unless the removed elements are likely to be in the cache (i.e. recently inserted). Thus, the potential advantage of `olistCached` is mainly for insertion. 

The cache size is part of the type, that is, is an argument to the typegen. This would be easy to change. 

The methods of `olistCached` closely match those of `olist`, with the addition of `flush`. 

#### typegen olistCached

    [olistCached(...): t1, t2, ...]
where T:: entity, and `t1`, `t2`: `T`.

`OlistCached` is enumerable via eachstart.

    val olistCached = \typegen(T::entity, xfn:\(T)->U, thresh: integer) {
        method $in = \(elt: T) -> boolean {...}
        method keyIn = \(oelt: U) -> boolean {...}
        method get = \(oelt: U) -> nilPossible(T) {...}
        method insert = \mod(elt: T) -> nothing {...}
        method insertIfUniq = \mod(elt: T) -> nothing {...}
        method count = \() -> integer {...}
        method remove = \mod(elt: T) -> nothing {...}
        method flush = \mod() -> nothing {...}
        method minelt = \() -> nilPossible(T) {...}
        method maxelt = \() -> nilPossible(T) {...}
        method popf = \mod() -> T {...}
        method popb = \mod() -> T {...}
    }
Method `$in`: this lookup code should be almost as fast as a standard `olist` when `recent` is empty. But if lookups to recent insertions are common, it will be slower than a code that tests `recent` first.

Method `keyIn`: is like `$in` but only tests the key

Method `get`: returns the (first) elt whose `oextract` is `oelt`, or nil.

Method `insert`: inserts `elt` into `recent`, after checking whether `recent.count` `==` `thresh` and flushing it if so.

Method `insertIfUniq`: same as `insert` but checks first.

Method `count`: returns number of elements in both. 

Method `remove`: removes `elt`; no indicator if `elt` not present. 

Method `minelt`: returns minimum element or nil if `[]`.

Method `maxelt`: returns maximum element or nil if `[]`. 

Method `popf`: removes and returns minimum element.

Method `popb`: removes and returns maximum element. 




