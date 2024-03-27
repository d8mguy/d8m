## Olist

Module olist provides ordered lists, i.e., lists stored in ascending order on `$<`. These are fairly different from regular lists, and the methods are different from those of list. In fact, olists as done here are really implementing sets, and the module includes an STMap statement so this can happen automatically. The correctness of this depends on assumptions discussed in the next few paragraphs.

There are two typegen definitions here: of `olist` and `olist0`. `Olist0` is for ordered types (types with a `$<` method); it
has a different identifier because typegens don't overload in d8m. The `olist` typegen has 3 arguments: the element type `T`, an ordered type that elements can be extracted to, and a key extraction function `oextract`. Thus, for `olist`, the type `T` is not ordered but `oextract` finds something within it that is. In more standard database language, oextract provides the _key_. 

Now let's talk about assumptions made. First, in d8m we always assume that the `$<` symbol designates a total order (i.e., not a partial order). This is defined to mean that every pair of elements is comparable, that is, for each pair of elements `t1`, `t2` we have `t1 < t2 || t1 == t2 || t2 < t1`. 

Second, in `olist` specifically, we assume that `oextract` is injective when restricted to the domain of any given olist. A function `f` is injective just in case for any `t1`, `t2`, `f(t1)` `==` `f(t2)` implies `t1` `==` `t2`.  

In practice, `olist0` will be used for totally ordered base types like numbers and strings, plus tuple types provided with a `$<` method, while `olist` will be used for tuple types where we don't want to fix the ordering into the type itself, but the tuple type contains one or more ordered attributes that can serve as keys. Consider `accts:list(T)` where `T` has various attributes, including 

    attribute email:string
Now imagine defining `oextract` as

    \(x:T) { x.email }
In database language, we're saying the `email` attribute is the key. And in that mindset, we know that when adding items to `accts`, we need to ensure that the `email` attribute is never duplicated. 

This is precisely what it means in mathematical language to say that `oextract` is injective, when restricted to `accts`. As a tuple type, it's easy to create two distinct items with the same `email` attribute. That means that as a mathematical function, `oextract` is not injective. It's in the nature of tuple types that it cannot be. So the "restricted to `accts`" clause is important here. It means that although the tuple type `T` can easily have distinct elements with the same value of `email`, the collection of `T`'s in `accts` will not. 

It should be easy to see why this condition isn't expressible within d8m's type language (and arguably, isn't a proper thing for type systems to attempt to express at all). D8m's type system is about the structure of entities; this condition concerns constraints on their contents. 

Making this assumption ensures that there cannot be consecutive elements in the olist that map to the same key but are distinct, which assures us that if we're looking for some `x:T` in `accts` and find one whose `oextract` matches at index `k`, then `x` is in `accts` iff `accts[k] == x`. If in addition, we insert into the list only elements that aren't already in it, then we know the list is a set. `Olist` provides an `insertIfUniq` method to make this easy, and generally assumes you don't insert duplicates. However, it doesn't enforce this (for example, by checking for duplication in the `insert` method) for two reasons. First, it's quite common in practice to have code structured so that duplicate insertion is impossible; in this case, embedding a check in `insert` loses performance for no reason. Second, if a duplication check in `insert` fails, the correct response depends on the context &mdash; it might be to panic, to return silently, or return some kind of error message. The solution I've adopted is to explain all this in detail and then make both methods available: `insert` for when the code structure makes duplication impossible; `insertIfUniq` for when a duplicate should be ignored. 

An alternative design choice would be to drop the "injective when restricted" condition. Doing this would weaken one of the really attractive things about olists, which is that the worst case lookup time is `O(lg(n))`. Since you're not guaranteed that the element returned by binary search is the element you're looking for, you must follow each binary search with a linear one that continues until the key changes in the olist. In general, this linear search can be arbitrarily long, and unless you cache the keys, you may be calling `oextract` a lot as well. Not only does this choice reduce the attractiveness of olist as a data representation, it adds very little, since the cases where olist makes sense pretty much always have a workable key. 

#### type olist

    [olist(T, U, oextract): elts]

where `elts: list(T)`.

    val olist = \typegen(T::entity, U::ordered, oextract:\(T)->U) {
        val OLT = olist(T, U, oextract)
        method index = \(oelt: U) -> integer {...}
        method insert = \mod(elt: T) -> list(T) {...}
        method insertIfUniq = \mod(elt: T) -> nothing {...}
        method count = \() -> integer {...}
    	method remove = \mod(elt: T) -> nothing {...}
        method $in = \(elt: T) -> boolean {...}
        method keyIn = \(oelt: U) -> boolean {...}
        method get = \(oelt: U) -> nilPossible(T) {...}
        method getOk = \(oelt: U) -> T {...}
        method before = \(elt: T) -> nilPossible(T) {...}
        method after = \(elt: T) -> nilPossible(T) {...}
        method rvalindex = \(inx: integer) -> T {...}
        method diff = \(other: OLT) -> OLT {...}
        method lithook4Set = \mod(elts: list(T)) -> OLT {...}
        method popb = \mod() -> nilPossible(T)
        method popf = \mod() -> nilPossible(T)
        method minelt = \() -> nilPossible(T)
        method maxelt = \() -> nilPossible(T)
    }

Method `index` provides direct access to what binary search returns. But use it carefully since any insert or remove can change the index.

Method `remove` removes the element, does nothing if element isn't present.

Method `$in` tells if the given elt is in the olist  (only correct if $< is a total order).

Method `keyIn` is like $in but only tests the key.

Method `get` returns the (first) elt whose oextract is oelt, or nil.

Method `getOk` is as get except you know the match will succeed so the return type isn't nilpossible.

Method `before` returns the greatest list elt <= T or nil if either list is empty or T is < the first elt of the list.

Method `after` returns the least list elt >= T or nil if either list is empty or T is > the last elt of the list.

Method `insertIfUniq` returns "unless(elt in self) insert(elt)", hand written because compiler doesn't do it well enough yet.

Method `rvalindex` can be helpful for types that use olists.

Method `diff` produces self - other, ie elts in self and not in other, in the manner of the set operation.

Method `lithook4Set` is defined to support stmapping from set to olist.

Method `popf` removes first element. (Note: nilPossible.)

Method `popb` removes last element. (Note: nilPossible.)

Method `minelt` returns first element. (Note: nilPossible.)

Method `maxelt` returns last element. (Note: nilPossible.)

#### type olist0

    val olist0 = \typegen(T0::ordered) {...}

`Olist0` simply instantiates `olist`:

    val olist0 = \typegen(T0::ordered) { olist(T0, T0, \(t: T0) {t}) }
