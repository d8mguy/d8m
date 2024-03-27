## Splay

The splay module defines type `splaytree`, generic on any ordered type. As usual, this means totally ordered. Splay
trees to not allow for duplicates, so this is not an appropriate implementation of olists, but is good for sets.

In the algorithms literature, splay trees are presented as a type that's efficient in an average sense for accessing
elements, and yet low overhead compared to some other varieties of balanced search trees. Performance measurements to
date don't suggest that splay trees are typically a good choice overall, being slower than most other options for sets,
even when the element type has a total ordering.

This code is based on code in the `Splay_tree` wikipedia article.
Methods `insert` and `delete` return true if they did something.
The `splayOnAccess` attribute controls aspects of when splay operation is called.
Enumeration is done with eachstart.

An STMap is defined for `set(T)`. 

Although splay trees don't seem to be especially performant, this code has some great examples of using pointers to make linked structures, using nilPossible guards, assertions, purification, etc. Thus, it certainly has some value for learning ways of doing certain things in d8m.

    [splaytree(CT): list(CT)]

    val splaytree = \typegen(CT::ordered) {
        method insert = \mod(v: CT) -> boolean {...}
        method delete = \mod(v: CT) -> boolean {...}
        method present = \imp(other: CT) -> boolean {...}
        method count = \mod() -> integer {...}
        method setSplayOnAccess = \mod(flg: boolean) -> nothing {...}
    }
