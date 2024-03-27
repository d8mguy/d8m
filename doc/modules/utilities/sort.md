## Sort

Sort provides sorting of arbitrary types, both generically and by instantiation. The generic sort works by arranging to call the go sort package. Thus, standard calls to `modsort` and `sort` instantiate a sort algorithm for the particular element type of the list. This code is lifted very directly from the go package code except that type specific methods are used instead of generic ones. Instantiated sorting is quite a bit faster than generic; you can find details in the performance data section.

The advantage of generic sorting is that when sorting small lists, the asymptotic speedup doesn't matter, and each distinct instantiation of the full sorting algorithm results in a non-trivial amount of code, so significant bloat is possible if you're sorting small lists of many different element types. To get generic sorting, you apply a special type generator, called `GSLT` ("generically sortable list type"), that creates list types augmented with the methods required by the go sort package. The sort module defines and enables a rewrite rule that changes calls to `modsort` on list types created by `GSLT` into calls to `Sort` in the go package. (Note that `sort.Sort` sorts its argument in place, just like `modsort`.)

#### modsort: function

    val modsort : \mod(list(OT)) -> list(OT) for any OT::ordered
Modsort is the main entry point for sorting. This version is for lists of ordered elements. Modsort simply calls quickSort.

    val modsort : \mod(list(T), fn:\(T,T)->boolean) -> list(T) for any T::entity
Modsort is the main entry point for sorting. This version is for arbitrary elements, plus a function to compare the elements. Modsort simply calls quickSort.

    val modsort : \mod(list(T), fn:\(T)->OT) -> list(T) for any T::entity
Modsort is the main entry point for sorting. This version is for arbitrary elements, plus a function to extract an ordered element from a list element. Unlike with olist, there is no need for the extracted values to form a total order with resepect to the collection. Modsort simply calls quickSort.

#### sort: function

    val sort : \(list(OT)) -> list(OT) for any OT::ordered
    val sort : \(list(T), fn:\(T,T)->boolean) -> list(T) for any T::entity
    val sort : \(list(T), fn:\(T)->OT) -> list(T) for any T::entity

Sort provides a "pure" version of modsort by making a shallow copy of its argument, calling modsort on it, and returning it. 

#### quickSort: function

    val quickSort : \mod(list(OT), first, last, mxdepth: integer) -> nothing for any OT::ordered
    val quickSort : \mod(list(T), fn:\(T,T)->boolean, first, last, mxdepth: integer) -> nothing for any T::entity

QuickSort sorts `lst[first...last]`, switching between heapSort, insertionSort, and quickSort algorithms based on the size of the sorting interval defined by `last - first`.

#### insertionSort: function

    val insertionSort = \mod(lst:list(OT), first, last: integer) -> nothing {...} for any OT::ordered
    val insertionSort = \mod(lst:list(T), fn:\(T,T)->boolean, first, last: integer) -> nothing {...} for any T::entity

InsertionSort implements the insertion sort algorithm on `lst[first...last]`. It's exported in case you want to use it for something but is mainly used by quickSort.

#### heapSort: function

    val heapSort = \mod(lst:list(OT), first, last: integer) -> nothing {...} for any OT::ordered
    val heapSort = \mod(lst:list(T), fn:\(T,T)->boolean, first, last: integer) -> nothing {...} for any T::entity

HeapSort implements the heap sort algorithm on `lst[first...last]`. It's exported in case you want to use it for something but is mainly used by quickSort.

#### inOrder: function

    val inOrder = \(lst:list(OT)) -> boolean {...} for any OT::ordered

InOrder returns whether the elements of lst are in order (according to `OT`'s ordering).

#### GSLT: typegen

    val GSLT = \typegen(OT::ordered) -> type {...}

GSLT returns a `list(OT)` augmented with methods needed by go's sort package for sorting types generically. The declaration provides effective assertions that ensure correct code generation for such calls. The sort module also defines and activates a rewrite rule that rewrites a call to `modsort` on any GSLT into a call to sort.Sort (in the go package). Therefore, you can get a list sorted generically by creating it as a GSLT, then calling modsort on it. Since it's otherwise a standard list, you can use it in all the usual ways outside of sorting.
