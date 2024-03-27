## Lininterp

Some linear interpolation code. While linear interpolation itself is a simple and well defined thing, there are quite a few ways to present the specification, deal with boundary conditions, handle slope calculations, etc.

This module is a stub, which provides one way of doing it; my intention is to provide a few variants in the fullness of time. For example, a not yet written algorithm could handle interpolants in any order by storing all slopes and using binary search for indices. 

 

#### type interpFSM

    [interpFSM: xlst, ylst]
where `xlst, ylst: list(float)`. Panic unless `xlst` and `ylst` have same length and that length is > 1.

`InterpFSM` holds state to generate a sequence of float interpolants from a non-decreasing series of `x:float` query values. Only calculates slopes as needed for the presented interpolant `x` values, so may be faster than some algorithms. The `exists` method catches values outside the x range of the interpolable data points; this allows the client to implement its preferred policy. 

    val interpFSM = extend tuple(curinx: integer, xs, ys: list(float), slope:float, noslope:boolean) where {
        method reset = \mod() -> nothing {...}
        method exists = \(x:float) -> boolean {...}
        method next = \mod(x:float) -> float {...}
    }

Method `reset`: Set internal state to same as newly constructed.

Method `exists`: true if self can supply an interpolant at x.

Method `next`: return the interpolant for x and update state, assuming the next `x` given will be >= this one. Assume `self.exists(x)`.

