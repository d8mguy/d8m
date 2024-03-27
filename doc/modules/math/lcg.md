## Lcg

Module implements a _linear congruential generator_. Recommended for cases where you need a sequence that's "slightly" random but mostly very easy to reset, reuse, etc. Use the math/rand go package for a serious PRNG, or crypto strong ones for relevant applications. Use this to generate sequences that are sort of random but where the key need is to reproduce them easily.

As an example, note that bit 0 of the generator sequence is 0,1,0,1,...; bit 1 is 0,0,1,1,0,0,1,1,... Higher bits appear somewhat more random.

To be slightly more abstract about it, the tradeoff here is simplicity of state versus robustness of randomness. LCG's like the one here have very little state, which makes them trivial to restart. Turns out the mathematicians have figured out that this is also why they aren't very random. Generators like the one in the Go package `math/rand` have a considerable amount of state; they pass tests of randomness much better and are much harder to restart.

The LCG implemented here is from Numerical Recipes; see wikipedia "Linear Congruential Generator".

    val LinCong = extend tuple(curval:integer) where {
        method next = \mod() -> integer {...}
        method cur = \() -> integer {...}
        method reset = \mod(nval:integer) -> nothing {...}
    }

Method `next` steps the generator by one and returns the previous value.

Method `cur` returns `curval`; this is also what a call to `next` would produce.

Method `reset` sets `curval` to `nval`. You use this to reset the generator to any previous value, the initial value, etc. After this call, both `cur` and `next` will return `nval`. 