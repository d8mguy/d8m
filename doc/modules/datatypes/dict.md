## Dict

Module dict provides dictionaries, aka associative arrays, as an abstract type. It also defines an STMap to go's map type. Dict uses extensional equality (written as ==) to look up keys. You can define a version of dict that uses === in a few lines of code if you need that.

    [dict(KT,VT): x0]
    [dict(KT,VT): kz, vz]

where `x0:list(pairT)`, `kz:list(KT)`, `vz:list(VT)`.

    val dict = \typegen(KT, VT::entity) {
        val pairT = tuple(key: KT, value: VT)
        extend tuple(...private...) where {
            method count = \() -> integer {...}
            method rvalindex = \(k:KT) -> nilPossible(VT) {...}
            method lvalindex = \mod(k:KT, v:VT) -> nothing {...}
            method remove = \mod(k:KT) -> nothing {...}
            method keys = \() -> list(KT) {...}
            method values = \() -> list(VT) {...}
        }
    }
