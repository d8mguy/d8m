## Set

Module set provides an abstract definition of set and an STMap to go's map type.

    [set(T): lst]

where `lst:list(T)`.

    val set = \typegen(T::entity) {
        val setT = extend tuple(...private...) where {
            method $cvt = \() -> list(T) {...}
            method $in = \(elt: T) -> boolean {...}
            method add = \mod(elt: T) -> nothing {...}
            method add = \mod(other: setT) -> nothing {...}
            method remove = \mod(elt: T) -> nothing {...}
            method remove = \mod(elts: list(T)) -> nothing {...}
            method count = \() -> integer {...}
            method isEmpty = \() -> boolean {...}
            method overlaps = \imp(other: setT) -> boolean {...}
            method $= = \(other: setT) -> boolean {...}
            method subset = \(other: setT) -> boolean {...}
            method intersection = \(other: setT) -> setT {...}
            method $+ = \(other: T) -> setT {...}
            method $+ = \(other: setT) -> setT {...}
        }
    }
