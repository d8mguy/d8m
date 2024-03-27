## Term

Module Term provides a `Term` type suitable for parser outputs, and which is used by the lexparse module.

#### Term:type

    val Term = extend tuple(kind: string, arg: ortype(terms: list(Term), strg: string), loc:integer) with {
        method stringify : \() -> string
        method asString = \() -> string
        method asTerms = \() -> list(Term)
        method child0 = \() -> Term
        method child1 = \() -> Term
        method child2 = \() -> Term
        method childN = \(n:integer) -> Term
        method asLeaf = \() -> string       // get the value of a leaf node
        method hasKind = \(wanted:string) -> boolean {...}
        method hasMatch = \(matchfn: \(Term)->boolean) -> boolean {...}
    }

The `asString` and `asTerms` methods interpret `self.arg` as a specific type. These methods are normally applied based on the value of `kind`; if lexparse creates the Term, `kind` is determined by the production being reduced. The four `child` methods interpret self.arg as a list(Term) and return the child Term at the indicated index in the list; `asLeaf` treats child 0 as a string. The `hasKind` and `hasMatch` methods traverse self to determine whether a given kind occurs (for `hasKind`) or the `matchfn` matches on any child element of self.

Access to the Term's `loc` is intended to be accessed directly through the attribute, and is for recording of the location in a parsed string where a Term is found. Since Term isn't provided with a lithook method you must provide a `loc` in the litform creating each Term.
