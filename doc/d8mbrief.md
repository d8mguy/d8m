# Fast Path to D8m

D8m is a programming language that claims to introduce some novel and interesting mechanisms for developing programs that
are practical, easy to maintain, run fast, and scale well. D8m wants you to write problem statements in an abstract
form and then _annotate_ them to describe how you want the compiler to handle them. D8m makes functional programming easy but recognizes that it's not always the right answer, so an imperative
programming style is encouraged as long as certain restrictions are observed. 

There is a specifications document for d8m, but like most such documents, motivations aren't the focus. This document is intended to be a fast paced introduction
to d8m that finishes with a set of non-trivial examples. It's aimed at people who know a programming language or three and
who don't mind if a few things are glossed over on a first pass. (Bonus points if one of the programming languages you know
is Go.) Also, there's a lot more discussion of motives, how I expect features to be used, and so on.

## Important Concepts

D8m is a REPL oriented language in that the basic programming model is that the interpreter sees some declarations, which merely change the internal state of the interpreter; then one or
more _queries_, which cause a program to be compiled and (usually) executed. While this is a familiar model, it
has consequences for language design. For example, because the interaction
is "online" rather than batch oriented, it strongly suggests a rule that things must be declared before use. Also, the notion
of query suggests an interactive process that provides an answer, so that building large program structures by constructing
and assembling components requires special verbs. D8m accepts these consequences: it has the declare before use rule, and
the special verbs for constructing more complex "queries". A REPL oriented language gravitates naturally towards what I like
to call "query compilation", in which none of the parts are pre-compiled so that all can be optimized for just the given query.

The conventional wisdom is that recompiling everything all the time is a waste of resources and doesn't scale to very large programs. I take issue with this. Very large programs nowadays run in multiple binary images connected via API through networks. Query compilation acts at the level of single binary images; these are eminently practical to compile from scratch with today's technology. As an example, the d8m compiler is written in go as a single package; it's not far from 30K lines of code and compiles in a couple seconds on a consumer laptop. Furthermore, I feel that compiling from scratch opens up interesting possibilities for both optimizing the program at hand, and designing better program development tools.

D8m uses multibinding (aka "overloading") extensively. Many of d8m's built in functions share a single identifier. Multibinding applies almost exclusively to functions, but it's also possible for types to have the same identifier
as entities in a given scope. Full details are in the spec, and will be mostly glossed over here, but you'll see quickly that
multibinding is used a lot in d8m. I think of "multibinding" as different from "generic". A "generic" function uses a single
definition to cover many types of argument and return types, while "multibinding" is about different functions being bound
to the same identifier. This is quite similar to the distinction sometimes found in the literature between _ad hoc_ and
_parametric_ polymorphism.

D8m uses a little bit of "lookup magic" to make programs more concise and readable. Specifically, the symbol `self` is always
the receiver in methods, the symbol `this` always designates the current element in chain operations, and both of these
can be omitted from attribute references. In other words, where you'd normally have to write `self.x`, you can write `x`, and in the
body of a chain operator, `this.x` can likewise be shortened to `x`.

Being a new programming language is hard. It's like doing a startup except it's guaranteed that you won't get rich. To be
a successful programming language, you need users and to get users you need an ecosystem. You need libraries, IDE's, lots
of documentation, people writing tutorials, discussing on stackoverflow, etc etc. One of the best
decisions I've made in developing d8m is to leverage
Go. Go is surely one of the best new languages of the last decade or so. Its ecosystem is well developed. It's a clean,
well designed language. The compiler is blazingly fast. It's low level enough that a d8m compiler can generate code nearly
as good as going straight to machine level while also providing high level features (closures, garbage collection) essential
to a language like d8m, and that save a lot of work. The tools are opinionated, but they exist and they work well. 

What it means for d8m to leverage Go is:

1. the d8m compiler generates Go, which is then compiled to binary
2. d8m code can import Go packages, and Go packages are the basis for all interaction with the outside world (d8m calls types and functions that interact with the outside world _connected_)
3. certain Go types and concepts that don't fit naturally into d8m's world are nevertheless commandeered so that the "annotation" process can generate a full complement of Go programs
4. various go tools (e.g. for testing, documentation) are inspirational, though not fully developed for the first release.

What this doesn't mean: d8m is not a "pre-processor" for Go. D8m code feels very different, despite both languages claiming
C as a strong syntactic influence. D8m isn't about building systems code at google scale, it's about pushing the metaphor
of separating problem statement (abstract) from solution (concrete). D8m is much more of a functional language than Go, and I think its scheme for generic types is truly excellent. In fact, it's the first programming language that has what I consider
to be true abstract types. And so on.

## Entities and Types

By and large, entities are the things programs operate on when they run, and types are the things that describe them. Types
in d8m are concrete, even the things Go calls interface types are not types in d8m. Entities are things that occupy memory
in a computer; types are things that correspond to a set of entities and methods that act on them.

Types exist at runtime only when a d8m program invokes "dynamic typechecking". The main way to get dynamic TC in d8m is called
`ortype`; this is a tagged set of possible types that a symbol can have at runtime. D8m also has type predicates, which
are quite similar to Go's interface types, and have a mathematical interpretation as just what they sound like: predicates
on types. However, type predicates aren't types, and although it is possible for d8m code to use functions in Go packages
defined with interface types, the default behavior is for generic functions to be instantiated to concrete types at compile time.

D8m's types consist of

| Type                                 | Notes                                             |
| ------------------------------------ | ------------------------------------------------- |
| integer, float, boolean, string, etc | basic types                                       |
| list                                 | ordered collections with lots of built in methods |
| space                                | fixed length collections                          |
| tuple                                | ordered set of attributes                         |
| function                             | pure and stateful are distinguished               |
| extend                               | add methods (and more) to existing types          |
| TG(...)                              | "call" user defined type generators               |

Basic types are usually thought of as having no internal structure, and this is a good attitude for d8m's basic types. Type
generators `list`, `space`, and `tuple` have internal structure identified by their `elttype` (for `list` and `space`), their
`dimensions` (for `space`), or their `attributes` (for `tuple`). Function types are not introduced with a keyword but with
the token `\`, which is short for _lambda_. (To enhance compatibility with the way `\` is commonly used to quote characters
in certain string literals, a practice which d8m adopts, d8m accepts `\\` as an alternate token for introducing function
types and literals.)

One often needs to bind together an identifier and a type, for example when declaring attributes. D8m does this with the
common syntax `X:T` where `X` is the identifier and `T` designates a type. I'll sometimes call this a _binding point_ or _bdgpt_.

Function types consist of a signature with an optional _purity word_; the signature consists of some argument types and a
return type. Purity words are `pure` and `mod`; the default is `pure`. Pure functions act on any kind of arguments and return
any kind of entity. Mod functions act on _moddable types_ which are lists, spaces, and tuples. This means the first argument
of a function with purity word `mod` (the _receiver_) must be moddable. Functions map entities of the given argument types
to entities having the return type. Mod functions may, in addition, change the receiver. Provisions for dynamic typechecking
of these entities are described below. In addition, d8m co-opts the form of _function literals_ to express a number of other
things, which are not functions in the sense of d8m's type system. For example, the syntax of function literals is used to
define type generators and rewrite rules.

Methods are functions in d8m; the only difference is that they're declared as methods, which causes them to participate in
type identity. To define types with methods, you use `extend`. This has a statement-like syntax but designates a type:

    extend basetype where { extendStmt; extendStmt; ... }

where `basetype` designates the type that is being extended. Only a few statement types are allowed in the body of `extend`,
and most of these are not allowed anywhere else. The most important is the `method` statement. If, in the body of an `
extend` statement whose result is bound to `T` we see

    method foo = \(i, j:integer) { ... }

then `T` has a method `foo` whose signature as a function is `\(T, integer, integer) -> U` where `U` is whatever type gets
inferred for the body of the function literal. The `T`-valued argument in the method is always named `self` and `self` is
an identifier that may be omitted from tuple references to make many definitions more readable.

I call methods and attributes _affordances_. The affordances of a type define what you can do with it. You're allowed to
call zero-argument methods without writing `()`; in this case you can't tell whether the code fragment is a function call
or an attribute reference. This is a style issue; it has no significant effect on the expressiveness of the language.

Every type has two built in equality methods named `==` and `===`. (I sometimes write `==` as `$==`; this is the form for
defining your own equality predicate. In d8m,
the `$` character "passivates" tokens, making operator tokens syntactically inactive.) These mean extensional and intensional
equality respectively. Roughly, entities are `==` if all of their components have the same values and `===` if they are the
same object. It's possible to redefine the `==` method for new types, but not the `===` method. Extensional equality differs
from intensional only for moddable types.

There is an equality predicate on types themselves, though it's not part of the language. It's extensional, and type names
have no part in type identity. It takes account of type structure (e.g. attribute names and types, in order) and methods,
but the only thing that matters about methods is their name and signature. The equality predicate on types is obviously relevant
to type checking and type inference; it also determines the limits of multibinding. (In practice, style considerations should
limit multibinding more than technical constraints on inference.)

Here are a few examples of familiar types

- `list(integer)`
- `list(tuple(a:integer, b, c:string))`
- `space(float, [64])`
- `tuple(col:list(list(integer)), inx:integer)`
- `\(integer, integer) -> integer`
- `\mod(tuple(a, b:float)) -> float`

Naturally, there are a few aspects of d8m's type system that are less familiar. The next few short sections cover these.

### Labels and Enumerations

One of the basic types is `label`. It's adopted from the Ruby class Symbol, with minor modifications. Labels are
a self-terminating subset of strings, so their literal form can be signalled with an initial character `:`. This is like
Ruby. The set of strings admitted as labels in d8m is a proper subset of those admitted in Ruby: only identifiers and the
operator tokens of d8m. Thus, `:welrkj` is a label in d8m but `:$elrkj` is not, nor is `:"3!3l4kj;34"`. Also, the Symbol
class in Ruby is an ordinary class, with lots of methods, which all of us are allowed to add to. The label type in d8m has
a very limited set of methods (equality and less than), and one may not define new methods on label.

D8m has labels in part because they're a convenient way to write what Go calls consts. I find that initial `:` a convenient
and comforting way to know that a symbol designates an enumerated constant rather than a variable. This is an abstract type
in the sense that the compiler assigns values to labels, you don't have to worry about it.

The token `label` can also be used as a type generator. So

    label(:a, :b, :c)

designates a type consisting of the three labels with an ordering that matches the definition. I call these types `ELT`'s
for "enumerated label types". Unlike labels, you can define methods on ELTs.

### Ortypes

Ortypes are for dynamic typechecking. Their syntax is similar to `tuple` types. I use different terms for their
components: they are `variants`, not `attributes`; and their names are `tag`'s. Ortypes may not have user defined methods
but they have one built in method `tag` which extracts the tag associated with a given ortype. A declaration like

    var foo: ortype(a:integer, b:string) = 12

creates a symbol that may be assigned an integer or string and initializes it to an integer. Supposing that later in the
same scope, `foo` has been assigned various integer and/or string values, then code like the following

    if(foo.tag = :a) F(foo) else G(foo)

tests whether foo is currently an integer, calling `F` if so, and `G` if not. Importantly, the compiler tracks the dynamic
checking here, so the inferred type of `foo` is `integer` in the call to `F` and `string` in the call to `G`.

This sort of "enhanced" type inference applies in all the ways you'd expect. For example, if you say

    if(foo.tag = :a) break
    G(foo)

the inference at the call to `G` is again string; that inference holds until the end of the loop (since `break` outside a
loop is an error), or until `foo` is assigned to. If the `break` had been `return` the inference would hold until the end
of the function. And so on.

### Nil

Nil (or its friend, null) is found in most if not all programming languages, and plenty of papers have been written about
how confused we all are about it. I think it's fair to say that the original intuition for `nil` is to represent a pointer
that doesn't point to anything, but that, depending on a programmer's intentions, this can be a legitimate value.
That's part of the confusion. D8m doesn't have pointers, though reasoning about the semantics of mod functions
should allow d8m programmers to understand in what cases the d8m compiler must use
pointers in the Go code it generates. (The d8m compiler is also free to use pointers
in other cases, such as when passing a large entity by reference is more efficient
and doesn't change the program's behavior.)
The fact that `nil` can be a legitimate value, even when treated as a pointer value,
provides a lead-in to how it's used in d8m: as a general purpose "exceptional" value,
in the spirit of ML's Option type. (ML introduced
more than one fantastic idea!) Formally, `nil` names a type whose single value is also written as `nil`. As a programmer, you're
supposed to check when a function is capable of producing an exceptional value. For example, you've got a list `L` and some
code that calls its `head` method. That method should not be called on `[]` (the empty list).

Like any programming language, d8m makes choices about how much to attempt to protect
programmers at compile time against wrong use of functions. For example, essentially no languages
that allow you to index a list protect you (at compile time) against out of bounds
indexing because it's impossible to do this in a type system. Now considering
the `head` method again, few languages detect the error of taking the head of an
empty list at compile time. Conceptually, this is because in the type system, an empty
list is indistinguishable from a non-empty one.

D8m is no different in this respect; instead, it defines `head` as returning an `ortype`
of the list element and `nil`. This is such a common pattern that we give it
a name: `nilPossible`. We can even define it in d8m:

    val nilPossible = \typegen(T::entity) { ortype(main:T, nil:nil) }

(We'll talk about type generators in [generics](#generics-and-type-abstraction).)

Head is a generic function on lists:

    val head = \(lst:list(T)) -> nilPossible(T) { if(lst == []) nil else lst[0] }

This is the first function definition we've seen, though the syntactic similarities to the type generator definition above
it are probably obvious. The part after the first `=` is a function literal. We'll talk much more about those in the appropriate
section. (Full disclosure: `head` is actually a built-in method of list, not an independent function.)

Since `nilPossible` is an `ortype` it has "enhanced" type inference. In particular, the `if` statement in the definition
of `head` returns different types in its then and else parts, but that's ok in this case, because they are "merged" into
`nilPossible`. In fact, the type checker is prepared to do this merge for a function whenever the function can return both
`nil` and some other type, call it `T`. That means we don't have to specify the return type in the function literal. We
could have written

    val head = \(lst:list(T)) { if(lst == []) nil else lst[0] }

and gotten exactly the same result.

When using `head` in code, we need to do "the opposite":

    if(lst != []) F(lst.head) else return

The check for empty list changes the inferred type of `lst.head` from `nilPossible(T)` to `T`. (It also works if the condition
is `lst.count == 0`.)

NilPossible is used extensively for lists but it's a perfectly general thing and you can use it in your own types. The effect
of treating `nil` the way d8m does is that the typechecker verifies that you check for exceptional return values. This strengthens
the role of static typechecking and further improves the robustness of code that compiles.

### Miscellany

The identifier `nothing` can be used as the return type of a function. Whether this is really a type is a metaphysical question
at best. It does not designate any entities. Nothing-valued functions are necessarily mod functions, and act like statements. (D8m's `nothing` is pretty much the same as C's `void`.)
Separately, one or more built in functions of d8m cause the program to exit. When such function(s) are called in `if` statements,
type inference on the statement ignores the branch that exits. Internally, d8m uses a type named `exit` to get this behavior,
but it's not available to users.

Go's world of types includes two generic types not seen here: `map` and `channel`. D8m commandeers these as built in type
generators `Gomap` and `Gochannel`. In d8m, types `Gomap` and `Gochannel` may not be used freely; they are not full fledged
types of d8m. The restrictions on their use are found in the spec and elsewhere in this document; the basic intuition is
that you cannot use them when writing your abstract problem statement but can when "annotating" it to say how you want a
query to be translated, or when working with code from go packages.

## Type Predicates

Type predicates identify the constraints on type valued symbols in generic definitions. We've already seen several examples
of this; here, we cover the details and some motivation. There are some built in type predicates (like `entity`) and a `typepred`
"function" that generates new ones. The built in type predicates are the basic type generators (`list`, `tuple`, etc) plus
a small set of others (`entity`, `enumerable`, `moddable`). A `typepred` expression is written like a specially interpreted
function call and designates a type predicate. The form is

    typepred(BT::BP, aff1, aff2, ...)

Here, `BT` is a symbol which designates the type being matched; its scope is the list `aff1`, etc. `BP` is also a symbol,
which must designate a previously defined type predicate, its role is to be the "base predicate". The variadic arguments
`aff1`, etc are a list of (distinct) bdgpts, each naming a function whose first argument is `BT`. The types that match such
a type predicate are those whose methods form a superset of the list `aff1,...`

If this sounds to you like interface types in Go, you're not wrong. However, type predicates aren't types; they're allowed
only in generic definitions and type generators, which we'll cover in more detail in a later section. We'll also discuss
at a later time why d8m doesn't have the equivalent of interface types.

A canonical example of an affordance based type predicate in d8m is

    val ordered = typepred(T::entity, $<:\(T,T)->boolean)

This is true of types whose methods include the binary predicate `<` (written here with the token passivating character `$`).
Assuming all such methods define total orders on their types, this type predicate is a good basis on which to build sorting,
ordered lists, and more complex types that rely on ordering.

## Function Literals

We've seen the function literal form (which I'll sometimes abbreviate to _ftnlit_) in earlier examples. Roughly speaking,
it consists of a function type in which the arguments have names (so they are bdgpts rather than type valued expressions)
followed by a list of statements in braces. In practice, the type checker can usually infer the return type so it is commonly
omitted, but it's always legal, can be helpful for documenting code, and is in fact required when defining recursive functions.
Schematically:

    \purity(a0:T0, a1:T1, ...) { stmt; ... rslt }

The ftnlit form is compact enough that tiny _thunks_ fit easily into argument lists and so on. It's the only syntax in d8m
that designates functions &mdash; the syntax for defining named functions merely binds a ftnlit in exactly the same way as any
other declaration. Like function types, ftnlits accept a purity word, and in ftnlits the word tells not only the
purity but also marks out various generalizations for ftnlits. For ftnlits that designate
functions, `imp` (for _imperative_) is accepted as
well as `pure` and `mod`. An `imp` ftnlit is pure but uses imperative devices (`var` declarations, assignments, calls to
mod functions) in restricted ways that ensure the resulting function is pure "from the outside".

The form of a ftnlit is generally useful to define any transform that's "parameterized" by
some arguments. Accordingly, we generalize its use in d8m to also define

- type generators
- rewrite rules

Both of these are marked with special purity words. Type generators are ftnlits that
return a type, and are marked with `typegen`. Usually, their arguments include type valued generic symbols.
Rewrite rules are ftnlits marked as `xprftn` ("expression valued function"); these provide an extended set of
types and built in functions useful for accessing and rewriting d8m expressions.

## Composite Literals

D8m's moddable type families all use the same form for their literal values: square brackets enclose an optional type followed
by `:` and a comma-separated list of values. The type (aka _type tag_) can be omitted in the following cases:

1. the literal is a `list` whose first element infers to the desired element type
2. the type of the literal is provided by context
3. the literal is "anchor tagged"

The canonical case for "provided by context" is when a literal is the binding of an explicitly typed declaration:

    val tupt1 = tuple(a:string, b:label)
    val foo:tupt1 = ["slkdfj", :ekrj]

Note that you get exactly the same result if you write

    val foo = [tupt1: "slkdfj", :ekrj]

It's more convenient to move the type into context if you need to write a lot of them:

    val foo:list(tupt1) = [["strg1", :lbl1], ["strg2", :lbl2], ...]

"Anchor tagging" is another variation of composite literal form for tuple types, where each attribute's value is preceded
by its name then `~`. So

    val bar = [a~"slkdfj", b~:ekrj]

binds `bar` to the same entity as `foo` in the earlier examples. (To be pedantic: the entity would be extensionally the same
but intensionally distinct, i.e. not the same object.) When the type is supplied (by context or a type tag), an anchor tagged
literal may order its attributes arbitrarily.

This example illustrates the extensional nature of types in d8m &mdash; the fact that the type is named `tupt1` in the earlier
examples makes no difference. Anchor tagged literals are most commonly used for anonymous types &mdash; the type of such literals
can be inferred entirely by the typechecker and need never be explicitly written down in a program. Such things can be useful,
and we'll see examples later.

## Generics and Type Abstraction

As we've seen, the standard way to define a symbol in d8m is with the `val` statement:

    val aSymbol:integer = 12 * anotherSymbol

Since the typechecker can usually infer `integer` from the binding of `aSymbol` in this case, that part is usually omitted.
But formally, we're seeing a _bdgpt_ &mdash; an association of identifier and type &mdash; bound, in this case to an entity, namely
whatever integer corresponds to `12 * anotherSymbol`.

Symbols declared with `val` cannot be assigned to or changed (by calling mod functions on them or modifying their attributes).
The `var` keyword works exactly the same as `val` with this restriction removed. (`Var` declarations are not allowed in `pure`
ftnlits.) Another standard way to declare symbols is in the signature part of a ftnlit. D8m has several additional
contexts in which symbols are declared &mdash; several variable binding operators, for example, which are covered later. Here,
we introduce the `given` statement, which declares _generic symbols_.

There are two flavors of generic symbols, declared with `::` and `:`; the first is _type valued_, and the second _entity
valued_. I'll sometimes write _GTS_ for "generic type symbol" and _GES_ for "generic entity symbol". In `S::T` the right
hand side must designate a type predicate, while in `S:T` it designates a type. Thus, for `S:T` the context tells whether
you're defining a generic symbol or a normal one. The "signature" part of `given` defines generic symbols, while the body
consists of declarations. So

    given(T::entity, OT::ordered) {
      val sort = \(coll:list(T), xfn:\(T)->OT) { ... }
    }

defines a function named `sort` that accepts any list, along with a function that extracts an ordered entity from elements
of that list. (Presumably, this function returns a sorted copy of the list, since it's not a `mod` function.)

After processing the above definition, when the compiler sees a code fragment like

    sort(myList, myFn)

it attempts to match the type of `myList` with `list(T)` for some unknown `T`. If successful, it then attempts to match
`myFn` with a function from `T` to some unknown type `OT` that matches the `ordered` type predicate. If successful, it infers
to the given definition of `sort`. Thus, you never supply types to generic functions; they are inferred.

A `given` statement is always the right way to define a generic function. To define generic types, you'll generally use a
type generator which, as we've already said, is a ftnlit that returns a type. The symbols defined in the signature of such
a ftnlit are interpreted as generic symbols. As with `given`, both `::` and `:` (i.e. GTS and GES) are allowed. Here's an
example that uses both:

    val olist = \typegen(T::entity, U::ordered, oextract:\(T)->U) {
      extend list(T) where { ... }
    }

This type lets you define lists of anything that contains ordered elements or can be ordered in some way that involves combining
them. Presumably, a binary search method is nestled in the `...` above. 

To a first approximation, you now know everything about generics in d8m. Next, we turn to _abstract types_. People sometimes
think these are the same thing. I feel strongly that they are quite distinct. Making types abstract requires precise control over

1. affordances
2. literals
3. enumeration

For example, if you build an abstract type using lists, like you might do for sets, you have to make sure that the methods of
the underlying list are not available for the set. To control the available affordances, you want to start from zero. Tuple
types have no built in methods (except copy). Both methods and attributes can be declared as private. Combining these features allows you
to start from zero, so

    val set = \typegen(T::entity) { extend tuple(private x:list(T)) where { ... } }

defines a type generator named `set`, generic on `T`, whose basetype has (essentially) no affordances. Thus, the affordances
of `set` are precisely the ones defined in the body of the `extend` expression.

The form of composite literals for tuples consists, as we've said, of a list of values in order of their attributes (or in
some other order for the rare case of forms both anchor tagged and type tagged). This approach doesn't work for tuples with private attributes, and isn't flexible enough anyway. In programming language design, the issue here is commonly identified as that of (entity) "constructors".

Instead of having a syntax for constructors, d8m treats methods named `lithook` specially &mdash; as a "hook" for literal forms. There are restrictions on lithook methods due to this role: they may only
be defined on tuple types; they must have `mod` purity and return `self` (even if they have no explicit return
value). They are never called explicitly, instead they are called when a composite literal's arguments match a `lithook` method. Thanks to multibinding, many lithook methods may be defined for any given type, giving plenty
of flexibility to entity construction.

Lithook methods can be defined on _any_ tuple type; there is no requirement that it have private attributes. Consequently,
one can easily set things up for default values of certain attributes, to initialize invariants associated with a type, and
all the other things that type constructors usually do. This convenience is not available for list and space types, but it's
easy enough to wrap a list type in a tuple type, as was just shown for defining sets. You should not assume that a tuple
type with a single attribute translates into a Go struct type. In fact, you should probably assume it does not.

So far we've covered affordances and literals; next we turn to enumeration. D8m handles `enumerable` as a type predicate rather than a type. D8m has a single enumeration primitive, the `each` statement,
which is described in more detail later. What it means for `X` to be `enumerable` is that `X` can be the thing enumerated
in an `each` statement. All `list` and `space` types are enumerable, and D8m provides two ways for abstract types to be made enumerable. The first works for tuple types and
involves coopting an enumerable attribute to enumerate the collection. This works for the definition of `set` sketched above --
it's safe to enumerate the list containing the set's elements, since `each` cannot modify the collection being enumerated.
The second involves what I call the _eachstart protocol_. This is a way of providing a bit of state plus methods to access
the current element, get the next element, and test for doneness. These can be combined in the obvious way to rewrite an `each`
statement enumerating an entity that uses the eachstart protocol into a loop that does the right thing. Specifically, we rewrite

    each(x^coll) { body }

to

    var cntl = coll.eachstart
    loop {
      if(cntl.eachdone) break
      val x = cntl.value
      body
      coll.eachstep
    }

While I claim that the mechanisms so briefly discussed in the last few paragraphs let you define abstract types cleanly and
clearly, my claims about type abstraction go further. I claim, in fact, that a type is not abstract if it always translates
the same way in the compiled program. The mathematical concept of set is very abstract, indeed, more abstract than is needed
for computing. But a programming language with a type called `set` that always generates a hash table actually has built
in hash tables, not sets. Sets can be represented in a million ways &mdash; with hash tables, ordered arrays, splay trees, bit
arrays, and on and on. If your "set" type actually means hash table and you're a good programmer, you're going to use it
just in the cases where a hash table is an appropriate implementation. Before it can sensibly promote writing code abstractly,
a language needs to break the link between source level types and their target level mappings. D8m lets you define mappings
with the STMap statement, and apply the resulting mappings selectively. This is described in more detail later.

Earlier, I promised more discussion of Go's interface types and why d8m's dynamic typechecking works differently. My view
on this is that Go's interface types are best understood as being of two kinds: `interface{}` and everything else. Go's type
`interface{}` (aka the _empty interface_) is suitable for type switch statements that correspond to what you do with d8m's
ortypes. The interface types with methods are most useful for _dynamic method lookup_, which you need because packages are
not only a way of structuring large programs, but are _compilation units_. Thus, when you import the `sort` package, whose
`Interface` type requires 3 methods, you're directing the Go compiler to link its _binary code_ into your executable. Since
the code is already compiled, dynamic method lookup is the only way to handle its generic features (i.e., the 3 methods in
`sort.Interface`).

D8m doesn't have packages, because I view the issue of identifying compilation units to be not properly a part of stating
problems abstractly but rather as something that should be done by the compiler, possibly with some help (input from the
user, by measuring running programs, tracking change frequency of parts of them, etc). (Aside: d8m _does_ have _modules_,
which are simply files of declarations with an `export` list. These absolutely do not constitute compilation units, and unfortunately,
the term conflicts with a feature of Go concerning a significantly more complicated concept.)

A d8m compiler __may__ instantiate generic types and functions in code to type-specific ones.
The d8m compiler I've developed always does this. 
One can criticize this as potentially leading to code bloat. I agree to some extent, but 
the key point here is that this is a compiler policy, not a property of the language. Nothing in d8m's semantics requires generic functions to map to instantiated
ones in the back end. The current d8m compiler can avoid instantiation in limited cases, under user control; a specific example is the sort module. 
Go's sort package is generic; by making a type declaration and enabling a rewrite rule, d8m programs can access that sort package instead of instantiating sort code for each different element type that needs to be sorted in a program. 
A more sophisticated compiler might gather information about collection sizes and frequency of use and make these declarations itself. An even more sophisticated one might be able to synthesize the interface methods that allow a particular algorithm to be run as generic.


## Chain Operators

An important argument people make for functional programming is that applying functional operators lets you express operations
on collections concisely and makes them more readable. The concept of "list comprehension" is a currently popular term for
one incarnation of this idea. D8m's approach to this area is simple and powerful. A first illustration is drawn from the spec. Consider

    val place = tuple(centroid:geocoord, population:integer, ...)
    val distance = \(a,b:geocoord)->float { ... }
    val sumpop = \(terrains:list(place), src:place, radius:float) {
      sigma(terrains.[ distance(src.centroid, this.centroid) <= radius ].{ population })
    }

Now `sumpop` is a function that designates the sum of the populations of those `terrains` whose `centroid` is within `radius`
distance of the centroid of `src`.

The expression constituting the function body reduces a list defined by a chain consisting of three expressions separated
by two dots. The reduction happens with `sigma`, which is defined by `reduce`, that prototypical functional programming function,
over the `plus` operator. The dot chain first filters `terrains` to produce a list of just the elements with distance less
than radius, then transforms it by extracting the population of each such element. I said at the beginning of this document
that `this` gets special lookup handling in chain operations. The definition I just gave of `sumpop` uses `this` in one place
(before `centroid`) and omits it in another (before `population`). The "official" interpretation of `population` is `this.population`.

A traditional programmer, worried about efficiency, might have written instead

    val sumpop = \imp(terrains:list(place), src:place, radius:float) {
      var sum = 0
      each(t^terrains) {
        if(distance(src.centroid, t.centroid) < radius) sum += t.population
      }
      sum
    }

This is precisely what the d8m compiler produces. In fact, the optimizations that transform the "functional" form into the
"imperative" one aren't even built into the compiler, they are expressed with rewrite rules ("xprftns"), which are discussed later.

There are two chain operators, known informally as _dotbrace_ and _dotbracket_. Dotbrace is written `lst.{expr}` and transforms
`lst` according to `expr`, mapping a `list(T)` to a `list(U)`. Dotbracket is written `lst.[expr]` and filters `lst` to just
the elements for which `expr` is true, thereby mapping a `list(T)` to a `list(T)`. Actually, these operators work on any
enumerable collection because they only enumerate their first argument and d8m has a rule (the "enum only" rule) stating
that any function in which a `list(T)` occurs as an argument and such that that argument is only enumerated in the function
body will typecheck with _any_ enumerable type in that argument position. (Note that the enum only rule does not affect the
return type, only the argument. So `s.{xfm}` if `s:set(T)` works but generates a `list(U)` rather than a `set(U)`. It is
possible to get a `set(U)` by defining a certain set method, as we'll discuss momentarily.)

Although it isn't needed for a high level use of the language, most d8m programmers will want to understand how the chain
operators work "under the covers". What happens is that `lst.{expr}` is rewritten to

    maplst(lst, \(this:T, index:integer) { expr })

where `T` is the inferred elttype of `lst` and `maplst` is a built in generic function defined as follows:

    given(T, U::entity) val maplst = \imp(lst:list(T), fn:\(T, integer)->U) {
      var rslt:list(U) = [], index = 0
      each(x^lst) { rslt.pushb(fn(x, index)); index += 1 }
      rslt
    }

Similarly, `lst.[expr]` is rewritten to

    filt(lst, \(this:T, index:integer) { expr })

While these are the most important chain operations, the full picture involves a few more things. First, the `index` argument
to `maplst` and `filt` can be used internally, as in

    lst.[index >= 5]

which lops off the first 5 elements of `lst`. But so far, there's no way to get the list of indices that match a predicate.
To do that, we add a new function `filtx`, with a special bit of syntax. So

    lst.[this > 17 => index]

returns the indices of the `lst` elements greater than 17. Formally, the form `lst.[pred => xfm]` is rewritten to

    filtx(lst, \(this:T, index:integer) { pred }, \(this:T, index:integer) { xfm })

where `filtx` is a function that applies the `xfm` when the `pred` matches and accumulates the result in a list.

Second, we need a few ways to terminate a chain. One option is that we want a list at the end, in which case, a chain of
`maplst` and `filt` suffices. More commonly, we want to extract something from a list, in which case we can use `reduce`
or one of the built in functions based on it. Various short circuiting reductions also come in handy, such as finding the
first element of a list that matches a predicate. We call this `filt1` and give it the same syntax as indexing:

    lst[this > 17]

returns the first element of `lst` greater than 17 or nil, if there is no such element. Thus, the return type of `filt1`
is `nilPossible`:

    val filt1 = \imp(lst:list(T), fn:\(this:T, index:integer)) {
    	each(x^lst) if(fn(x, index)) return x
    	nil
    }

This rewrite is done only if the "index" argument infers to `boolean`. Otherwise, `x[y]` rewrites to `rvalindex(x, y)`. This
means you can capture indexing syntax for an abstract type by defining an `rvalindex` method for it. Similarly, `x[y] = z`
rewrites to `lvalindex(x, y, z)`.

It's also possible to extract the index of the first element matching a predicate with the same syntax as for dotbracket:

    lst[this > 17 => index]

does this. This rewrites to a built in function called `filt1x`, which is also nilPossible and capable of an arbitrary transform.

If we want multiple distinct things from a chain, the easiest way to write it is often to duplicate the chain. For example
the min and max of the squares of a list(float) can be conveniently written

    val mn = lst.{this*this}.min
    val mx = lst.{this*this}.max

A reasonable compiler should merge these into a single enumeration of `lst`.

The `reduce` function is built in but only in the sense that its definition is read from a known file when the compiler initializes; the definition reads as follows:

    given(T::entity) {
      val reduce = \imp(lst:list(T), fn:\(T, T)->T, initv:T) {
        var rslt = initv
        each(x^lst) rslt = fn(rslt, x)
        rslt
      }
    }

There are useful variants for defining reduce. For example, one can make it so `fn` accumulates into something. For example, given

    given(T::entity, U::moddable) {
      val reduce = \mod(acc:U, lst:list(T), fn:\mod(U, T)->nothing) { each(x^lst) fn(acc, x) }
    }

you could define `statsT = tuple(lo, hi:float)` and lst:list(float), and then write

    var stats:statsT = [SmallestFloat, LargestFloat]
    reduce(stats, lst, \mod(acc:stats, x:float) { acc.lo = min(acc.lo, x); acc.hi = max(acc.hi, x) })

to accumulate the `min` of the list into `stats.lo` and the `max` into `stats.hi`.

One of the morals of this story is that `reduce` is seen as one thing, but ought to be seen as a family of (generic) functions.
As just shown, it takes only a few lines of code to define a new one, and the results are optimized just as well as the built
in function. Some of these might need to be more specialized, such as when you want reduction with a "short circuit". Consider:

    val all = \(lst:list(boolean)) { reduce(lst, \(x,y:boolean) { x && y }, true) }

The intention here is that `all` is true just in case it's true for every element of `lst`. So you can write things like

    all(lst.{this > 100})

to find out if every element of `lst` is greater than 100. The problem is that this doesn't work the way you'd like: every
element of `lst` is evaluated, and `reduce` doesn't short circuit. To fix this, use a more direct definition of `all`:

    val all = \imp(lst:list(boolean)) {
        var rslt = true
        each(x^lst) unless(x) { rslt = false; break }
        rslt
    }

Similarly, one can test existence of a matching element with

    val any = \imp(lst:list(boolean)) {
        var rslt = false
        each(x^lst) if(x) { rslt = true; break }
        rslt
    }

Beyond treating `reduce` as a customizable family of functions, one can use knowledge of how chain operations work to customize
the treatment of abstract types. For example, I mentioned earlier that chain operations automatically work on sets but generate
lists. If you want dotbrace on sets to generate sets, you can use a type generator for set that defines `maplst` as a set-
valued method of set; it will restore the "no duplicates" property and return a new set.

D8m has a special built in function `xp` for transposing between tuples of lists and lists of tuples. This gives an elegant
solution to the problem of enumerating multiple collections in parallel. For example, given a function
`f:\(integer, string)->boolean` and two lists: `li:list(integer)` and `ls:list(string)`, you can run `f` on them in
parallel by writing

    [int~li, strg~ls].xp.{ f(int, strg) }

Here, xp isn't a chain operator, despite occurring between two dots. A more official way of writing this is

    xp([int~li, strg~ls]).{ f(this.int, this.strg) }

In other words, we call the `xp` function on the anchor tagged literal whose type is `tuple(int:list(integer), strg:list(string))`.
Its return type is `list(tuple(int:integer, strg:string))`. That list goes to dotbrace, where the attributes of `this` are
passed to `f`. The `xp` function is formally an infinite collection of functions all bound to the same identifier under d8m's
multibinding rules. Thus, it applies to tuples with any number of attributes having arbitrary names and types. It is defined
so that the result's length is that of the shortest list in the attributes of its argument.

## Modifying State

D8m takes what I think is a quite unusual position on state &mdash; the language fully supports it but significantly restricts
how and when stateful things can be modified. Imp functions can modify locally declared state and assign to var symbols,
while mod functions can in addition modify their first argument, which is the receiver when the function is a method. State
modification arises in tuples by assigning to attributes or calling mod functions on them; it arises on lists and spaces
only by calling mod functions, which always depend ultimately on their built in mod functions. In general, it takes data
flow analysis to enforce these rules.

These rules require you to think more carefully than you might do naturally about what entities are stateful and how they
change. In some cases, you'll need to define mod methods where you might normally be inclined to "reach into" a tuple and
change something about it. I have found the rules to be more helpful than not in writing readable, maintainable code, but
I guess the jury is out until quite a bit more code has been written.

At a more philosophical level, I think state is a wonderful and deeply intuitive concept when used appropriately, so I am
not especially sympathetic to attempts at banishing it. I also agree with those who claim that state modification can be
seriously abused in program design. My intuition is that controlling it is an appropriate response. Whether d8m's rules are
"just right" is hard to say. I have proven no interesting mathematical results about them, and I'm not sure that's a useful
avenue to pursue. My coding experience so far is positive.

## Assertions

D8m's `assert` statement is also unusual. It makes a real assertion, as in logic, one that involves implicit quantification
as explained in the spec. Since d8m is a programming language, the spec also says precisely what limited things a compiler
is required to do with assertions. One of these involves "effective assert predicates", which are essentially compiler directives.
More interesting are assertions that affect typechecking of ortypes (especially of `nilPossible` ones) and the requirement
that asserted things should be _manifest_. This means that infrastructure that depends on whether something is manifest can
be influenced by assertions, and also that compiler inferencing capabilities over certain rather simple things (such as ordering
operators) can be unreasonably effective in increasing the utility of assertions. We'll see examples later in this document.

In connection with the third effective use of assertions, there is a built in function

    val manifest:\(boolean)->boolean

which is always evaluated at compile time and is false unless its argument is known to be true at compile time. Therefore,
`manifest(X)` will be true in any context where `assert X` is active. (Of course, it can be true for other reasons as well.)

## Nuts and Bolts

At this point, we've covered most of the facts about d8m that are relevant to making abstract problem statements. In this
section, we discuss the control flow statements, the built in methods for the most important types, and a few other built
in functions that are good to know about. With those details out of the way, we can launch into larger examples that give
more of a flavor of how d8m can be used to solve real problems.

The control flow statements are by and large lifted straight from C; here's a summary, full details are in the spec. The
following keywords introduce statements whose syntax and semantics are the same as C: `if`, `while`, `break`, `return`, and
`continue`. D8m adds `loop`, which is essentially `while` without the condition, and
`unless`, which is `if` with the condition inverted. C's `?:` expression works too. D8m has a `case` statement that is somewhat
different from C's `switch` statement. D8m does not have the equivalent of C's `for` statement. Instead, d8m's `each` statement
is intended for all enumerations of collections; as mentioned earlier, it's tightly linked with the (built in) `enumerable`
type predicate. The syntax of `each` is

    each(iv^coll, ...) body

where `iv` is a symbol, the "iteration variable", `coll` is an expression designating some enumerable thing, and `body` is
either a statement or a `{}` enclosed statement list. The `...` in the above schema indicates that `each` can have multiple iterators.

    each(i^L, j^M) body

is equivalent to

    each(i^L) each(j^M) body

The iteration variable's scope is `body`. As you can infer from the equivalence, the second iterator in `each` can depend
on the first. It's also possible to name a symbol that holds the index in the enumeration. For example, in

    each(i^L, sym) body

`sym` is the index variable.

All of `each`, `loop`, and `while` are nothing-valued. The `if`, `unless`, and `case` statements may be expressions if all
of their clauses are present and infer to compatible types. Otherwise, they too are nothing-valued. For example,
`if(P(x)) M(x)` is nothing-valued since it has no else.

The built in `..` and `...` operators provide numeric ranges, respectively inclusive and exclusive. Formally, they are treated
as mapping to tuple types that automatically convert to lists on demand, so that `0..5` is formally equivalent to the list
whose literal form is `[0,1,2,3,4,5]`.

There are a small number of built in operators that are called like functions but involve various kinds of special handling,
usually because of type-valued arguments. One is `cvt`, short for _convert_, and written

    cvt(expr, type)

The expression `cvt(X, T)` is treated as calling a function `cvt` with signature `\(XT)->T` where `XT` is the type inferred
for `X`. This uses the multibinding rules. Thus, the semantics of `cvt` is depends entirely on user defined functions. One
defines a `cvt` function in the normal way:

    val cvt = \(x:XT)->T { ... }

The standard definitions of `cvt` to `string` are used in string interpolation, which is discussed shortly.

`Always` is a variable binding operator that links an assertion with a value. Written

    always(x~expr, P(x))

where `P` is a predicate, `always` is rewritten to

    \(x:T) { assert P(x); x }(expr)

where `T` is the type inferred for `expr`. As was mentioned in the last section, `assert` is only required to have operational
effect in limited cases, including with `nilPossible` values. So an idiomatic use of `always` is

    always(x~expr, x != nil)

which produces a non-nilPossible version of `expr`.

The idea with lists is to have a fairly complete set of built in methods. Quite a few of the methods found in other list
oriented languages can be done so concisely with chain operators that there's no need to define methods for them. The built
in methods of list are in the following table (`NP(x)` is short for `nilPossible`):

| Name        | Signature                                                           | Notes                                                                                                                                              |
| ----------- | ------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| `select`    | `\(lo, hi:integer) -> list(T)`                                      | new list of elements in given (exclusive) range                                                                                                    |
| `count`     | `\() -> integer`                                                    | length                                                                                                                                             |
| `index`     | `\(elt:T) -> NP(integer)`                                           | index of first occurrence of `elt`, else `nil`                                                                                                     |
| `head`      | `\() -> NP(integer)`                                                | first element of list, `nil` if empty                                                                                                              |
| `tail`      | `\() -> NP(list(T))`                                                | rest of list, `nil` if empty                                                                                                                       |
| `last`      | `\() -> NP(T)`                                                      | last element, `nil` if empty                                                                                                                       |
| `reverse`   | `\() -> list(T)`                                                    | new list with elements in reverse order                                                                                                            |
| `rvalindex` | `\(i:integer) -> T`                                                 | get ith element, written `lst[i]`                                                                                                                  |
| `lvalindex` | `\mod(i:integer, v:T) -> nothing`                                   | change ith element, written `lst[i] = v`                                                                                                           |
| `replace`   | `\mod(ofs:integer, other:list(T)) -> nothing`                       | generalizes lvalindex to replace multiple elements starting at ofs with elts of other; panic if ofs > self.count; otherwise extends self as needed |
| `copy`      | `\()->list(T)`                                                      | shallow copy of `self`                                                                                                                             |
| `copy`      | `\mod(src, dest, len:integer) -> nothing`                           | move elements within a list: from `src` to `dest` for `len` elements; all of `src`, `dest`, `src+len`, and `dest+len` must be < `self.count`       |
| `insert`    | `\mod(i:integer, nv:T) -> list(T)`                                  | insert nv at index i and push following elements up 1                                                                                              |
| `insert`    | `\mod(i:integer, other:list(T)) -> list(T)`                         | insert (all of) other at index i and push following elements up length of other                                                                    |
| `removeElt` | `\mod(e:T) -> nothing`                                              | mod self by removing first occurrence of e (via `$=`); no effect if `e` not found                                                                  |
| `removeInx` | `\mod(i:integer) -> nothing`                                        | mod self by removing element at index `i`; panic if `i` out of range                                                                               |
| `removeSeq` | `\mod(lo, hi:integer) -> list(T)`                                   | remove elements between `lo` and `hi`, returning self; panic if `lo > hi`                                                                          |
| `swap`      | `\mod(i0, i1:integer) -> nothing`                                   | swap elements at `i`, `j`; panic if either out of range                                                                                            |
| `maplst`    | `\(fn:\(T, integer) -> U) -> list(U)`                               | rewritten from chain operator                                                                                                                      |
| `filt`      | `\(fn:\(T, integer) -> boolean) -> list(T)`                         | rewritten from chain operator                                                                                                                      |
| `filtx`     | `\(fn:\(T, integer) -> boolean, xfm:\(T, integer) -> U) -> list(U)` | rewritten from chain operator                                                                                                                      |
| `filt1`     | `\(fn:\(T, integer) -> boolean) -> NP(T)`                           | rewritten from chain operator                                                                                                                      |
| `filt1x`    | `\(fn:\(T, integer) -> boolean, xfm:\(T, integer) -> U) -> NP(U)`   | rewritten from chain operator                                                                                                                      |
| `$in`       | `\(lst:list(T), elt:T) -> boolean`                                  | true if `elt` is in `lst`                                                                                                                          |
| `$+`        | `\(other:list(T)) -> list(T)`                                       | new list with other appended to self                                                                                                               |
| `$+`        | `\(other:T) -> list(T)`                                             | new list with other appended to self (aka non-mod version of pushb)                                                                                |
| `pushf`     | `\mod(elt:T) -> list(T)`                                            | mod self by pushing elt onto front of self; return self                                                                                            |
| `pushb`     | `\mod(elt:T) -> list(T)`                                            | mod self by adding elt after last elt of self; return self                                                                                         |
| `popf`      | `\mod() -> NP(T)`                                                   | mod self by popping first elt                                                                                                                      |
| `popb`      | `\mod() -> NP(T)`                                                   | mod self by popping last elt                                                                                                                       |
| `uniq`      | `\() -> integer`                                                    | new list consisting of distinct elements of self in order                                                                                          |
| `contains`  | `\other:list(T)) -> boolean`                                        | true if all elements of other are also elements of self                                                                                            |
| `disjoint`  | `\other:list(T)) -> boolean`                                        | true if no element of self equals any element of other                                                                                             |

With `$+` and `insert` one sees multibinding in action. With `remove`, it works better to have different symbols for "by
index" and "by element", especially when considering `list(integer)`.

The built in methods on string include (lexicographical) ordering (`$<` and the like), substring selection, concatenation,
and a few more. In addition, strings written in double quotes admit _interpolation_. As with the label type, this is in the
style of Ruby. So `"something #{x + y}"` designates the string "something " concatenated with the string that results from
evaluating `x + y` and converting it to a string, using `cvt`. This feature is a property of literal strings, not the `string` type.

# Examples

We present two abstract types, a moderately complex coding task that doesn't simplify much with functional programming techniques
but that is still tolerably compact in d8m, and finally some "concrete" types. All of these examples are well described as
providing infrastructure for the goal of making abstract statements of problems.

D8m's built in types are minimal, so user defined types like set and dict will be used in essentially every non-trivial program.
Normally, one just says "import" to use them, but the basic definitions are simple enough that project-specific variations
are not at all out of the question.

## Sets

The standard definition I've been using for set is quite compact; here's the whole thing:

    val set = \(T::entity) {
      val setT = extend tuple(private x:list(T)) where {
        assert enumerable(x)
        method lithook = \mod(c:list(T)) { x = c.uniq }
        method $cvt = \() -> list(T) { copy(x) }
        method $in = \(elt:T) { elt in x }
        method add = \mod(elt:T) { unless(elt in x) x.pushb(elt) }
        method add = \mod(other:setT) { each(o^other) self.add(o) }
        method remove = \mod(elt:T) { val inx = x.index(elt); unless(inx == nil) x.remove(inx) }
        method remove = \mod(elts:list(T)) { each(e^elts) self.remove(e) }
        method count = \() { x.count }
        method isEmpty = \() { x.count == 0 }
        method $== = \(other:setT) -> boolean { other.count == x.count && all(x.{this in other}) }
        method subset = \(other:setT) { all(x.{this in other}) }
        method intersect = \imp(other:setT) { var rslt:setT = []; each(o^other) if(o in x) rslt.add(o); rslt }
        method union = \imp(other:setT) { var rslt:setT = copy(self); each(o^other) rslt.add(o); rslt }
      }
      setT
    }

The type generator body defines a local type `setT` so that it can refer to it in various methods that involve sets. Then
it returns that type. The `extend` expression stores the elements in a (private) list named `x` then makes an assertion and
defines 13 methods. The assertion says that a set can be enumerated by enumerating `x`. The methods concern creating a set,
converting a set to a list (using the built in `copy` function), testing whether an element is in a set, adding or removing
an element or another set to/from the set (as mod methods), getting the length of the set, testing for empty set, redefining
(extensional) equality, testing whether self is a subset of another set, generating the intersection or union of two sets.
These are _all_ one-liners! This is possible because we're only trying to define sets abstractly, with absolutely no optimizations.
Of course, the code shown here will run, and is appropriate for small sets.

It's worth noting that some of these methods can be synthesized from others. There's a minimal set (`lithook`, `$in`, `add`
and `remove` one element, `count`, `$=`); the others are defined in terms of these. That's relevant when we define STMaps
that implement sets &mdash; applied STMaps need only define how to translate the minimal methods; the compiler will automatically
translate the others.

## Dictionaries

Dictionaries are a similar story to sets. Here's the standard definition:

    val dict = \(T1, T2::entity) {
      val pairT = tuple(key:T1, value:T2)
      extend tuple(private x:list(pairT)) where {
        assert x.count == x.{key}.uniq.count
        assert enumerable(x)
        method lithook = \mod(x0:list(pairT)) { x = x0 }
        method lithook = \mod(kz:list(T1), vz:list(T2)) { x = [k~kz, v~vz].xp.{ [key~k, value~v] } }
        method count = \() { x.count }
        method rvalindex = \(k:T1) { val tmp = x[key == k]; tmp = nil ? nil : tmp.value }
        method lvalindex = \mod(k:T1, v:T2) {
          var inx = x[key == k => index]
          if(inx == nil) x.pushb([key~k, value~v]) else x[inx].value = v
        }
        method remove = \mod(k:T1) {
          val inx = x[key == k => index]
          unless(inx == nil) x.removeInx(inx)
        }
        method keys = \() { x.{key} }
        method values = \() { x.{value} }
      }
    }

Here, we define an auxiliary type with attributes named `key` and `value`. The `extend` expression starts with two `assert`
statements, the first of which is likely useful only for documentation, though it could possibly be of use to an advanced
compiler. The second assert tells how to enumerate a `dict`, which clarifies that such an enumeration produces key-value
tuples. Next, there are two `lithook` methods, which let you create a dict from a list of key-value pairs or a pair of lists.
(The second uses the "xp trick" shown earlier.) The `rvalindex` and `lvalindex` methods correspond respectively to indexing
and indexing on left side of assignment. In a dict, `rvalindex` looks for a value corresponding to the key, and `lvalindex`
adds a new key-value pair or changes the value of an existing one.

The fact that type identity is extensional comes in handy here &mdash; the `pairT` name does not need to be exported or remembered;
the only fact a programmer needs to remember is that the attribute names are `key` and `value`.

As with `reduce`, I find that `dict` is best understood as a family of types rather than a single one. I find "counting dictionaries"
useful (dicts whose value is a count of the number of times a key was seen); also "accumulating dictionaries" (where the
value records a list or set of occurrences). These can be defined in just a few lines of code.

Types like `dict` are most closely associated with hash tables among all the "high level" types found in modern programming
languages. There are many flavors of hash table, none optimal for all cases. But since hash code can be complicated and the
performance differences are often not that great, extensive experimentation with hash table flavors is generally not a great
investment of programmer time. The bigger issue with hash tables is that some types aren't good candidates as keys. D8m gives
choices &mdash; ordered lists or other implementation techniques are sometimes appropriate.

## A Date/Time Module

One of the dirty secrets of functional programming is that although you can often express operations on collections very
concisely and clearly, the functional operators don't help so much when you're dealing with complex state machines, specs
with numerous exceptions, and so on. This section goes through some d8m code to represent, parse, and print dates and times
in a wide variety of formats. Although both external and internal forms for such things are the subject of numerous standards,
I think it's fair to say there's little standardization in their representation. One reason is that time is a rather complex
subject. Although many units are related by constant factors (minutes, hours, days, weeks, microseconds, ...), we also have
badly behaved ones: months, quarters, years, leap-years, leap-seconds. Not only are these units hard to work with, they show
wildly varying precision requirements. Few libraries I'm aware of make it easy to record different precisions, incommensurable
units, etc. The module shown here does this; it also makes it easy to parse and compactly maintain a column of dates that
are all in the same format. We start with an outline of the type:

    val datetime = extend tuple(baseunit:label, baseOffset:integer, 
      firstcode, sep:label, mn1:boolean, errstrg:string) where {
        method lithook = \mod(mnfirst:boolean) {...}
        method fromString = \mod(s:string) {...}
        method toString = \imp(ofs:integer) {...}
        ...
    }

The key methods we'll focus on are here called `fromString` and `toString`. A complete module will likely add others, for
example methods that calculate differences between dates in various units. The attributes of the base type store precision
and formatting information (in `baseunit`, `firstcode`, `sep`, and `mn1`), plus an internal representation of the datetime
in `baseOffset`. The `errstrg` attribute provides a way to record the reason for an error return from `fromString`.
The `lithook` method initializes all attributes to default/unknown values except `mn1`, since the order of month and day
fields (EU or US conventions) are not reliably detectable from a string form date.

For tighter typechecking, we could declare many of the attributes with ELTs instead of general labels. Here are the equivalent
ELTs used in the code:

| attribute | ELT                                               | Notes                                                      |
| --------- | ------------------------------------------------- | ---------------------------------------------------------- |
| baseunit  | `label(:unknown, :second, :minute, :day, :month)` | initially `:unknown`                                       |
| firstcode | `label(:dg4, :dgsh, :monnm, :wknm)`               | 4 digits, 1 or 2 digits, month and week names (in English) |
| sep       | `label(:none, :dash, :slash, :colon)`             | `:colon` for time-only strings                             |

The fromString method uses a simple tokenizer named `tknstreamer` whose `next` method returns a label that initially goes
into `firstcode`. Here's that definition:

    import "bytes" as bb

    val monthnames = ["jan", ...]
    val weeknames = ["mon", ...]
    val singletons = "+-,:/"
    val snglClasses = [:plus, :dash, :comma, :colon, :slash]

    val tknstreamer = extend tuple(strg:list(byte), ofs, value:integer) where {
      // find is a predicate for index of tok in a list, plus sets value if returning true
      private method find = \mod(tok:list(byte), lst:list(string)) {
        val inx = lst.index(tok)
        if(inx == nil) false
        else { value = inx; true }
      }
      // next tokenizes according to simple rules; it returns a label classifying the token, and sets value if needed
      method next = \mod() {
        loop {
          if(ofs == strg.count) return :eos
          if(bb.spacetab(strg[ofs])) ofs += 1 else break
        }
        val tkstart = ofs
        var allDigit = true
        while(ofs < strg.count) {
          val nxbyte = strg[ofs]
          val sinx = singletons.index(nxbyte)
          if(sinx != nil) {
            if(ofs == tkstart) { ofs += 1; return snglClasses[sinx] }
            break
          } else if(bb.lower(nxbyte) || bb.upper(nxbyte)) {
            allDigit = false
            ofs += 1
          } else if(bb.digit(nxbyte)) {
            ofs += 1
          } else if(bb.spacetab(strg[ofs])) {
            break
          } else {
            return :bad
          }
        }
        // here we've got a token terminated by punctuation; figure out what kind
        val len = ofs - tkstart
        if(allDigit) {
          value = strg.select(tkstart, ofs).to_i
          if(len == 4) :dg4
          else if(len == 1 || len == 2) :dgsh
          else :bad
        } else {
          // here for word tokens: find sets value attribute
          val tok = bb.toLower(strg.select(tkstart, ofs))
          if(find(tok, monthnames)) :monnm
          else if(find(tok, weeknames)) :wknm
          else :bad
        }
      }
    }

Here, "bytes" is a d8m module derived in part from go's "bytes" package; it also includes pattern predicates like `lower`,
`upper`, `digit` and `spacetab`.

Here's an outline of the `fromString` method:

    method fromString = \mod(s:string) {
      var tkstrm = [tknstreamer: cvt(s, list(byte)), 0, 0]
      var code0 = tkstrm.next
      var lastval = tkstrm.value
      val setPattern = (firstcode == :none)
      unless(code0 in [:dg4, :dgsh, :monnm, :wknm]) return false
      if(setPattern) firstcode = code0
      else unless(firstcode == code0) { errstrg = "date #{s} does not match previous row"; return false }
      var nxcode = tkstrm.next
      var curOfs = 0
      var gotIt = false
      if(code0 == :dg4) {				// here for year first
        ...curOfs = ...
      } else if(code0 == :dgsh) {
        ...curOfs = ...
      } else {
        ...curOfs = ...
      }
      // at this point, we've got the date, unless there isn't one; now collect the time if there is one
      // if already nxcode == :colon, we're in the time-only case, otherwise either get to there or detect that it's date only
      unless(nxcode == :colon) {
        nxcode = tkstrm.next
        if(nxcode == :dgsh) { lastval = tkstrm.value; nxcode = tkstrm.next }
      }
      val expectTime = baseunit in [:minute, :second]
      var tmOfs = 0
      if(nxcode == :colon) {
        ...
      } else if(expectTime) { errstrg = "time expected"; return false }
      if(baseunit in [:minute, :second]) curOfs = lshift(curOfs, 17) + tmOfs
      baseOffset = curOfs
      true
    }

Details are missing, but it should be clear that this method handles a wide variety of datetime formats. If `setPattern`
evaluates to true, this is the first datetime seen by the `datetime` entity whose `next` method is evaluated, and the format
is determined and used to set the relevant attributes. Otherwise, the earlier format is enforced, as seen in the first error
message in the above code, and only the `baseOffset` changes in calls to `fromString`. Therefore, one can easily store a
column of datetimes as a single `datetime` entity plus a column of `baseOffsets`.

The `baseOffset` is stored as multiple bit fields in an integer: month gets 4 bits, day gets 5. If present, the time in seconds
gets 17 bits; the year gets the upper part of the integer. As a result, this code can't represent years before year 0.

## Ordered lists

An earlier section showed how to define an `olist` type generator on any type of list that can be supplied with a function
that extracts an ordered entity from its elements. The type uses binary search to find and insert elements. Here's a sketch
of definitions for both lists of ordered types and lists with a function that extracts an ordered entity:

    val olist = \(T::entity, U::ordered, oextract:\(T)->U) {
        extend tuple(private x:list(T)) where {
            assert enumerable(x)
            // return index or -1
            private method binsearch = \imp(oelt:U) { ... }
            method count = \() { x.count }
            method insert = \mod(elt:T) {
                val inx = indexBinsrch(oextract(elt))
                if(inx < 0) x.pushb(elt)
                else x.insert(inx, elt)     // insert into (standard) list at inx
            }
            method lithook = \mod(lst:list(T)) { each(e^lst) self.insert(e) }
            method remove = \mod(elt:T) { ... similar to insert ... }
            method $in = \(elt:T) {
                val inx = binsearch(oextract(elt))
                inx >= 0 && x[inx] == elt 
            }
            method insertIfUniq = \mod(elt:T) { unless(elt in self) insert(elt) }
        }
    }
    val olist0 = \(OT::ordered) { olist(OT, \(x:OT) {x}) }

We give these two type generators different names since multibinding is not allowed on them. To me, they feel more concrete
than `set` and `dict`, but they are still pretty abstract. Assuming the standard mapping of a `list` is to a Go slice, they
produce slices of ordered elements, accessed via binary search. Various assumptions about the ordering are covered in the olist module documentation and won't be repeated here.

Ordered lists can be implemented in a lot more ways than this &mdash; balanced trees are an obvious example. Here we'll explore
an idea that significantly improves the speed of constructing large, slice-based ordered lists at the expense of slightly slower lookup.

The idea is to split the ordered list into two; we'll call them `official` and `recent`. `Official` is the big list and
`recent` holds recently inserted elements, it's best thought of as a cache. When its size exceeds a threshold, it's copied to `official`. This can be done so that the
(ordered) elements of `recent` are copied into `official` in order by moving at most `official.count` elements. 

Normally, each insertion causes on average half the elements to move, at least if insertions are in random order. The cache flush step is much more complicated than inserting one element, but it is done _much_ less frequently. 

If you mix lookups with inserts, the lookups cost two binary searches instead of one. If your usage pattern happens to be readily separable into phases dominated by insertion and lookup, flushing before lookup gives you the insertion speed advantage with minimal lookup penalty. Here are the key definitions:

    val olistCached = \typegen(T::entity, U::ordered, xfn:\(T)->U, limit:integer) {
        extend tuple(private official, recent:list(T), private limit:integer) where {
            // binsearch is used on both official and recent: returns index of elt <= oelt or -1 if no such elt
            val binsearch = \imp(lst:list(T), oelt:U) { ... }
            // list2Official copies recent to official
            private method list2official = \mod() {
                val olenb4 = official.count
                official.insert(official.count, recent)
                if(olenb4 > 0 && recent.count > 0) {
                    var inxo = olenb4 - 1, inxl = recent.count - 1
                    var destinx = inxo + inxl + 1
                    var oelt = xfn(official[inxo]), relt = xfn(recent[inxl])
                    loop {
                        if(oelt < relt) {
                            official[destinx] = recent[inxl]
                            inxl -= 1
                            if(inxl < 0) break
                            relt = xfn(recent[inxl])
                        } else {
                            official[destinx] = official[inxo]
                            inxo -= 1
                            if(inxo < 0) break
                            oelt = xfn(official[inxo])
                        }
                        destinx -= 1
                    }
                    if(inxl >= 0) copyFrom(official, 0, recent.select(0, inxl+1))
                }
                recent = recent.select(0,0)
            }
            method $in = \(elt:T) {
                val oelt = xfn(elt)
                val inx = binsearch(recent, oelt)
                if(inx >= 0 && official[inx] == elt) true
                else if(recent.count > 0) {
                    val rinx = indexA(recent, oelt)
                    rinx >= 0 && recent[rinx] == elt
                } else false
            }
            method insert = \mod(elt:T) {
              if(recent.count >= limit) {
                list2official()
                recent = []
              }
              val inx = indexBinsrch(recent, xfn(elt))
              if(inx < 0) recent.pushb(elt)
              else recent.insert(inx, elt)
            }
            method lithook = \mod(elts:list(T), limit:integer) {
              recent = []
              official = []
              self.limit = limit
              each(e^elts) insert(e)
            }
            method count = \() { official.count + recent.count }
            method remove = \mod(elt:T) { ... }
            method eachstart = \imp() { ... }
        }
    }
While this example omits a few details, it should give a good sense of how an algorithm like this looks in code. The rather
fidgety `list2official` method is the most conceptually complex part; it creates space to extend the `official` list, then
working backwards, finds each point where an element of `recent` should be inserted, moving just the part of `official` above
it into its final position. In this way, each element of `official` is moved at most once, supporting the claim that this
step is of similar computational complexity to that of a single insert or delete.

Since enumerating a cached olist requires a merge sort of the recent and official lists, we need an `eachstart` method for enumeration; refer to the module source code for details.

# Annotation

The tools d8m provides to specialize abstract problem statements for different contexts include `STMap` to declare how a
_source_ type can implement a _target_ type; `xprftns` to declare how expressions that match a pattern can be rewritten into
other expressions (aka "rewrite rules"); pseudo-functions to _apply_ STMaps and rewrite rules in particular contexts; assertions
and checks for manifestness. In order to use these tools effectively, a good deal of infrastructure is needed: definitions
of abstract data types, concrete ones that are good mappings for them in different circumstances, `STMap` declarations, pre-
built rewrite rules for different purposes, good documentation, examples to inspire and teach. Earlier parts of this document
give examples of abstract and concrete types, and cover some of the basics of assertions and manifest checking. Here, we
develop more examples. The details of declaring STMappings and rewrite rules are in the spec and are not covered here. The
spec also contains a fairly extensive set of examples of rewrite rules.

## ST Mapping

I've harped on sets earlier in this document, so let's see what we can do with them. An obvious first step is to provide
a hash table implementation. Rather than roll our own, we'll use Go's hash tables, accessed in d8m via `Gomap`. Here's an
STMap statement for this set implementation:

    given(T::entity, anElt:T, aList:list(T)) {
      STMap {
        source = set(T)
        target = Gomap(T, boolean)
        direct count
        enumerable(self) => self.keys
        lithook(self, aList) => lithook(self, aList.{[key~this, value~true]})
        $in(self, anElt) => rvalindexWD(self, anElt, false)
        add(self, anElt) => lvalindex(self, anElt, true)
        remove(self, anElt) => delete(self, anElt)
      }
    }

Paraphrasing the translation rules: the `count` method works the same in sets and maps; `lithook` maps a list of elements
into a list of key-value pairs; the `$in` method uses the "with default" version of `rvalindex` for maps, so the return type
is `boolean` instead of `nilPossible(boolean)` and it returns `false` if the element is not in the map; `add` and `remove`
use the corresponding map operations `lvalindex` and `delete`.

Given this declaration, we can say things like

    val mySet:set(account) = ...
    applyST(Gomap(T, boolean), mySet)

then a query involving `mySet` will compile it using a map in Go.

Another option, assuming an ordering on the set's elements is available, is to use ordered lists. Here's the translation rule for that:

    given(T::entity, OT::ordered, xfn:\(T)->OT, anElt:T) {
      STMap {
        source = set(T)
        target = olist(T, OT, xfn)
        direct count, lithook, $in, remove
        enumerable(self) => self
        add(self, anElt) => insertIfUniq(self, anElt)
      }
    }

The `enumerable` line isn't needed, since it's the default rule; maybe it's good style to affirm that enumeration is possible.
All the methods map directly except `add`. The extraction function is supplied when the rule is applied:

    applyST(olist(account, string, \(a:account) { a.email }), mySet)

causes `mySet` to be represented as a list ordered on the `email` attribute (which we'll assume is a string and hence, ordered).

Next, suppose `mySet` is large and changes often, so we want to try it as an `olistCached`. The mapping can be supplied either
directly or by composition with a rule mapping `olistx` to `olistSplit`. However, the latter rule would not be composable
unless it sets the `limit` parameter, so the best way is probably direct:

    given(T::entity, OT::ordered, xfn:\(T)->OT, anElt:T, limit:integer) {
      STMap {
        source = set(T)
        target = olistCached(T, xfn, limit)
        direct count, lithook, $in, remove
        add(self, anElt) => insertIfUniq(self, anElt)
      }
    }

This is the same rule as the one for `olist` except for the extra parameter; note that `insertIfUniq` was omitted from the
above example but is straightforward. Now we can map `mySet` to `olistCached` if we wish.

Sets can also be nicely represented with arrays of bits if the domain of the set is fixed, reasonably small, and capable
of being mapped into small integers. This is true of `integer` types for which both `minval` and `maxval` are manifest and
such that their difference is "small". Although `list(bit)` is a tempting way to represent such things, the variable length
property of lists is of no particular use when the domain is fixed. So we prefer to use the built in methods on type
`space(bit, [64])` which are documented in the spec. This type autoconverts from integer.

Thus, if we've got

    val rangedInt = \typegen(mn, mx:integer) {
      extend integer where { assert mn = type.minval; assert type.maxval = mx }
    }
    val modelOptions:set(rangedInt(0, 63)) = [1, 17, 3, 45]

then it's reasonable to implement this as a bitset. Here's a definition for one that works on up to 64 bits:

    import go "math/bits"
    val bitset0 =
      extend tuple(private x:integer) where {
        method setbit = \mod(inx:integer) { assert inx < 64; x = or(x, lshift(1, inx)) }
        method clearbit = \mod(inx:integer) { assert inx < 64; x = and(x, not(lshift(1, inx))) }
        method isBitSet = \(inx:integer) { and(x, lshift(1, inx)) != 0 }
        method count = \() { bits.OnesCount(x) }
        method empty = \() { x == 0 }
        method union = \(other:bitset0) { or(x, other.x) }
        method intersection = \(other:bitset0) { and(x, other.x) }
        method lithook = \mod(lst:list(integer)) { each(elt^lst) setbit(elt); self }
      }

It can be used to implement a set with the following

    given(aBit:integer) {
      STMap {
        source = extend set(integer) where { assert type.elttype.minval == 0; assert type.elttype.maxval < 64 }
        target = bitset0
        direct count, lithook
        self.isEmpty => self.empty
        aBit in self => isBitSet(aBit)
        self.add(aBit) => setbit(aBit)
        self.remove(aBit) => clearbit(aBit)
      }
    }

## Rewrite Rules

Both the purpose and the operation of type mapping rules (STMap statement) are quite straightforward. This is much less true of rewrite
rules. Their machinery is well defined (in the spec) and the examples there should give a pretty good indication of their
power. The main purpose of those examples though, is to show how to structure them and some of the pitfalls in writing and
using them. Here, I want to talk more about ways I expect rewrite rules to be used, and about the limits of what I know at
this point on that topic.

Some very general purpose rewrite rules are in use in the compiler and are a key part of code optimization. Two in particular:
"each merge sequential" matches on pairs of `each` statements where the first produces a list that the second
uses; "each merge parallel" matches on pairs of `each` statements that both enumerate the same list. The sequential
match arises naturally from chain operators after their thunks and function definitions have been inlined. Subject to various
checks such as no other uses of the intermediate list (which are always satisfied in the case of chain operators), the pair
is merged into a single `each` statement. Similarly for the parallel case.

A second "class" of rewrite rules might be rules that optimize some specific but important functions like sorting. A number
of these are shown in the examples in the spec. Finally, there are rewrite rules that provide what I like to call "scaffolding".
The example given in the spec applies to the specific task of making a daemon that performs some activity at regular intervals
of real time. The rewrite rule permits using an abstract problem statement while arriving at an idiomatic use of a `Ticker`
from go's `time` package. This is an example where in some sense we're extending the language. See the spec for details.

Here, we're going to develop a rule that addresses a small piece of the large problem of exploiting the concurrency in abstract
problem statements, and potentially allowing them to be solved on parallel computing resources. Specifically, we develop
a rewrite rule that can take an enumeration that sums pure transformations of a list of numbers and "map-reduces" it. This
means: maps it into a set of goroutines that sum parts of the list, followed by a "reduce" part that sums the partial sums.

Map-reduce is a famous design pattern nowadays, one that is used to speed up certain "big
data" calculations. It's not something that can be done automatically from abstract problem statements with today's tools.
The example given here doesn't change that, although it kinda sorta points in a direction that may be promising for changing
it. Here, we're going to automate a very restricted subset of the problems that are handled with available map-reduce tools,
and we're going to do it with the tools available in Go (i.e., goroutines and channels), which are designed to express concurrency
and that make no commitments about parallelism.

The "good part" of this example is that the map reduction transformation is automatic &mdash; the programmer is not required to
say how to split the initial data or reduce the partial results. The rule's parameters only say how big the list needs to
be in order to qualify, and how many pieces it should be split into. This rule can be generalized to handle more cases
where map reduce is useful, and other rewrite rules can be developed to parallelize sorting and other important algorithms.
But the options for parallelizing most abstract problem statements are so vast
that it barely makes sense to enumerate the possibilities. Typically, it makes more
sense to drive such decisions from the hardware
options available &mdash; threads on a shared memory machine, processors connected by shareable memories or by networks, etc.
I happen to believe that d8m, with its "abstract statement then annotation" theme, could enable tools and solutions of value
in this area, and that it's a good area for future work.

Without further ado, here's the rewrite rule; it's wrapped in `given(T::entity)` which is omitted. Explanation follows:

    val rwSum2MR = \xprftn(minlength, nparts:integer) {
        val L0:list(T), IV, Acc, Rhs:xprterm, S1, S3: xprstmts
        val S2:xprstmts = matching("Acc += Rhs", true, true)
        matchquery("{S1; each(IV^L0) S2; S3}")
        skipIf(count(L0) < minlength || usescount(S3, Acc) == 0)
        val accums = matches("Acc += _", S2)
        skipIf(accums[!(type(this.Acc).family == :float || type(this.Acc).family == :integer)] != nil)
        var chneltT:type, S2p:xprstmts, accumsym:xprterm, attribs:list(xprsym)
        if(accums.count == 1) {
            chneltT = type(Acc)
            accumsym = Acc
            S2p = S2
        } else {
            attribs = accums.{ mksym(gensym(), type(this.Acc)) }
            chneltT = mktype(:tuple, attribs)
            accumsym = mksym(gensym(), chneltT)
            S2p = S2.subst(accums.{this.Acc}, attribs.{mkterm(:symchain, accumsym, this)})
        }
        val chntype = mktype(:Gochannel, chneltT, :both)
        val chnvar = mksym(gensym(), chntype, parseRW("channelMake(chntype, 0)"))
        var stmtlist: list(xprterm) = [mkterm(:var, chnvar)]
        // The list we're splitting might be an expr; if so, alloc a temp for it, and create a decln
        var L0sym = L0
        if(termtag(L0) != :symbol) {
            L0sym = mksym(gensym(), type(L0), L0)
            stmtlist += [mkterm(:val, L0sym)]
        }
        val lstarg = mksym(gensym(), type(L0))
        val chnarg = mksym(gensym(), chntype)
        val neweach = mkterm(:each, IV, lstarg, S2p.subst([L0], [lstarg]))
        val thdbody = mkstmts([mkterm(:val, accumsym), neweach, parseRW("channelSend(chnarg, accumsym)")])
        val thdfntype = mktype(:function, [type(L0), chntype], nothing, :pure)
        val thdftn = mkftn(thdfntype, [lstarg, chnarg], thdbody)
        val thdftnvar = mksym(gensym(), type(thdftn), thdftn)
        val partsizevar = mksym("partsize", integer, parseRW("ceiling(cvt(L0.count, float) / nparts)"))
        stmtlist += [mkterm(:val, thdftnvar), mkterm(:val, partsizevar)]
        var mergebody: list(xprterm) = []
        // generate a Gocall stmt and a merge sequence for each part
        each(i^0...nparts) {
            pushb(stmtlist, parseRW(`Gocall(thdftnvar(L0sym.select(i*partsizevar, i == nparts-1 ? L0.count : (i+1) * partsizevar), chnvar))`))
            val rcvd = mksym(gensym(), chneltT, parseRW("channelReceive(chnvar)"))
            pushb(mergebody, mkterm(:var, rcvd))
            each(acc^accums, index) {
                val typ = type(acc.Acc)
                val plusfn = methodNamed(typ, :+)
                val accumd = (accums.count == 1 ? rcvd : mkterm(:symchain, rcvd, attribs[index]))
                pushb(mergebody, mkterm(:asgn, acc.Acc, mkterm(:funcall, plusfn, [acc.Acc, accumd])))
            }
        }
        mkstmts(S1.stmts + stmtlist + mergebody + S3.stmts)
    }

The key condition for the `matchquery` is an `each` statement whose body contains one or more assignments that increment some
numeric accumulator that's used later. (It makes no sense to map-reduce a variable used only internally.)
A secondary condition is that the list is long enough to be worth splitting. The `matchquery` captures the surrounding statements both for testing and to insert the new statements properly into the existing code.
After the `matchquery` we use the `matches` form, saved into `accums` to record each accumulating subterm in the loop. We get to reuse the `Acc` matchable since its use in the `S1` PEM involves `matchcount` which doesn't bind matchables.

The rule is going to create a local function, the ftnlit in `thdftn` which is bound to the symbol named `thdftnvar`. We call
this function inside a `Gocall` with a segment of the list and a channel for sending its result back to the reduction code.
The `stmtlist` and `mergebody` lists accumulate `nparts` of these `Gocall` statements and merging (reducing) statements respectively. The `thdftn` coopts a single accumulator or, if there are multiple accumulators, declares a tuple type to hold all of them. It substitutes these into the original accumulation statements and adds a `channelSend` at the end.

The each statement on `0...nparts` runs in the rule, accumulating code into `stmtlist` and `mergebody`.
The code leading up to the `thdftn` definition figures out the right type for the channel elements based on whether there
is a single accumulator or more than one. It creates the channel, the accumsym, and a symbol (`L0sym`) for the original list
`L0` &mdash; since that will be referenced in each `Gocall`, and could possibly be an expression involving a mod function, it's
important to create that symbol.

You enable this rule with a statement like

    rewrite(rwSum2MR(10000, 4))

which says that accumulations of lists manifestly inferable to being at least 10000 elements long get split into 4 threads.

# Coda

There are a number of areas of d8m that we haven't touched on here. These include quantity types, handlers, and simulations
of time-based models. These are all covered in the spec, but the spec doesn't give many examples or extensive motivation.
All of these features are retained in the language because I think they're useful and valuable enough to retain. Even so,
they're not central to the value proposition of "stating abstractly and solving by annotation".
