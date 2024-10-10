# Introduction

## Overview

D8m is a high level, easy to write, high performance language for general programming, with an emphasis on working with large and structurally complex data sets. It is statically typechecked with options for dynamic typechecking. It leverages Go -- the compiler generates Go and the language builds on Go packages. Its high level part includes fully abstract types, strong generics that include capability based type parameters, concise syntax for chainable operations (map, filter, reduce) on collections, convenient compile-time checking that `nil` is used properly, automatically compiled simulations from high level descriptions of models that evolve in (simulated) time, and quantity types that enable dimensional checks and ensure consistent use of units. It favors a concise functional style while allowing use of state in well controlled ways. Its support for generics arises not only through type parameters and the like, but through overloading (which I call _multibinding_ of symbols bound to functions).

The most unusual aspects of the language are some largely declarative features that affect how programs are compiled. First, type mapping allows abstract types like set, dictionary, and ordered list to be translated into different concrete types depending on how they are used. In a single program, one can have sets implemented with 5 different algorithms if desired. Second, rewrite rules let programmers define algorithm-level optimizations, "scaffolding" that automates use of library code in design patterns, and much more. Additionally, the use of assertions in the language is closer to their use in mathematics than is usual in programming languages, which opens the possibility of automatic compiler optimizations at a level well beyond those usually available. These "meta-programming" features point towards tools that, in conjunction with realistic models of program use, could empirically derive highly optimized target code from extremely abstract programs.

Thematically, d8m promotes a programming style that separates _statement of problem_ from _statement of solution_. I like to call this _annotation-based programming_. Traditionally, a "higher level" programming language is more abstract, but programmers have little to no choice about how programs compile. Performance suffers and so does the use case for the language. D8m aims to let programmers write abstract code that does not commit to implementation decisions, then "annotate" this code with decisions appropriate to the particular ways they're using abstract concepts.

Nobody cares about a new programming language if it doesn't promise to improve their productivity and perhaps make their lives a bit more fun. I think d8m is worth investing in chiefly because of the features mentioned already, but productivity and fun also devolve from good compilers and tools. I've developed a d8m compiler whose responsiveness is nearly that of an interpreter on small to medium programs, but which often run faster than the same program in python. D8m's ability to compile generic functions into concrete types makes it run faster even than go programs (like sorting) that use lots of interface methods. Infrastructure available in the initial release suffices for comfortable and productive programming for many kinds of problems and points the way towards much more.

## Amuse Bouche

Lexically and syntactically, d8m is very much in the C/C++ language family. There are a few slightly unusual lexical and syntax conventions, for example the backslash character `\` means "lambda" in function literals. Tokens can be classified as usual as identifiers, punctuation, and various literal forms (numbers, strings, etc). Identifier token rules are as usual except that identifiers beginning with two underscores are reserved and may not occur in user programs. Semicolons are statement separators, but line breaks usually work too.

D8m is REPL-oriented in the sense that its interaction model is that you make a series of definitions and then one or more _queries_. Queries can be arranged to give answers, to generate programs that can be run later, to run tests, or a variety of other things.

The built in types are minimalist: `list`, `tuple`, and `function` are the main vehicles for complex types, while `space` types provide fixed length analogues to lists and their higher dimensional extensions. The basic types are mostly conventional -- integers, floats, strings, and the like. What's important and novel about d8m's types is built on this minimalist base: a set of conventions that allow fully abstract types to be defined, and an orthogonal set of conventions that allow generic and parameterized types. There are of course no pointer types, but since d8m includes state, there is a notion of object identity as well as the more classical mathematical notion of extensional equality.

I'll try to be consistent about using the term _symbol_ only for the association of an identifier with a type (or something like a type), and usually, a binding. D8m requires symbols to be declared before they are used. It is sometimes possible to create a symbol without a binding, and later declare it again with a binding. This is how we do "forward declarations" for mutually dependent definitions of types and functions. There are also conventional ways to define symbols without bindings: formal arguments to functions, and more.

Scoping is conventional: there is a global scope, each function definition defines a local scope, certain control flow statements and expressions define nested local scopes, functions can be defined at any level and returned with the expected lexical semantics. There are modules, which introduce distinct "global scopes" for symbols defined within.

I think of d8m as a functional language. Its functions are "first class" entities, and it has syntactic support for important operations on collections, and significant restrictions on modifying state. But it does have state, so it's maybe not a functional language for purists.

The just mentioned support for operations on collections is a key feature. Chain operators are rewritten into calls to the `maplst` function or one of four related filtering functions. In support of chain operators, and of methods, d8m treats the identifiers `self` and `this` as pronouns. `Self` always names the receiver of methods and designates a generic instance in type definitions; `this` is the name of the current element in chain operations. References to attributes of `self` or `this` can leave the pronoun implicit, i.e., `self.X` can be written as `X` and so on.

Here is a quick example showing how d8m's functional operators can express relatively complex calculations concisely. Consider a type named `place` whose affordances include `centroid` (a `geocoord`) and `population` (an `integer`). Assume we've defined `src:place`, `terrains:list(place)`, and `radius:float`. And let `distance` be a function from a pair of `geocoord`s to `float`. Then

    sigma(terrains.[ distance(src.centroid, this.centroid) <= radius ].{ population })

designates the sum of the populations of `terrains` whose `centroid` is within `radius` distance of the centroid of `src`. The syntax `lst.[expr]` filters `lst` by `expr`, while `lst.{expr}` maps `expr` over `lst`; `sigma` is a built in summing function defined as a simple application of `reduce`.

The d8m compiler translates this code into a single loop over `terrains` with conditional summation. There is no performance penalty for writing "high level" code like this, which will hopefully encourage programmers to use it without hesitation.

Indeed, this example barely scratches the surface of d8m's ability to affect program performance by other means than writing programs specifically tuned to what a programmer thinks will make them run fast. D8m's most unusual feature is a kind of _meta-programming_ facility embodied in a pair of program transformation features that allow user control of compiler behavior to an unusual degree. These allow user defined optimizations to be applied universally, directed at specific parts of a specific program, or a range of options in between. These features focus on optimizations at the level of algorithm and data representation, with traditional compiler optimizations being mostly left to the Go compiler.

D8m uses function overloading extensively; I call it "multibinding" in this document. Among other things, it provides a semantically clean way to allow function argument defaults without complicating an essentially mathematical notion of function. Overloading applies to syntactic elements that are rewritten internally to functions -- binary infix operators, indexing, and so on. No doubt overloading is somewhat controversial. To my mind, it helps a programming language express ideas more naturally, and this benefit greatly outweighs the confusion that can arise as to what precisely a given use of a function identifier means.

D8m provides dynamic typechecking via two distinct, complementary mechanisms. First, "ortypes" list the types an entity can possibly take at runtime. Second, "type predicates" are used in several ways, including dynamic typechecking, where they describe certain properties the matching types must have. The type system treats `nil` as a type distinct from all others and provides convenient ways to handle possibly `nil` entities. This ends up acting like a convenient version of ML's Option types. Much of the convenience arises through typechecking rules that require the consequences of runtime checks on possibly `nil` entities to affect type inference. In this document, I'll use the term _guarding_ to talk about this, for example, a conditional check for `nil` _guards_ the subsequent reference. D8m's use of `nil` gives you better static typechecking with minimal verbosity.

D8m has a number of features aimed at making it good for modeling and simulation. First, it's possible to declare numeric types as _quantities_. This has two advantages: it enables dimension checking to be done, and it allows a compiler to optimize the representation of quantities. For example, in a query involving years, a compiler could choose 1 as the internal representation of a year whereas if the query involves nanoseconds, those might be the internal representation for durations. Second, there's a notion of _simulated time_ (via the `time` type) and of entities _situated in_ this simulated time (via the `situated` type operator). A semi-declarative _handler_ feature allows to say how situated things evolve as time passes, and a special binary operator (`x@t`) allows one to calculate the state of such things at different times. The upshot is that the compiler can generate simulation code for a surprising variety of situations from a largely declarative behavioral description. (Note, however, that most of these features are omitted from the current release.)

On the other hand, d8m does not have much ability at this moment to talk about required precision of numbers and of calculations with them. There are ways to convert to and from external formats with arbitrary representations but the internal representation of integer and float types is not a subject for "discussion" on the meta-programming side. Numerical analysis is an important and extensively developed topic; it is just not what I've focused on to date.

In keeping with C/C++ language family traditions, d8m programs consist of statements, most of which contain expressions. Statements are generally introduced by a keyword which is a reserved token. The most common statement by far is `val`, which declares one or more symbols whose bindings are fixed. Another way to declare symbols is with `var`; such symbols may be modded and assigned to. As such, `var` statements are only allowed in certain kinds of function definitions. Statement separation is by semicolon or newline, with semicolons generally
needed only when writing multiple statements in a line of program text.

D8m's execution model is that you define some things then make queries. (Expressions are taken as queries.) Definitions are checked and certain errors are detected. Queries are run and the answer is provided. It's quite natural to think of this model as a REPL, though there's no requirement for a d8m language processor to act precisely in this way. Usually, queries are compiled before being run but the language specification does not ordinarily require any particular policy about what is done within the compiler versus by running generated code. D8m's approach to the outside world includes a notion of deferred values, and a query that involves deferred values always results in a program rather than an answer.

We call the state of the compiler its binding environment (commonly abbreviated to _bdgenv_); this is the collection of defined symbols and their bindings, along with a number of other kinds of declarative information introduced by statements such as `assert`, `oncondition`, `ontime`, `STMap`, and `import`.

To give a bit more color on the specifics of the language, and evidence of its simplicity, you can finish off this amuse bouche with a list of keywords classed by role, followed by a one line summary of each statement type.

| role                          | tokens                                  |
| :---------------------------- | :-------------------------------------- |
| declare meanings for symbols  | `val var given import export dimension` |
| define complex types          | `extend`                                |
| control-flow expressions      | `if unless case`                        |
| looping statements            | `each map while loop`                   |
| other control-flow statements | `continue break return`                 |
| handlers                      | `ontime oncondition`                    |
| meta-programming              | `assert STMap xprftn`                   |
| misc expressions              | `cast cvt`                              |
| connecting words              | `else where`                            |

| Keyword by keyword |                                                                                 |
| :----------------- | :------------------------------------------------------------------------------ |
| `val`              | basic statement to define any non-assignable symbol                             |
| `var`              | like val but assignable; only allowed in imp and mod functions                  |
| `given`            | to create generic definitions, mostly of functions                              |
| `extend`           | to define types with methods, handlers, assertions, and so on                   |
| `assert`           | to assert facts, which often (but not always) influence compiler behavior       |
| `if`               | basic conditional statement, can be expression or statement                     |
| `unless`           | same as if with reversed conditional                                            |
| `case`             | can take if-else chain form, or to pick apart ortypes                           |
| `each`             | essential statement to enumerate enumerables                                    |
| `map`              | explicit expression form of what is usually written with dotbrace               |
| `while`            | loop with condition                                                             |
| `loop`             | just loop                                                                       |
| `continue`         | jump to top of loop or next iteration of each                                   |
| `break`            | break (one level) from loop or each                                             |
| `return`           | return from function                                                            |
| `cvt`              | convert one type to another (creating new structure)                            |
| `cast`             | interpret argument as a specified type (restricted; cannot compromise safety)   |
| `import`           | add a module or Go package into the binding environment                         |
| `export`           | declare which symbols are exported when the current file is treated as a module |
| `dimension`        | declare a new dimension for quantity types                                      |
| `oncondition`      | declare a condition handler                                                     |
| `ontime`           | declare something about behavior of a situated type                             |
| `STMap`            | describe how a "target type" can implement a "source type"                      |

## Notation and Terminology

For syntax, we will often use BNF in the following. When we do, `'::='` separates a production name from its value, tokens in single quotes are literal, tokens in angle brackets are productions, and meta-characters are `|()+*?` with the usual meanings derived from regular expression notation.

To give some flavor, and to define a couple of useful productions, here are statement lists

    <stmtxpr>		::= <stmt> | <expr>
    <stmtlist>	::= <stmtxpr> ( ';' <stmtxpr> )*

This BNF doesn't capture the rules about semicolons; as is common in modern languages, newline can be read as a statement separator.

D8m favors infix dot notation to an unusual extent; infix dot syntax is used for package/module identifiers, attribute references or assignments, function calls, and certain chain operators. We will occasionally refer to chains of infix dots as _dot chains_. An important special case of dot chains is when all the constituents are identifiers; we call these _symbol chains_ (or sometimes _symchains_), and these get special treatment with some of the meta operations because they are an appropriate generalization of variables for identifying specific entities.

D8m has a way to _passivate_ tokens: the `$` character prepended to a token means that token in a syntactically passive form. This is used only for infix and prefix operator tokens. You'll see things like `$=` and `$<` in this document when we want to talk about the functions associated with these operators.

# Types

## Structural Aspects

We think of types as a kind of algebra, the quotient of a free algebra on terms defined
in the next few paragraphs, with an equivalence relation determined by the rules about when
distinct terms designate the same type. The leaf terms of this algebra are

    integer, float, label, string, boolean, byte, bit, time, nil

Each of these words names a type, which I call a "basic type". The operators of this algebra are type generators named

    list, space, tuple, function

These generally combine freely with other type terms, subject only to having the correct number and kind of arguments. There are a few other bells and whistles that we'll get into below, but structurally, this is the entire type scheme. In other words, everything the type system models, with the possible exception of quantities, is described ultimately as a term over these ground terms and operators.

The equality relation on types depends on structure, as defined in this section, and on methods, which are covered in the next section. The relevant notion of equality is an easily computed relation whose negation we call TC-distinct, to emphasize its use in the typechecker, which depends on type structure and on methods. This relation is defined in [methods-and-more](#methods-and-more).

Since the purpose of types is to classify entities, a set of literals is associated with every type; these designate the entities of the type. Literals of each basic type are distinct from those of other types except that `bit` and `byte` literals are small integers. All the basic types are immutable. The use of type `nil` is somewhat unusual and is detailed below. The literal forms are quite standard:

| type      | desc                                                               | examples                                     |
| :-------- | :----------------------------------------------------------------- | :------------------------------------------- |
| `integer` | decimal digit string or hex with "0x" or "0X" prefix               | `0`, `1533429`, `0xdeadbeef`                 |
| `float`   | fraction plus integer or fraction followed by exponent             | `0.5`, `.999`, `1.7e12`, `45e-1`             |
| `label`   | colon followed by identifier or operator token                     | `:eeee`, `:_12d`, :<=                        |
| `string`  | stolen from Go: double quoted or backticks for multiline           | `"simple string here"`                       |
| `boolean` | true or false                                                      | `true`, `false`                              |
| `byte`    | single quoted string of one character or digits                    | `'\t'`, `'a'`, `'\107'`                      |
| `bit`     | integer literal 0 and 1                                            | 0, 1                                         |
| `time`    | the identifier `start` and 3 to 6 digit strings separated by slash | `start`, `2012/12/12`, `2012/12/12/11/10/59` |
| `nil`     | nil                                                                | `nil`                                        |

Byte and string literal forms are heavily inspired by Go but not identical. Double quoted strings have quoting and interpolation; strings in \` have neither. Single quotes are for bytes (versus runes in Go). Details in an appendix ([lexical-details](#lexical-details)). String type has a significant number of built in methods, including `$<`, which is lexicographic.

### List Type Generator

`List` is a type generator on a single, type-valued argument, so `list(integer)` and
`list(list(string))` are types. List's parameter is called its `elttype`. List literals
are comma separated lists of element literals, optionally preceeded by a type tag,
all enclosed by square brackets. When the type tag is omitted, the `elttype` is that
inferred for the first element; all the other elements must match. So

    [1,5,12]

is a `list(integer)` literal, while

    [[1.5, 3.14], [], [6.55, 3.7, 4]]

is a `list(list(float))` literal. (The admissibility of the token `4` as a float in
the third element of this literal is due to something called `autocvt`, which is explained
later.)

Type tagging identifies the type associated with a complex literal. A type tag is a
type-valued expression followed by colon. If `LT` is a symbol bound to a list type,
`[LT: a, b, ...]` is a literal value of `LT` whose elements are whatever the
symbols `a`, `b`, etc are bound to. In this case, each of `a`, `b`, etc. must be bound to an entity with the `elttype` of `LT`. Any type-valued expression can be a type tag -- if we had written `[list(integer): a, b, ...]` then `a`, `b`, etc must
designate integers. When we discuss typechecking in detail, we will see that there
are other ways to identify the type.

### Space Type Generator

`Space` is a type generator on two arguments, so `space(integer, [12])` is a type, the type of 1-d integer spaces of length 12. The first argument to a space type is the `elttype`; the second argument is its _dimensions_ argument, which is a list of positive integers enclosed in square brackets. A space with N dimensions corresponds structurally to a rank N tensor, though spaces don't necessarily have all the properties of tensors. Multi-dimensional spaces can model high dimensional data abstractly and are therefore excellent candidates for advancing some of the key ideas motivating d8m's design. Combined with other aspects of the type system, it lets us model sparse collections of high dimensional data, and the meta features of the language potentially let us select from the immense number of fascinating (and complicated) techniques for concrete implementation of such data while writing our programs abstractly.

However, handling high dimensional data is an advanced topic for good reasons. Accordingly, the initial implementation of d8m restricts space types to a single dimension.
A 1-d space corresponds to a fixed length vector which is essentially go's array type. I anticipate that a more complete implementation will require a significant amount of work to

1. define a proper set of built in methods on space types of particular dimensionalities (especially 2-d, corresponding to matrices or linear operators),
2. define operations that reorder dimensions, project between dimensions, etc.
3. extend to higher dimensions the rules given below for space type literals
4. define operations to move data between spaces and lists

In the rest of this document, we assume space types are restricted to a single dimension.

In general, we expect most spaces to be constructed, i.e., literal values are likely to be rare. A built in function is available to create constant-valued spaces. Literal values of space types must be identified (e.g. by type-tagging). For example, if `S` is defined to be the space type given earlier, then

    [S: 5,12,0,0,7,6,4,1,3,2,8,8]

is a literal designating an entity of type `S`.

There is no requirement for the dimensions of space types to be manifest. Thus, while each dimension of a space has a fixed length, that length can be determined at runtime.

Enumerability is an exceptionally important property of types, and is sometimes modeled as a type. In d8m, there are precise rules, covered in [abstract-types](#abstract-types) about what's enumerable, and the symbol `enumerable` is a type predicate ([defined later](#generic-types)). As you would expect, `list` and `space` types are enumerable.

### Tuple Type Generator

`Tuple` is a type generator with complex arguments. It is defined by its attributes, in order of occurrence. We use the term _bdgpt_, short for "binding point", for the syntactic construct `A:T` where `A` is an identifier and `T` a type; it occurs in attribute declarations but also in other situations. It is a symbol without a binding. Tuple types are normally written as `tuple(a1:T1,` ...`)` where the bdgpts inside the parentheses are the attributes. One may also declare attributes separately in `extend` statements, which are described in section [methods-and-more](#methods-and-more). Tuple types must have at least one attribute. The identifiers of attributes of a tuple type must all be distinct and disjoint from the identifiers of the type's methods.

The attributes of tuples take values which are accessed by the infix dot operator: `tup.atrb` designates the value of the `atrb` attribute of `tup`. As we'll see, infix dot has a few other possible interpretations as well. Tuples that are modifiable can change state either by assignment to their attributes or, for attributes of a moddable type, by calling a mod function with the attribute reference as receiver.

Attributes may be declared as private by prefixing the `private` keyword. Private attributes are accessible only in methods of the type; methods are covered in the next section. Tuple type identity depends on the identity of their attributes, treated as a list. In other words, declaration order matters:
`tuple(x:integer, y:integer)` is not the same type as `tuple(y:integer, x:integer)`.

Tuple literals are usually identified (eg type-tagged) but there are alternatives. Details are below and in [literal-forms](#literal-forms). Assuming `pt2d` is bound to the type `tuple(x:integer, y:integer)`, the literal

    [pt2d: 15, 5]

designates an entity of type `pt2d` with the `x` attribute bound to 15 and `y` to 5. Tuple literals do not need type tags when they are _anchor tagged_. This is an option where attribute names are given explicitly, with a `~` separator. For example

     [a~5.0, b~"a string", c~:xxx]

is a literal of type

    tuple(a:float, b:string, c:label)

This is true even if no such type has been declared. As such, expressions like this are known informally as "anonymous tuple literals", which have "anonymous tuple types". Type equality in d8m is structural (extensional) so anonymous tuple types are fully supported and can be very useful. Full details of anchor tagging are in [literal-forms](#literal-forms), while examples of anonymous tuple types are found in [chain-operators](#chain-operators).

We define list, space, and tuple types as _moddable_. These are the only types for which mod functions (discussed next) can be defined. Equivalently, only entities of these types have state.

### Function Type Generator

`Function` is another type generator with complex arguments, namely the argument types, the return type, and the purity. This requires slightly more complex syntax:

    \(T0, T1)->T2

is the type of pure functions from `T0` and `T1` arguments that return a `T2`. The form

    \pure(T0, T1)->T2

designates the same type. The purity indicator that optionally follows the backslash may be one of two words: `pure` or `mod`. So

    \mod(T0, T1)->T2

is also a function type, that of mod functions with a signature otherwise identical to the earlier example. Mod functions may modify their first argument, which must be of a moddable type. Thus, the above example is legal only if `T0` is a moddable type. Note that no function may ever modify anything other than its first argument; this is the crux of the restrictions on modifying state in d8m.

There is no special syntax for defining functions; it's done by binding function literals in declarations, just like any other definition. Function literals add a code body to a part that's like a function type expression, except with named arguments. In addition, the return type declaration is normally optional in function literals, and the word `imp` is allowed in the purity section. An imp function literal's type is pure but the code body is written in imperative style. Details are found in [state](#state). The _form_ of function literals is also used for type generators, discussed [here](#type-generators).

An example of function literal syntax is

    \(lst:list(integer), a:integer) { if(lst.count >= a) sigma(lst.select(0, a)) else -1 }

This function designates the sum of the first a elements of `lst` if it has enough
elements, else -1.

Function literal syntax also allows for arguments with default values. The syntax extends the argument with an `=` token and a value. For example, to modify the above function literal so the default value of `a` is `12` write

    \(lst:list(integer), a:integer = 12) { if(lst.count >= a) sigma(lst.select(0, a)) else -1 }

Formally, this definition designates two functions, namely:

    \(lst:list(integer)) { if(lst.count >= 12) sigma(lst.select(0, 12)) else -1 }
    \(lst:list(integer), a:integer) { if(lst.count >= a) sigma(lst.select(0, a)) else -1 }

As such, it may only occur in a declaration. For example, you are allowed to write

    val f = \(lst:list(integer), a:integer = 12) { lst.count >= a ? sigma(lst.select(0, a)) : -1 }

but not

    \(lst:list(integer), a:integer = 12) { lst.count >= a ? sigma(lst.select(0, a)) : -1 }([])

The first argument to a mod function cannot be defaulted. Since functions with defaulted arguments are not part of the type system but rather of the rules for typechecking, this discussion doesn't belong here in a formal sense. It's here for expository clarity. Full details are in [typechecking-details](#typechecking-details).

Most of the types have built in methods, details are in [the relevant appendix](#methods-on-built-in-types). Here we briefly review: `function` has no built in methods, `tuple` has only `copy`, `list` has many of them, and `space` has a few. The built in methods of basic types are as you'd expect -- arithmetic for numeric types, list-like methods for strings, etc.

### Non-Structural Types and Generators

Now let's get into the bells and whistles. These include `ortype`, `situated`, enumerated label types, quantity types, and types named `nothing` and `exit`.

`Ortype` is a pseudo type generator whose syntactic form is like that of `tuple`. So `ortype(a1:T1`, ...`)` designates an `ortype`; each component is called a _variant_. Ortype doesn't add any new entities to the type scheme. Rather, expressions (including symbols) to which ortypes are given have one of the listed types but at the moment we don't know which one. This is commonly thought of as being about "compile time" (or _static_) typechecking versus "run time" (or _dynamic_) typechecking. We'll treat it in terms of the idea of _manifest_ values; details shortly. The variants of an `ortype` are required to be a set of TC-distinct types, that is, neither duplicates nor types in subtyping relationship are allowed. In particular, `ortype` variants may not be ortypes. Because of set semantics, the type `ortype(a:T0, b:T1, c:T2)`. can also be written `ortype(c:T2, a:T0, b:T1)` and so on. Ortype expressions with duplicated variants generate an error.

The only built in method of ortypes is `tag`, which extracts the identifiers of its variants as labels:

    method tag:\() -> label

For example, for `ortype(x:integer, y:string)` the tag method's range is `:x` and `:y`.

Ortypes interact with the typechecker in some unusual ways: tag checks and certain other tests in conditionals and related control flow statements affect the inferred type of symbol references. For example, given a type

    val OTX = ortype(a:integer, b:string)

and a symbol `x:OTX`, a code fragment like

    if(x.tag = :a) F(x) else G(x)

treats the occurrence of `x` in the call to `F` as an integer and that in the call to `G` as a string. If `OTX` had a third variant, it would treat `x` in the call to `G` as an `ortype` with the 2 variants remaining after removal of `a:integer`. As an example of other control flow statements, consider the code

    if(x.tag != :a) return
    F(x)

Here, the occurrence of `x` in `F` is treated as an `integer`. The language requires quite robust type inference over ortypes based on such conditional expressions; I call this _guarding_ behavior. Detailed requirements are given in section [special-properties-of-type-nil](#special-properties-of-type-nil).

Consistent with treating `ortype` as a pseudo-type, `ortype` literals are never required and are not defined. Instead, expressions of any of the variant types are accepted where an ortype is expected; there are more details in [typechecking-details](#typechecking-details).

`Situated` is a type generator that applies to tuple types: given a tuple type `T`, `situated(T)` is the corresponding situated type. Situated types are provided to allow for compiler support of simulations on time-based models, which they do by admitting the `@` operator. Type `situated(T)` has a built in method `simT:\()->time` but is otherwise identical to `T`. Situatedness is a boolean property of the underlying tuple type, so `situated(situated(T))` is the same type as `situated(T)`. Use of situated types is described in [@ operator](#the-operator) and [ontime](#ontime-and-simulation).

Labels are adapted from Ruby; they can be thought of as the subset of strings consisting of symbol tokens and certain punctuation tokens. This subset is self-terminating so the literal form is the label value prefixed by colon. Since symbol tokens are ascii, so are labels; they never contain more general utf-8 coded characters. Type `label` cannot be extended and its only methods are `$=` and `$<` (which is lexicographic, like strings).

In addition to being the name of a type, `label` can be used as a type generator.
All of the arguments must be label literals and their order is significant. For example,

    label(:a, :b, :c)

designates a type whose elements are the 3 labels in the list. We call this an
_enumerated label type_, or `ELT`. Unlike `label`, the `$<` method for ELTs matches
the order in which elements are enumerated. Thus

    label(:b, :c, :a)

is a distinct type from the one defined earlier because its `$<` method is defined
differently. (These types are not TC-distinct, however.) Assuming a label such
as `:a` is defined in at least one ELT, the compiler usually cannot tell from the occurrence
of `:a` whether it is being used as a label or ELT. Rules defined
in [typechecking-details](#typechecking-details) tell how one can clarify this in programs.
It is legal to use both of the above types in programs, in which case the occurrence
of `:a` might mean a general label or either of the ELTs. This is usually a bad idea stylistically.

There's a canonical way to translate an enumerated label type to a set of integer values,
according to their index in the list. This will be done on demand in STMap. See [here](#stmap).

We close with two built in types that are quite different from the others: `nothing` and `exit`. They may only occur as the return type of functions. Neither corresponds to any entities, and their definition is best understood as operational. A nothing-valued function returns nothing. As in "doesn't return anything". As such, calling it acts as a statement. Such a function must be a mod function, since its effect cannot be observed otherwise. (Another possibility is revealed in [the-outside-world](#the-outside-world), which defines connected functions.) `Nothing` is usually the appropriate return type for mod functions unless you want them to be chainable. It is the inferred return type for mod functions ending with nothing-valued statements, such as loops or assignments.

An `exit`-valued function exits the computation. Its operational effect is in conditional expressions: when the type checker infers one branch to type `exit` and the other to type `T`, it infers `T` for the expression. One or more `exit`-valued functions are built into the language. Otherwise the type is ephemeral in the sense that the token `exit` is not predefined as a type; one gets its effect only by using the built in function(s).

#### Quantity Types

D8m supports quantity types and entities as follows. `Duration` is a built in quantity type because `time` (which is a built in type but not a quantity type) has built in addition and subtraction operations involving durations. Other basic quantity types can be defined, such as `length`, `weight`, `money`, etc. Each basic quantity has a baseunit, which is a symbol designating a single unit of a quantity of that type. Users can easily define new quantity types with the dimension statement. For example,

    dimension(length, meter)

defines two symbols: `length` as a basic quantity type and `meter` as its baseunit.

The `/` and `*` operators may be used on quantity types to build up compound quantity types. Standard cancellation rules apply, so `(length*weight)/(weight*duration)` `==` `length/duration`, which could be expressed in `meter/second`, `mile/hour`, etc. The identity element is `float`, so `Q/Q` = `float` for any quantity type `Q`.

There are no special literals for quantity-valued entities. Instead, these are created by multiplying numbers by quantities, typically quantity baseunit symbols or expressions involving them. Thus, if `second` is the baseunit for `duration`, then `5*second` is a `duration`. If you've defined `minute` == `60*second`, then `3.1415*minute` is also a `duration`. If `Hertz` == `1/second`, then `10*Hertz == 0.1*second`.

The initial state when the compiler starts is that `duration` is a defined quantity type but there is no baseunit associated with it. Accordingly, a dimension statement for `duration` is allowed. (Normally, it is an error to define the same quantity type twice with a dimension statement.) No other quantities are predefined; this allows full flexibility in using (or not using) any quantities you might desire, with the baseunits that make sense.

## Methods and More

The previous section describes all of the basic structural properties of types. However, methods are a critical aspect of type identity, and d8m types can have other properties as well. Most types have built in methods, and allow new methods to be defined on them using `extend` expressions. Methods are just functions with two special properties. Given a type `T`, a method of `T` is a function

1. whose first argument is a `T`,
2. which is declared as a method of `T`.

Type identity involves methods; details are given shortly. In addition to methods, assertions and handlers can be associated with types. These never affect type identity for typechecking purposes. You are welcome to imagine a "full" type identity that includes entailment of assertions and handler equality, but since this is not computable, it's of no practical interest. Type identity for typechecking purposes depends on the structural properties described in section [structural-aspects](#structural-aspects), plus the names and signatures of methods. The inverse of this relation we call "TC-distinct", since it relates types that are distinguishable by the typechecker.

A notion we find useful is that of _type families_. We associate a label naming the family with each basic type and built in type generator. So the list of type families is

    :integer, :float, :label, :string, :boolean, :byte, :bit, :time, :nil,
    :list, :space, :tuple, :situated, :function, :ortype, :nothing, :exit

This defines all the type families, and this set is fixed. Each family is distinct and there are no containment relations except that a type that is `:situated` is also a `:tuple`.

Everything about types beyond basic structural properties is expressed with `extend`, which is a type-valued expression. The syntax is

    <extend>  ::= 'extend' <type> 'where' '{' <xtndBodyStmt>+ '}'

Here, `<type>` may be any type-valued expression except an `extend` expression; we call it the _basetype_.
The options for `<xtndBodyStmt>` are

    <xtndBodyStmt>	::= <val> | <methodStmt> | <attribStmt> | <assert> | <oncond> | <ontime>

The `where` clause of an `extend` statement defines a scope; `<val>` statements declare symbols local to the scope. Within this scope, `self` designates an arbitrary instance of the type being defined.

A `<methodStmt>` defines a method. It has the form

    <methodStmt>  ::= 'private'? 'method' <ident> '=' <ftnlit>

This is best understood as a variant of the usual way to define functions, which is described in section [declarations](#declarations). Here, we cover the differences. The optional keyword `private` indicates that the method is private; this means that it is visible (as a symbol) only within method definitions of the type, or any extensions of the type. The `<ftnlit>` omits the first argument, named `self`, which has the type being defined and which we often call the receiver. Thus, a `U`-valued zero argument method of `T` is a one argument function with signature `\(T)->U`. In the method declaration this would be written `\()->U` or, since the return type is usually omitted from the signature of a `<ftnlit>`,
the `<ftnlit>` might be written `\() { <stmts> }`.

It's worth noting that `self` means the same thing in the body of the `extend` statement as in the method definitions but in the first case it's the symbol associated with the whole statement while in the second, it's the name of the receiver. Thus, the `self` of the statement is used in `assert` statements and handlers.

The `<attribStmt>` is a statement variant of an attribute declaration in a tuple form. It is legal only in `extend` expressions that extend a tuple type. It has the form

    <attribStmt>  ::= 'private'? 'attribute' <decllist>

where `<decllist>` is a standard list of (unbound) declarations as described in [declarations](#declarations). The resulting type's attributes concatenate those of the tuple type being extended with the `<decllist>` of each `<attribStmt>` in the order given. The rule given in [structural-aspects](#structural-aspects) for distinctness of attribute identifiers applies here as well.

The meaning of the `private` keyword is the same as for methods: such attributes are only visible within method definitions. This means private attributes are not visible in tuple litforms, which in turn means that tuple litforms cannot be used to create instances of tuple types containing private attributes. See [abstract-types](#abstract-types) for how this situation is resolved.

The `<oncond>` and `<ontime>` statements define handlers for the type; see [oncondition](#oncondition) and [ontime-and-simulation](#ontime-and-simulation).

We'll use the term _affordance_ in the following to mean either a method (for all types), an attribute (for tuple types) or a variant (for ortypes). Affordances are the way you observe the behavior of types, and they are what the type checker uses to decide type identity. D8m intentionally shades the distinction between attributes and methods in the sense that calling a "zero argument method" (whose only argument as a function is the receiver) can be syntactically identical to referencing an attribute. This applies when the "dot" form is used, for example, if `f` is a zero argument method of `T` and `x:T`, then the expression `x.f` will be interpreted as calling `f`, whereas if `f` was an attribute the same expression would designate the value of the attribute. When `f` is a method, the expression `f(x)` unambiguously means the function call, as does `x.f()`. This ambiguity makes code more abstract, and in my view, improves code flow.

Methods are functions, but it's less obvious how to get a reference to a method when not calling it. A straightforward idea is to say `x.f` always designates the affordance of `x` named `f` -- that works for attributes, when being either used or assigned to. This also works if `f` is a method with arguments other than the receiver. However, as we've just stated, if `f` is a zero-arg method then `x.f` designates not the function `f` but the result of calling it. A couple of workarounds are available for this syntactic inconvenience: you can write `cast(x.f, \(T)->U)` or `\(x:T) { x.f }`.

A complete description of the conditions on `extend` statements is as follows:

- The following types may not be extended: `nothing`, `nil`, `boolean`, `bit`, `time`, `label`, any `function`, any quantity. However, enumerated label types (i.e. specific collections of labels) may be extended.
- `Ontime` statements may only occur in tuple and situated types
- `Attribute` statements may only occur in tuple types

D8m builds in a definition of equality for (the instances of) every type; actually, two versions of equality which I call extensional and intensional (or "object") equality. See [equality](#equality) for details.

We also define an equality relation on types themselves. This is not used in d8m but only to define its typechecker. To emphasize this, we say "TC-distinct" for the negation of type equality. Two types `T0` and `T1` are equal if they have the same family, compatible methods, and in addition some conditions that depend on the type family. Method compatibility means having the same identifier and `=` types. The families with extra conditions are listed in the following table:

| family     | conditions                                                                                                         |
| :--------- | :----------------------------------------------------------------------------------------------------------------- |
| `label`    | if ELT, then same labels in same order, else no conditions                                                         |
| `function` | same number of arguments, argument types pairwise `=`, return types `=`, same purity word                          |
| `tuple`    | same situatedness, same number of attributes in same order, each corresponding attribute has same name and types = |
| `ortype`   | variants are identical as sets                                                                                     |
| `quantity` | `T0` and `T1` numerator and denominator dimensions are identical as sets                                           |

The "compatibility" requirement on methods in type equality is essentially syntactic. The good thing about this is that it's easy to define and verify. But syntactically compatible methods can designate utterly different functions, and depending on implicit properties of these can be treacherous. For example, we define the `ordered` type predicate with the `$<` method, and build a structure on this using algorithms like binary search that assume `$<` is a total ordering. If you decide that `$<` is a good name for the subset predicate on sets, then sets will suddenly become "ordered" even though the usual `$<` method on sets is a partial order.

You may imagine that the type designated by an `extend` expression is a _subtype_ of its basetype, and that the typechecker accepts entities of the new type wherever entities of the basetype are expected. In d8m, this is not the case. As discussed further in [scopes](#scopes), the methods of the basetype become methods of the new type but that's it. The effect is that you can call the methods of the basetype with entities of the new type but a function with a basetype argument will not typecheck with entities of the new type. As such, it may be said that d8m is actively hostile towards certain classic OO patterns, which I feel can lead to difficulties in understanding code that aren't justified by what might be gained. However, see [the description of cast](#built-in-pseudo-functions) for a way to get what you want with a little extra verbiage.

D8m does use subtyping with ortypes. An `ortype` is a supertype of each subset of its variants, including singleton subsets. So the typechecker accepts entities with variant types in ortype-valued arguments. As always, there is an asymmetry of function arguments versus function results. Thus, if `T0` is a subtype of `T1` (e.g. `T0` is `integer` and `T1` is `ortype(a:integer, b:string)`), the typechecker allows a `T0` as a function argument when a `T1` is required, and but does not allow a `T0` as a return type when the context requires a `T1`.

The `<assert>` statement is described in [assert](#assert). When used in `extend`, the quantification requirement mentioned there is satisfied by a reference to `self`, which in `extend` always designates an arbitrary entity of the type being defined. Therefore, an `assert` within a type definition makes an assertion about all entities of the type. Assertions about types can be important for the interaction of their manifestness with annotative mechanisms (i.e. `STMap` and rewrite rules) that depend on this.

The standard comparison functions have well known mathematical properties, and some of these are incorporated into d8m by design. Specifically, every type with a method named `<` also has a method named `<=` and similarly for the pair `>` and `>=`. If you define `<` for some type, the compiler automatically supplies `<=` according to the standard definition. I've chosen not to incorporate the inverse relationship of `<` and `>` because in obscure cases involving modding, changing argument order can change the semantics of a program. Thus, a type with a `<` method does not automatically have a `>` method. Of course, these methods are built into all the standard types.

## Generic Types

We started the discussion of types with the "structural" aspect -- a free algebra over `list`, `tuple`, and so on. Then we added methods, which participate in type identity for typechecking purposes, and other things, such as assertions, which do not. Adding "genericity" to a type system means, at minimum, letting users define their own "type generators" analogous to `list` and so on. Since d8m's built in types are absolutely basic, generic types like `set`, `dict`, and `olist`, which are useful in most every non-trivial program, must be user defined, and must demonstrate excellent performance.

D8m has user defined type generators and several other features to make the language more expressive and convenient. We preface our discussion of these features with a short discourse on the ideas of _manifest_ and _explicit_, concepts whose importance in programming languages may be underappreciated. _Manifest_ is essentially just a fancy way of saying _known_ -- the value of an expression is _manifest_ if it's known. But time is an implicit parameter here: manifestness is defined at a particular time. In compiler talk, this usually means "when the compiler reads the expression", aka "at compile time". Manifestness applies to all kinds of expressions, including type-valued expressions. Another way to say that a language is statically typechecked is to say that all of its type-valued expressions are manifest at compile time. This definition begs a question though: are _all_ the types really manifest? Go has a powerful and fascinating design feature called _interface types_. Like d8m's ortypes, these are not new types structurally. Rather, they designate things whose types are manifest at runtime. The methods they require tell the compiler what operations can be counted on to work in programs using such types, and go provides ways of asking and asserting what the runtime types are.

The concept of _explicit_ is marginally related to manifest. In a free algebra over any set of type generators, the types are all explicit. This is not always the most convenient option in a programming language. For example, the type of the standard functional operation `map` over lists is written in the d8m notation as `\(list(T), \(T)->U)->list(U)`. If this is a method of lists, then the `T` type is supplied by the built in `list` type generator, but `U` is a free variable. If all type variables are supplied explicitly, then our generic type scheme will need a syntax that lets us write down the value of `U` every time we invoke `map`. I repeat: this is not maximally convenient.

### Type Predicates

To define d8m's generic typing features, we start with _type predicates_, which we'll designate as `typepred`s. Type predicates sound like they might be functions from types to booleans, and that's not wrong, but it's not quite the whole story. As we will see later, d8m uses function syntax to define quite a few things that aren't formally functions in the type system. (Examples include `typegen` and `xprftn`.) But although one useful interpretation of typepreds is functional, we use a simpler syntax to define them. We'll proceed again with the algebraic point of view. The ground terms are

    entity moddable enumerable tuple list space quantity situated

Each designates a "class" of types: `entity` designates all of them, `tuple` all the tuple types, and so on.

The `typepred` operator creates more complex typepreds. It is written syntactically like a function call, with a variable number of arguments. The first argument declares and names a _base predicate_ while any subsequent arguments designate required _affordances_. An affordance is a method or attribute, while the base predicate is the moral equivalent of the base type in the `extend` expression introduced in the previous section. Thus, schematically the `typepred` operator is written

    typepred(T::TP, aff0, aff1, ...)

where `T` is the base predicate (a local name for the type being matched), `TP` is a typepred that `T` has to match, and each of `aff0`, etc. is a bdgpt, optionally prefixed with the keyword `attribute`. Note that `::` is the token in d8m that declares a type-valued symbol constrained by a typepred, just as `:` declares an entity-valued symbol constrained by a type.

The interpretation of all this is hopefully pretty obvious: a type matches this typepred if it matches `TP` and has all the required affordances, as attributes if the `attribute` keyword is used, else as either methods or attributes. Note that only zero-argument methods can also be attributes, and that all method affordances must be written with the receiver explicit.

For example, let's define a type predicate that matches types having an integer-valued affordance named `foo`. To require it to be an attribute, write

    typepred(_ ::entity, attribute foo:integer)

In this case, a basepred of `tuple` would be equivalent (since only tuples have attributes), and a basepred of any other type family would result in an unmatchable type predicate, which is an error. Note that the symbol \_ has no special meaning in d8m. Its scope is local to the `typepred` operator and is a convenient identifier for an unused symbol.

To require `foo` to be a method, write

    typepred(T::entity, foo:\(T)->integer)

Finally, to allow it to be either an attribute or a zero argument method, write

    typepred(_ ::entity, foo:integer)

A more interesting example is `ordered`:

    val ordered = typepred(T::entity, $<:\(T,T)->boolean)

This matches any type with a binary predicate method named `<`. Assuming all such methods implement a total ordering, `ordered` is a good basis upon which to build types such as ordered lists. (Note that this assumption is not expressible in d8m's type system, or for that matter, in computable type systems generally.)

I promised earlier that typepreds aren't just predicates. We'll use the predicate interpretation in several ways, such as to constrain type-valued arguments in type generators. But a typepred is quite similar to one of go's interface types, and we'll interpret typepreds as non-manifest types when they occur in type-valued expressions. In this interpretation, their affordances play exactly the same role as the methods of an interface type in go.

Actually, the foregoing overstates things slightly. Typepreds can occur in type-valued expressions _except as variants in ortypes_. Ortypes are a way to talk about non-manifest types, typepreds are another. Typepreds can't designate ortypes by construction, and ortypes can't designate typepreds by fiat.

Now that I've introduced both ways of talking about non-manifest types, I want to point out that d8m's execution model admits of _three_ times relevant to manifestness. There's runtime, as always, but it can be worth distinguishing between two stages of compile time which I'll call _definition time_ and _QC time_. (QC stands for "query compile", the act of compiling a query.) You might have a process, like formatting some data into a string, that applies to most any type, which means it's convenient to define in a function `FMT` accepting a non-manifest argument, perhaps `x:entity` instead of `x:integer` or `x:ortype(...)`. Then if you have a query that includes `FMT(a)` the type of `a` is manifest when the query is compiled. So, d8m is allowed to compile it as the case in `FMT` for `a`'s type, rather than as an `interface{}`. A practical example of a QC-manifest type is a JSON encoder. JSON can encode many types; statically typed languages need to define the encoding function using some kind of non-manifest type. But particular calls to the encoding function will involve manifest types in most cases. It turns out that d8m can take advantage of this.

### Type Generators

Type generator literals have the syntax of function literals, with the following differences

1. they use `typegen` as the purity word
2. they return a type and
3. their arguments may be type-valued (aka "generic type symbols") using `::` declarations

The body of a type generator has the same rules as `pure`: no variables, no assignments, no use of mod functions.
The classic type generator has only type valued arguments, as in

    val set = \typegen(T::entity) { extend list(T) where {...} }
    val dict = \typegen(KT, VT::entity) { extend tuple(...) where {...} }

An example that mixes generic type symbols with generic entity symbols is

    val olist = \typegen(T::entity, U::ordered, oextract:\(T)->U) { ... }

which defines the type of ordered lists of `T` given an ordered type `U` and a function that projects `T`'s onto `U`'s.

### Definitions in Generic Context

Type generators give us "generic" or "parameterized" types. Since types can have methods, this provides certain kinds of generic functions. But not all functions are conveniently presented as methods of types. To provide another "dimension" of genericity, d8m has the `given` statement (full details are in [given](#given)). This introduces generic symbols which can be used in definitions of functions and certain other things. Whereas in type generators, the generic symbol is a parameter and hence takes its value when the type generator is instantiated, generic symbols defined in `given` get values by inference from context of use. Let's start with an example to illustrate the general ideas.

    given(T::entity) {
    	val reduce = \imp(lst:list(T), fn:\(T, T)->T, initv:T) -> T {
    		var accum = initv
    		each(x^lst) accum = fn(accum, x)
    		accum
    	}
    }

This is a definition of a function `reduce`, with 3 arguments whose types share certain constraints. While this code involves numerous details that haven't been introduced yet, the thing to focus on here is the generic symbol `T`. It's the `elttype` of the first argument, the second argument is a function that reduces two `T`'s to a single one, and the third argument is a `T`. It continues to matter in the body of the function, since the type of `accum` is inferred from that of `initv` and so on. By using `T` we've defined `reduce` as a kind of _function schema_, from which one can create a specific `reduce` function for each possible value of `T`. With this definition in hand, a d8m compiler will check every call to `reduce` to see if it matches this pattern, and if successful, it will bind the function in that call to the one corresponding to that value of `T`.

This is the essential idea of `given` for function definition. More generally, it's used to declare type- and entity- valued symbols for various purposes. Details are in a later [section](#given).

## Abstract Types

We've talked about several generalizations of types but we haven't yet gotten to abstract types. Abstract types of interest to us are usually parameterized as well, but that's not why they're abstract.
To make types _abstract_ we need to precisely control their affordances, provide ways to construct them that are consistent with those for built in types, and if they are collections, to say how to enumerate them. An apparent technical problem is that `extend` can only _extend_ existing types, and cannot delete their affordances. However, `tuple` has essentially no methods, and attributes can be `private`. Thus, to get a type that's a blank slate, define a `tuple` type with `private` attributes:

    tuple(private x:T)

is a type with no externally visible affordances (except `copy`). Then build on this with extend:

    val abstT = extend tuple(private x:T) where { ... }

Parameterized abstract types can be defined with a type generator that returns such an `extend` expression.

In the rest of this section we discuss how to construct and enumerate abstract types.
The problem of constructing is that all the built in types have litforms; what shall we do about private attributes? The answer was stated briefly in [methods-and-more](#methods-and-more): tuple types with `private` attributes cannot be constructed with litforms, because `private` attributes may not be accessed outside the `extend` expression defining the type.

For this situation, and for abstract types generally, we introduce a method-based way to construct tuple-based entities. Methods named `lithook` are not called explicitly but are inferred from litforms that match. Since methods are part of type definitions, the type's `private` attributes are accessible to `lithook` methods. Thanks to multibinding, multiple `lithook` methods can be defined. A litform for a type that has one or more `lithook` methods is matched against those methods, and if any match, the litform is rewritten to call that method. A few technical conditions are needed to make all this work. Methods named `lithook` are always `mod` methods, their receiver is implicit, they must return `self` (and if they don't do so explicitly, the compiler will add code to do so), and they may only be defined on types constructed with litforms (i.e., tuple, list, and space types).

The opportunity to construct entities in different ways via multiple lithook methods can be used in obvious ways and more creative ones. An obvious example is for a directed graph type, where it may be convenient to create a graph from a list of tuples, each describing a node and its edges, or from a list of nodes and a separate list for the edges. More creatively, you can have one argument direct the interpretation of another, as in a complex number type:

    val C = extend tuple(...:float) where {
    	method lithook = \mod(a, b: float, interp:label(:cart, :polar)) { ... }
    }

Here, the `interp` argument directs interpretation of `a` and `b` arguments as either `rl` and `im` or `r` and `theta`. Then you could write

    val cx1 = [C: 1.0, 5.0, :cart]
    val cx2 = [C: 1.0, 5.0, :polar]

to create two quite distinct complex numbers. Using d8m's annotation mechanisms, you can cause complex numbers to have either a cartesian or polar internal representation.

One final notational convenience for `lithook` methods is that if the only explicit argument is a list, that list can be implicit in the litform. Specifically, suppose type `T` has a `lithook` method of the form

    method lithook = \mod(x:list(E)) { ... }

Then a `T`-tagged litform with a list of arguments will match the `lithook` method. In other words

    [T: a, b, c, d, ...]

where the list items infer to `E`, will be interpreted as

    lithook([a, b, c, d, ...])

Note that a litform where the list is explicit will also match the same method:

    [T: [a, b, c, d, ...]]

This special treatment of lists only applies to lithook methods with a single
list-valued argument.

It is useful to think of the standard litform for tuple types (without `lithook` methods) as an implicitly defined `lithook` method. This clarifies that other `lithook` methods can be defined for such types, as long as they are TC-distinct from the standard one.

This completes our discussion of constructing abstract types. The final issue is enumerating them if they are "collection like". In d8m, `enumerable` is a type predicate, not a type. Operationally, a d8m type is enumerable just in case it can be the binding of the iteree in an `each` statement. Types in the `list` and `space` families are enumerable, and there are two ways to make tuple-based types enumerable:

1. a special `assert` statement names an attribute to enumerate
2. a method named `eachstart` tells how to enumerate its elements using the _eachstart protocol_.

Let's take these one by one. It is fairly common to have an abstract type that is essentially a restricted version of `list`, or less commonly, `space`. For example, a natural way to define sets is as lists with the list methods removed and some set methods added. Such a definition is perfectly expressed with the schema introduced earlier:

    val set = \typegen(T::entity) { extend tuple(private x:list(T)) where { ... } }

It turns out that it's safe to enumerate such a set by enumerating the underlying list. (It's safe in part because `each` cannot modify the list, only its elements.) A special `assert` statement is designed for cases like this. There is a built in "effective assert predicate"

    enumerable:\(T)->boolean

which is allowed only in `extend` statements whose base type is a tuple type; its argument is required to be the name of an attribute of the type being defined. See [effective-assert-predicates](#effective-assert-predicates) for a complete discussion. Extending the earlier example, we get

    val set = \typegen(T::entity) {
    	extend tuple(private x:list(T)) where {
    		assert enumerable(x)
    		...
    	}
    }

This `set` type is enumerable, and enumeration works by enumerating the underlying list, without exposing it to any other list methods.

For cases where an enumerable attribute isn't available, we have the _eachstart protocol_ . A type `T` obeys the eachstart protocol if it has a zero argument method named `eachstart` which returns an entity whose type matches the following type predicate:

    given(ET::entity) {
    	val ESTP = typepred(T::entity, value:ET, eachdone:\(T)->boolean, eachstep:\mod(T)->nothing)
    }

Here, `ET` plays the role of the `elttype` of the collection. It requires methods named
`eachdone` and `eachstep` with the indicated signatures, and either an attribute or
a method named `value`. Note that `ESTP` is not used in programs, we just use the name here as a way to describe the requirements on `eachstart`'s return type. To summarize, a type `T` is enumerable if its affordances include

    method eachstart = \() -> E

where `E::ESTP`. In the usual case that enumerating `T` requires some state, the entity returned by `eachstart` includes that state. Here's how an `eachstart` method makes the type enumerable. Imagine `x:T` where `T` obeys the eachstart protocol. Then the code

    each(a^x) { <each body> }

translates into code like the following:

    var es = x.eachstart
    loop { if(es.eachdone) break; val a = es.value; <each body>; es.eachstep }

where `es` is an unused identifier.

An enumerable type with an `eachstart` method will enumerate using that method rather than its existing one. For example, if you define a type `T` that extends list and the extension includes `eachstart`, then `T` will enumerate using the `eachstart` method rather than standard list enumeration.

There is one other way for a type to be counted as enumerable: if it has an `autocvt`
function (see [built-in-pseudo-functions](#built-in-pseudo-functions)) to `list` or `space` (or any other enumerable type). This is somewhat different because the semantics of `autocvt` involves creating new structure, although in some cases a compiler will be able to optimize that away.

In keeping with our somewhat relaxed attitude about enumerability, d8m has an "enumonly" rule for functions that saves a lot of conceptual and practical effort. Let `E` be a type that's enumerable for any of the reasons given, and `F = \(..., ea:E, ...) {...}` be a function with an `E`-valued argument. Now suppose that the only use of `ea` in `F`
is to enumerate it. Then `F` will typecheck with _any_ enumerable type in the `ea` argument position.
For example, `reduce` is defined on `list(T)` where `T::entity` is generic.
Look at its definition and you will see that the only use it makes of the list is to enumerate it.
The enumonly rule applies, so `reduce` can be called with any enumerable type.
For example, the set type sketched above can be reduced without having to define `reduce` specifically for sets.
In addition to functions, special "enumonly" consideration is given to the list methods
associated with chainops. Specifically, these are `maplst`, `filt`, `filt1`, `filtx`, and `filt1x`.
Since these are methods, they wouldn't normally typecheck with non-lists in receiver position.
All of them are enum only and the special dispensation is that any enumerable entity in the
receiver position of these functions will typecheck.

The "enumonly" rule extends the range of arguments that typecheck in functions
but note that it doesn't change the return type of these functions.
So applying a list-valued enumonly function to a set produces a list, not a set.

## Special Properties of Type `nil`

In d8m, `nil` is a symbol that designates both a built in type and the only entity of that type. Normally, such a thing wouldn't be useful, but d8m has built in support for `nil` to mean a "missing value" in various `list` and `string` methods, and this built in support can be extended to user defined types and methods. Formally, a method that can fail due to a missing value has a return type that's the `ortype` of `nil` and its normal type. It's convenient to call this a `nilPossible` type, and we can define this as a type generator:

    val nilPossible = \typegen(T::entity) { ortype(main:T, nil:nil) }

The following `list(T)` methods have `nilPossible` return types:

    method filt1 = \(\(T, integer)->boolean) -> nilPossible(T)
    method filt1x = \(\(T, integer)->boolean, \(T, integer)->U) -> nilPossible(U)
    method head = \() -> nilPossible(T)
    method last = \() -> nilPossible(T)
    method tail = \() -> nilPossible(list(T))
    method popf = \mod() -> nilPossible(T)
    method popb = \mod() -> nilPossible(T)

Intuitively, `filt1` and `filt1x` have a missing value if there is no element in the receiver matching the predicate, while the other 4 methods fail if their receiver is an empty list. (Note that `filt1` and `filt1x` normally result from internal rewriting of chain operations, see [chain-operators](#chain-operators).)

The built in support for `nilPossible` types takes two forms. The critical support is the guarding behavior enabled by tests on `nilPossible` entities. This is just the normal behavior of ortypes but extended in a logical way for `nil`. Since type `nil` has a finite set of possible values, we treat expressions of the form `X == nil` as guarding `X`. Thus, given `x:nilPossible(T)` in the statement

    if(x == nil) { g() } else { h(x) }

`x` infers to `T` in the else branch. Next, consider

    if(x == nil) return
    h(x)

Now, `x:T` is inferred for all statements after the `if` until the end of the function (or the end of `x`'s scope). The same applies if `return` is replaced by `continue` or `break`, except that the inference persists until the end of the `each`, `while`, or `loop` statement in which these statements occur.

The obvious variants are inferred as well, so given the previous definition of `x`

    if(x != nil) { h(x) } else { g() }

has the same behavior as the previous example.

For the nilPossible `list` methods that fail due to empty list, there's an easy way to test before generating the `nil` value, and it is a better coding practice to do so. Consequently, the relevant tests are also required to have guarding behavior. So given `x:list(T)`, in the expression

    if(x != []) f(x.head)

the inferred type of `x.head` in the call of `f` is `T` rather than `nilPossible(T)`. The same applies to

    if(x.count != 0) f(x.head)

and of course, one can replace `x.head` with any of `x.last`, `x.popf`, or `x.popb`.

The second form of support for type `nil` has to do with inferring return types in functions. Given that a function definition infers a return type of `T`, the occurrence of a conditional return of `nil` changes that inference to `nilPossible(T)`. So whereas most functions with return types in the `ortype` family must explicitly declare the return type, nilPossible return types can usually be inferred.

As we've seen, several fundamental list and string operators have nilPossible results, but the real power of `nil` is that users can "borrow" the built in support to use `nil` as a convenient missing value indicator for their own types. Of course, they can also roll their own; at some loss of convenience.

NilPossible list operators can lead to ambiguity; for example, let `x:list(nilPossible(T))` and consider when `x.head` is `nil`. We don't know if that is because `x` is a non-empty list whose first element is `nil`, or an empty list. To prevent this problem, d8m forbids declaring a symbol whose type is `list(OT)` where `OT` is an `ortype` whose variants include a `nil` type. Such types are allowed in expressions and can arise quite naturally in chain operations (see [chain-operators](#chain-operators)) where, if `xfm1`'s output is `nilPossible(T)`, an expression like

    lst.{xfm1}.[this != nil].{xfm2}

should and does work as expected, despite the fact that the inferred type of `lst.{xfm1}` is `list(nilPossible(T))`. In fact, this example shows another instance of guarding behavior -- since the dotbracket filters `nil` results from `xfm1`, the inferred input to `xfm2` is `T` rather than `nilPossible(T)`.

# Programs

## Declarations

A program consists of a series of declarations followed by a query. Declarations affect the binding environment and only produce output if they contain detectable errors, while a query produces either an error, an answer, or a program. Before we go through the statements that declare symbols, we explain more carefully what we mean by the term _symbol_ and what the possibilities are in d8m.

A _symbol_ always associates an _identifier_ with something. It always has a _scope_, which always starts at the point of declaration and continues until the end of the current scope. Symbols differ in what they're associated with and whether they have a _binding_. A _bound symbol_ has an entity, type, type generator, or type predicate bound to it. Most ways of declaring symbols do not admit of binding, but `val` and `var` statements do, and these are the most commonly used. The kind of thing a symbol is associated with is in some sense one step more abstract than the kind of thing it gets bound to. In particular, if associated with a type, it gets bound to an entity; if associated with a type predicate, it gets bound to a type.

The full list of symbol declaration forms is

1. in `val` and `var` statements,
2. in `given` statements,
3. in the signatures of function literal forms,
4. as attributes of tuple types,
5. as the iteration variables of `each` statements,
6. in the `always` expression,
7. as the first argument in a `typepred` expression.

Only symbols declared in `val` and `var` statements can be bound in the declaration. The symbols declared in `given` are _generic_ and may be associated with types or type predicates; they get bound via context, as explained elsewhere. Function literal forms are used to define functions but also type generators and a few other things; when used to define functions, the symbols are always associated with types. When defining type generators they're associated with either types or type predicates, and are considered generic. Attributes are always associated with types, as are iteration variables and the `always` symbol. The symbol in `typepred` is always associated with a type predicate.

Generally speaking, multibinding rules allow distinct functions to use the same identifier, and allow any entity-valued symbol to use the same identifier as a type-valued one. Multibinding generally does not apply to generic symbols, attributes within a tuple type, symbols bound to entities other than functions. The signatures of multibound functions must differ in a way the typechecker can detect, and where no subtyping relationship is possible. The full details of multibinding rules are found in [normal-typechecking](#normal-typechecking).

### Val and Var Statements

The paradigmatic declaration is the `val` statement, which binds one or more symbols to entities, types, type generators, or type predicates. Symbols declared with `val` may not be assigned to or modified. The `var` statement is different only in this respect -- that symbols declared with `var` can be assigned to and/or modified. As such, `var` is not allowed to occur in function literals tagged as pure, nor may types, type generators, or type predicates be declared with `var`.

The syntax is as follows:

    <decl>		 ::= <declword> <declglist>
    <declword>	 ::= 'val' | 'var'
    <declglist>	::= ( <decl1> | <decl2> ) ( ',' <declglist> )*
    <decl1>		::= <ident> ( ':' <xtypedecl> )? '=' <expr>
    <decl2>		::= <identlist> ':' <xtypedecl>
    <identlist>	::= <ident> ( ',' <identlist> )*
    <xtypedecl>	::= <typedecl> | 'type'

The relatively complex syntax allows one to declare multiple (unbound) symbols and to optionally omit type information. Binding to type generators and type predicates occurs when the `<expr>` following `=` designates one of these things; in both these cases, the "type" part (introduced by `:`) _must_ be omitted, as there are no keywords to indicate these things. (Note: the only expression, other than a symbol, that designates a type predicate is `typepred(...)`. Similarly, the only type generator is a function literal form with `typegen` as the purity word and that returns a type.)

On the other hand, it is possible, but not required, to explicitly label a type-valued symbol, so both of

    val aType = tuple(x,y:integer)
    val aType:type = tuple(x,y:integer)

have the same effect.

Declarations in d8m take effect immediately, so in a statement like

    val first = 1, next = other + first

the binding of `next` unambiguously depends on that of the `first` in the same statement.

Multibinding rules, which are detailed in [normal-typechecking](#normal-typechecking), allow an identifier to be bound to both a type and one or more entities (typically functions). As a result, there is an obscure potential for ambiguity between type and entity interpretations of a statement like

    val bar = foo

Such statements are stylistically bad; formally, the entity-valued interpretation is preferred if the declaration is actually ambiguous (that is, if `foo` can be interpreted as either a type or an entity).

Functions are declared in the same way as any other entity. They will almost always be declared with `val`, although function-valued `var` declarations are allowed. The bodies of function literals usually include enough type information to infer the return type, so that part of the type declaration is usually omitted when functions are defined. The syntax for function literals is

    <ftnlit>	::= <flitsig> '{' <stmtlist> '}'
    <flitsig>	::= '\' <purity>? '(' <funargs>? ')' ( '->' <typedecl> )?
    <purity>	::= 'pure' | 'mod' | 'imp'
    <funargs>	::= ( <nodfl> | <withdfl> ) ( ',' <funargs> )*
    <nodfl>	::= <identlist> ':' <typedecl>
    <withdfl>	::= <ident> ( ':' <typedecl> )? '=' <expr>

In words, the `<ftnlit>` consists of a signature with optional return type, and a function body within curly braces. The `<funargs>` part of the signature defines (unbound) symbols, as discussed earlier. The `<withdfl>` option defines functions with default arguments, and may be used only in function literals bound to symbols. Formally, a `<ftnlit>` with _n_ `<withdfl>` productions defines 2^n distinct functions, corresponding to each of the formal arguments being present or absent. All of these must be TC-distinct. See [typechecking-details](#typechecking-details) for further discussion. If the return type is omitted, the typechecker attempts to infer it; it is an error if this fails.

Recursive functions cannot be defined anonymously, only with `val` (or `var`). Also, for definitions that are recursive, the return type _must be included_ in the `<flitsig>` part the `<ftnlit>`, because it can be difficult to infer the return type of recursive functions, and it makes for clearer code anyway. Think of it this way: the typechecker can either infer the return type or detect recursion, but not both.

Type generators use the same syntax as function literals, with `typegen` as the purity word. They must return a type (usually an `extend` expression, though any type-valued expression will do). The formal arguments can be declared with `::` for type-valued arguments, or `:` for entity-valued ones; and the return type may be omitted or written as `type`. Anonymous type generators are not allowed; nor may type generators have default arguments.

Normally, it is not allowed to redefine a symbol in a scope. However, this rule doesn't apply to unbound symbols. This lets us use unbound symbols to set up mutually recursive declarations of either types or functions. For types, one can start with

    val MRT0, MRT1:type

to establish that these symbols designate types. Then repeat with bindings:

    val MRT0 = tuple(...MRT1...)
    val MRT1 = tuple(...MRT0...)

Recursive types are subject to a rule that ensures the existence of finite instances and avoid possible ambiguities: each path from the definition of a symbol to its occurrence in some (possibly different) symbol's definition must pass through `ortype` or `list`, and if not through `list`, it must pass through a tuple type. Thus, a definition like

    val RT = tuple(a:integer, b:tuple(c:string, d:list(RT)))

is allowed (because `RT` passes through `list`), whereas

    val RT = tuple(a:integer, b:RT)

is not allowed. Also

    val RT = ortype(a:integer, b:tuple(x:float, y:RT))

is allowed (because the `ortype` based recursion passes through `tuple`) whereas

    val RT = ortype(a:integer, b:RT)

does not because it's ambiguous whether `cast(12, RT)` is variant `a` or `b`.

There are other uses for unbound symbols in rewrite rules and certain manifest evaluation situations. See [meta](#meta) for details. Otherwise, it is an error for an unbound symbol to occur in a query.

### Given

We've seen the `given` statement quite extensively in examples. Here we cover both syntax and semantics more formally, with rules about what can occur in the scope of a `given` statement and what they mean. The syntax is

    <givenstmt>	::= 'given' '(' <gdecllist> ')' <stmt>
    <gdecllist>	::= <decl3> { ',' <gdecllist> }
    <decl3>		::=  <identlist> ( ':' <typedecl> ) | ( '::' <typepred> )

The `<stmt>` is usually a statement list; it may contain certain `val` statements and all `STMap` statements. The allowed `val` statements are those whose binding is a function, type generator, type predicate, or xprftn. We discuss these restrictions in more detail later in this section.

The symbols introduced in the `<gdecllist>` are _generic symbols_; symbols which are bound implicitly from the context of their use, rather than explicitly, via instantiation in function calls. A symbol declared with `::` is constrained by a type predicate and bound to a type, so we call it a generic type symbol and sometimes say _GTS_. A symbol declared with `:` is constrained by a type and bound to an entity, so by contrast we call it a generic entity symbol or _GES_.

Any generic symbol that occurs just once in a definition effectively abstracts that definition with a _forall_ quantifier. This applies to both type and entity symbols. A generic symbol always designates the same thing so multiple uses in a statement constrain the expression's meaning. It's often convenient to think of this operationally, like pattern matching: the first use binds the generic symbol to a specific value (such as a specific type) and later uses must match the same type.

Generic symbols work as intended only if they can be bound in the context of use. For example, a function definition that uses a GTS outside the signature cannot bind the symbol, since the body is not present or inferred when the function symbol is encountered. Therefore, such a use of a GTS is illegal. Similarly, the only way a GES can occur in a function signature is as the size of a `space` type. Therefore, the only legal use of a GES in a function definition is as the size of a `space` type that occurs in the signature.

This principle of accessibility for binding motivates all the rules about where generic symbols can be used. For functional forms, each generic symbol must occur in the signature; for other allowed statement types, there are no restrictions. The use of GTS's in `STMap` statements is conventional -- it applies them to type schemas, and the first occurrence should be in the `source` binding of the statement. The use of GES's in `STMap` statements is _not_ conventional and is discussed in the appropriate [section](#STMap).

As with `val` and `var` statements, the declarations in `given` statements take effect immediately, both in the header and the body. For example, in a statement like

    given(T::entity, PT:\(T)->boolean) { ... }

`PT` designates a predicate on `T`'s.

The distinction between generic symbols inferred from use and supplied explicitly can be illustrated with an earlier example. In the [introduction to type generators](#type-generators) we gave an example that mixed GTS and GES args:

    val olist = \typegen(T::entity, U::ordered, oextract:\(T)->U) { ... }

It's also possible to define this with `U` inferred instead of supplied:

    given(U::ordered) val olist = \typegen(T::entity, oextract:\(T)->U) { ... }

In this version, `olist` has 2 arguments instead of 3, and the ordered type is inferred from the return type of the supplied "extraction function". The choice is a matter of taste.

### Dimension

The `dimension` statement is simple both syntactically and conceptually. It has the form of a function call:

    dimension(qty, baseu)

is a `dimension` statement; both arguments are required to be identifiers. The statement defines the first identifier (`qty` in the example) as a quantity type and the second as the _baseunit_ of the newly defined quantity type. The baseunit is simply an entity whose type is the quantity. Other units can be defined in terms of this one. For example,

    dimension(weight, kilogram)

defines `weight` as a type and `kilogram` as a `weight`; one can then write

    val pound = 0.453592*kilogram

## Scopes

New scopes are created in the body of function literal forms (including method definitions and type generators), the statement lists associated with control statements, the body of `extend` expressions, and the second argument of chain operators. Symbols introduced in one scope may shadow symbols introduced in outer scopes. In any context where multibinding is possible, shadowing only applies to bindings that are not TC-distinct, because TC-distinct definitions don't conflict.

Certain scope boundaries also introduce a binding for the identifier `self`: in the body of `extend`, where `self` means an instance of the type being defined; and in the body of method definitions, where `self` names the receiver. In the body of an `each` statement or a `map` expression, the iteration variable is bound to elements of the enumerable. And in the body of a function literal form, the arguments named in the signature are part of the local scope.

An expression in one scope can use symbols already defined in any enclosing scope. This causes no difficulties when some nested scope of a function body uses symbols defined in the beginning of the function or as its arguments, but the consequences become harder to think about when the function defines local functions and/or types. An example for types is when a type definition local to a function has methods, or possibly parameters, that mention symbols defined locally. (The case of parameters applies only to the dimensions of space types.) This reduces to something recognizably like a closure given the close relationship of methods to functions. For nested function definitions, non-local but non-global symbol references result in what are typically called lexical closures. D8m's rules about modding apply in the usual way to such cases. Thus, vars in imp or mod functions, and the receiver of a mod function, may be modded in a nested definition. However, vars in nested pure function literals may not be assigned to.

Another set of symbol accessibility issues arises from imported modules. As discussed in [import-and-export](#import-and-export), the symbols defined in imported modules may be integrated into the scope where they are imported, or left in a separate symbol table and accessed via qualified identifier syntax.

In function calls, the typechecker always checks for methods before functions. That is, given `f(x)` where `x` infers to type `T`, the typechecker checks first whether `T` has a method named `f`, and failing that, whether there is a function `f:\(T)->U` in any nested scope.

Generally speaking, symbols may not be redefined within a scope, but there are subtleties to this rule. A TC-distinct definition involving an identifier is not a redefinition in cases where multibinding is allowed. The notion of TC-distinctness is itself complicated by generics. Suppose a function `F` is defined in a `given` statement with an argument declared
in the `given` header as `T::TP` where `TP` is some type predicate. So

    given(T::TP) val F = \(x:T, ...) { ... }

Conceptually, `F` is defined for any type matching `TP`. The multibinding restrictions on `F` depend on how we extend the definition of TC-distinctness to generics. See [typechecking-details](#typechecking-details) for the details. There are two built in functions on all, or nearly all, types: `$==` and `copy`. The no-redefinition rule means you cannot define functions on either of these in global scope. However, you can define _methods_ with those names.

Although `extend` creates a new scope, the methods of the basetype are part of the extended type and hence are in the new scope. Therefore, the no-redefinition rule applies to method definitions, while shadowing rules do not apply. The upshot is that d8m does not allow type extensions to redefine existing methods. (Standard multibinding rules do allow the identifier of an existing method to be used for a TC-distinct method.)

## State

The world is full of state, and we really can't avoid the concept when we discuss how programs interact with the world. However, this is not a usual topic of programming language specifications, which usually cover only how state works inside programs. I feel that both topics need discussion. This section covers d8m's rules about internal state, while [the-outside-world](#the-outside-world) talks about how d8m programs can express facts about their interactions with the outside world.

The idea of moddable types came up in [an earlier section](#structural-aspects) where it was also mentioned that `mod` functions have a first argument of a moddable type and that only this argument may ever be modified. It's time to give all the details. D8m provides functions that modify state; this means that the language semantics supports this. One of the initial goals I had for d8m as I developed it was the features that turned into the `@` operator (see [here](#ontime-and-simulation)), which models things changing in time. Since state is surely the most natural way to think about modeling such things, it's not surprising I wanted to include state. My attitude is that state is actually a very natural way to think about many kinds of problems and that the reason state has gotten kind of a bad reputation in programming languages is that it has been too poorly structured and contained. This allows people to do things that are terrible from various points of view (such as maintaining, scaling, parallelizing, etc) in the supposed service of efficiency, faster code development, or the like.

So d8m has state and you're encouraged to use it, but its use is restricted and channeled in important ways. The operations that change state are

1. the built in mod methods of `list` and `space`
2. assignment to, and mod methods on, tuples and their attributes
3. mod functions and methods built on any of the above
4. certain aspects of the `@` operator

One of the important restrictions on mod functions (including methods) is that only the first argument may be modified. Thus, there is no way ever to modify any function argument other than its receiver. (There is an asterisk on this rule: the effective assert predicate `rcvrLinked` ([details](#rcvrLinked)) can be asserted to state that some argument is part of the receiver, i.e. linked to the receiver; when this is done, the argument is allowed to be modified.) This rule makes it easier to keep track of every possible modification path for entities of any given type.

In pure functions, the rules about state are simple: only pure functions may be called, `var` symbols may not be declared, and assignment is never permitted. There is actually an asterisk to this rule too, because d8m has a _purify_ option that lets users tell the compiler to ignore the state modifying nature of functions. This is intended for things like pseudo-random number generators, loggers, and so on. Sometimes, state modification is sufficiently isolated to be mostly ignored. Details [here](#purified).

D8m also has the `imp` option for defining pure functions using an imperative style. Detailed rules are in the next paragraph; the point of these rules is to ensure that the function cannot change entities visible outside the function, so it's pure when viewed from the outside.

`Imp` and `mod` functions may define symbols with `var`, and such symbols may be modded and assigned to, but may not be initialized or assigned to with non-locally defined moddable symbols (including function arguments). This means that if there is a control flow path that binds a non-local symbol of a moddable type to one used in a mod call, or conditionally assigns a non-local moddable symbol to a local one, the definition is rejected. For example, consider

    val badfn = \imp(x:list(integer), y:integer) {
    	var foo:list(integer) = []
    	if(x.count != 0 && y > x.last) foo = x
    	each(a^x) if(a > y) foo.pushb(a) else foo.pushf(a)
    	foo
    }

This will be rejected because `foo` is conditionally assigned to `x`, which is of moddable type and not locally declared. Operationally, these rules require "conservative dataflow analysis" of `imp` functions.

Assignment is never permitted in pure functions. In `imp` and `mod` functions it is permitted on locally declared variables and attributes of locally created entities. For mod functions, also on attributes of the first argument (which for methods is `self`, the receiver). Note that assignments of the form `a[b] = c` are rewritten to `lvalindex(a, b, c)`, so they are subject to the rules about mod functions stated earlier, rather than those pertaining to assignment. An important consequence of the assignment rules is that you cannot assign to a globally defined symbol or its attributes. Ever. Still, you must declare global symbols with `var` to call mod functions on them or assign to their attributes. This reinforces the point that queries, which compile programs and often return answers, may internally change the state of entities defined globally, but never change the state of entities in the binding environment.

Conservative dataflow analysis ("CDFA") plays a role in enforcing a number of d8m's restrictions on modding. In addition to the already mentioned locality restrictions for modding in `imp` functions, it comes into play in enforcing the rule against modding non-receiver arguments to mod functions. One might think it possible to work around this rule by passing such an argument to a locally defined moddable entity and calling a mod function on the entity; CDFA can (and must) detect and prevent this.

There are some unhappy interactions between the `imp` purity rule and generic functions, specifically when some but not all types that match one or more of the generic type symbols cause the rule to fail. The problem is nicely illustrated by `reduce` as defined [here](#definitions-in-generic-context). It's generic on `T::entity` which includes moddable types. Now consider instantiating `reduce` in a context where `T` binds to `list(U)`. Then the third formal argument `initv` gets type `list(U)`, so when it initializes `accum`, the `imp` rule is violated.

The most conservative possible solution to this problem is to forbid generic definitions of functions whose generic symbols permit any violation of their purity rules. This is _not_ the solution we adopt. First, we decree specifically that purity rules are enforced when generic functions are instantiated, not when they are defined. Second, we note that instantiations of generic functions almost always occur in function calls, and that when they do, we have extra information about the formal arguments, which we can use to make the rule enforcement more flexible. After all, the purpose of the `imp` purity rule is to ensure that such functions "look pure" from the outside.

For example, a violating use of reduce is in the following definition of the list function `flatten1`:

    given(U::entity) {
      val flatten1 = \(lst:list(list(U))) -> list(U) { reduce(lst, $+, []) }
    }

The type of the `initv` argument to `reduce` is `list(U)` and its assignment to `accum` violates the `imp` purity rule. But we know that `initv` is bound to `[]` which cannot be externally visible, and hence doesn't violate the purpose of the `imp` rule. A d8m compiler is required to permit instantiated generic functions that violate the `imp` rule if the violation is traceable to a formal argument and every instantiation of the function binds the argument to `[]`. It's possible that in the future, other "safe" values will be added to this exception.

Type systems that allow state change must provide a consistent notion of equality, one that goes beyond the standard mathematical notion of "extensional" equality, since we can't tell if something has changed unless it has some identity apart from its extension. Thus we'll say that stateful entities can be _identical_ or not. We'll call the predicate for this "object" equality, or "identity". Accordingly, d8m provides predicates for both extensional and object equality, see [equality](#equality). The definition of object equality will necessarily clarify which functions create new entities (in the object sense) and which modify existing ones. This is discussed, along with the related issues of copying and converting entities among types, in [equality](#equality).

### The Outside World

D8m uses ideas from (elementary) algebra and other disciplines in math and computer science, to create an internally focused world of entities. Everything in this specification so far is about this world of entities, and machinery that helps in structuring this world, such as generics, type definitions parameterized by types, features to support abstraction in type definitions, chained operations on collections, and so on.
Actual programs are run on real computers and interact with the world, often in complex ways.
D8m structures these interactions somewhat differently from most programming languages, a topic that is covered in this section.

We focus on two properties, which we attribute mostly to functions but occasionally to types as well.
The most important property is that of being _connected_ to the outside world.
We won't define this formally, but informally, it means the result of the function depends on, or calling the function changes, the state of the world outside the model d8m provides.
In d8m, the property of being connected always resides ultimately in functions and types accessed through _go packages_.
Formally, this is expressed by the rule that a connected function must either be identified as such in a package definition, or must call at least one connected function.
Obvious examples of connected functions include those that access files or talk to networks. But the outside world also involves real time (as opposed to the simulated time that's part of d8m's internal world). So the `Now()` function in go's `time` package is connected.
Not all go packages define connected functions, and in packages that define connected functions, not all exported functions are connected.
For example, no functions in go's `math/rand` package are connected, and while go's `time` package defines `Now` and other connected functions, many of its functions concern format conversion and are not connected.

The second property is _deferred_.
This concerns how queries are treated.
Only connected functions can be deferred.
A deferred function gets a value at runtime, so a query that calls a deferred function results in a runnable program rather than an answer.
Some connected functions are inherently deferred. For example, the functions in go's `flag` package parse command line arguments, so these functions are both connected and deferred, and a query using any of these functions creates an executable program that can be run from a command line interface.
On the other hand, a query to get the length of a file is certainly connected, but if the filename is part of the query, it is not necessarily deferred.
The notion of deferred value is tied up with the rules about how the compiler interprets different expressions, which is defined below.
A d8m compiler should provide an option for compiling queries as if deferred.

Although the semantics of _connected_ and _purified_ are very different, the restrictions on how the compiler treats them are quite similar. Calls to connected functions cannot be moved or eliminated, just like purified functions. Also, connected functions may be nothing-valued even if pure. The idea of a pure connected function may seem paradoxical, but the paradox is resolved by understanding that the concept of function purity in d8m's type system has to do only with internal state. A connected function that doesn't modify internal entities may well change something in the outside world, but it is still pure as far as d8m is concerned. For example, a logging function that writes to a file is connected and may be purified.

While connected functions are ultimately derived from go packages, connected types are purely user defined, using an effective assert predictate (see [connected](#connected)). Their role is to provide models for data structures that have any of the "magical" properties commonly associated with connected functions. An obvious example is file descriptors. Normally, programs access files by first "opening" them, then using the resulting magical entity (known as a file descriptor) to read and write them. In our case, these magical entities are instances of go types. In order to build similar abstractions in d8m, we need d8m types that have the desired affordances, one of which will be the magical entity provided by a connected function (such as `os.Open`), which we'll use as a "black box".

The go documentation doesn't tell you the internal structure of a file descriptor. Instead, it tells you it's a struct that "contains filtered or unexported fields". You're allowed to read the source code and learn the definition of type `File`. But if you do so and then write something in go like

    foo := &File{...}

you will not get a valid file descriptor. Calling `os.Read` with `foo` will likely result in a panic, and will certainly not return the contents of a file. This observation inspires d8m's main rule about connected types, which is that they cannot be created with litforms (directly or indirectly). Instead, the proper way to define connected types is to define a normal type with a name like `CT`, then define a connected function `F` that returns `CT`, then use an effective assert predicate to declare `CT` to be a connected type. The definition of `F` uses a litform (or `lithook` method) to create the `CT` it returns, which is allowed because `CT` is not yet a connected type. Once it is, calling `F` is the only way to create a `CT`. Normally, these definitions and assertions will occur in a module.

When combined with modules and rewrite rules, connected functions and types can be used to create program templates for using various library functions that should in many cases be much easier than the way this is done in earlier programming languages (including go). I call this idea "scaffolding"; there are more details in the section on rewrite rules (see [rewrite-rule-examples](#rewrite-rule-examples)).

## Expressions

### Normal Expressions

The standard way to make an expression is by composing function calls over symbols and literals. The conditional expression is also available, either in the `?:` form or in `if` syntax (which is covered in [control-flow](#control-flow)). Expressions are evaluated by evaluating the arguments in left to right order, evaluating the function, then calling it.

Most unary and binary expressions are rewritten to function calls and these cases are listed in
the [appendix](#methods-on-built-in-types). Here, we review these operators, with their precedence and
associativity rules. Precedences of binary operators are given in the following table:

| precedence | operators                   |
| :--------- | :-------------------------- |
| 1          | `~`, `=>`, `@`              |
| 2          | `=`, `+=`, `-=`, `*=`, `/=` |
| 3          | `&&`, `\|\|`, `^`, `?:`     |
| 4          | `==`, `!=`, `!==`           |
| 5          | `<`, `>`, `<=`, `>=`        |
| 6          | `in`                        |
| 7          | `..`, `...`                 |
| 8          | `+`, `-`                    |
| 9          | `*`, `/`, `%`               |
| 10         | `.`                         |

Of the above, `&&` and `||` are often called "logical operators", and while their behavior as functions does correspond
to AND and OR of logic, what's really interesting about them is operational -- an `&&` chain evaluates operands until
the first `false` one while an `||` chain evaluates operands until the first `true` one. As such, these aren't rewritten
to functions, and have guarding behavior, as discussed [here](#special-properties-of-type-nil).

Several more of these operators aren't rewritten to functions: as detailed
in [the next section](#chain-operators) the `..` and `...` operators turn into literal
values of specially interpreted types that represent ranges. The `^` is used
only to introduce the iteration variable in `each` statements, while `~` is used
in `always` and anchor tagged literals. And while `@` sometimes is rewritten to a function,
its interpretation is complex; it is covered [here](#the-operator).

The `in` keyword is predefined as a presence predicate for lists, i.e.

    x in lst

generally is true if `x` is in list `lst`. In order to make this work as a list method, `in`
has the unusual property of mapping to the `$in` function with arguments in reverse
order, so the above becomes `$in(lst, x)`.

The following sub-sections cover expressions that are privileged or special in some way.

### Chain Operators

D8m uses a well known strategy of rewriting some syntax to functions. (These features are all part
of the language definition -- d8m does not have macros.) External syntax involving dots,
square and curly brackets, and a few other tokens, translates
into 7 generic functions named `maplst`, `filt`, `filtx`, `filt1`, `filt1x`, `rvalindex` and `lvalindex`.
The following table shows schematic versions of what translates to what; in this table `T` and `U` stand
for arbitrary types, in other words `T`, `U::entity`.
Also, `NP` is short for `nilPossible`, the ortype-generating type generator defined earlier.

| Function    | Syntactic form        | Signature                                                   |
| :---------- | :-------------------- | :---------------------------------------------------------- |
| `maplst`    | `lst.{xfm}`           | `\(list(T), \(T, integer)->U)->list(U)`                     |
| `filt`      | `lst.[pred]`          | `\(list(T), \(T, integer)->boolean)->list(T)`               |
| `filt1`     | `lst[pred]`           | `\(list(T), \(T, integer)->boolean)->NP(T)`                 |
| `filtx`     | `lst.[pred => xfm]`   | `\(list(T),\(T,integer)->boolean,\(T,integer)->U)->list(U)` |
| `filt1x`    | `lst[pred => xfm]`    | `\(list(T),\(T,integer)->boolean, \(T,integer)->U)->NP(U)`  |
| `rvalindex` | `coll[inx]`           | `\(list(T), integer)->T`                                    |
| `lvalindex` | `coll[inx] = val`     | `\mod(list(T),integer,T)->nothing`                          |
| `map`       | `map(s^lst, ...) xfm` | similar to `maplst` but nestable; see below                 |

In words, the form `lst.{xfm}` maps a `list(T)` to a `list(U)` by running `xfm` on
each element of `lst`. (The informal term for this is _dotbrace_.) The four
filtering variants combine two binary choices: return the first or all
matching element(s); apply or not a transform to the result before returning
it. `Filt1` and `filt1x` return the first matching element (or `nil`), while `filt`
and `filtx` return all matching elements. (The informal term for the `lst.[pred]`
operators is _dotbracket_.) `Filtx` and `filt1x` apply a transform to the
returned element(s).

Each of the chain operators has two expressions in the source level syntax: the first designates
the collection that is enumerated for mapping or filtering, while the second becomes the
body of a pure anonymous function whose first argument (named `this`) is the current
enumerated element and which usually has a second argument (named `index`) that is the index of that
element in the collection. That's part of how `this` becomes a special pronoun,
the other part is the convention to allow `this` to be implicit when it's a tuple
whose attributes are referenced. If the `index` argument isn't used, it will be
removed by the compiler. The compiler will always inline the anonymous functions in chain ops.

For dotbrace and dotbracket, the second expression (written `xfm` in the above table)
is allowed to be a statement list whose final expression designates the value of the whole.
This facilitates the rare cases where it's convenient to introduce a temporary in the
body of the chain operator. (Note that it's never very hard to turn a statement list
into an expression, just wrap it in a lambda and call it.)

All the list-producing functions associated with chainops are `enumonly`, so any enumerable can be used in argument position. For example, letting `aSet` be a set, ` aSet.{xfm}` maps the elements of `aSet` through xfm. Since the enumonly rule doesn't change the return type in any way, the result is a list. This is as it should be, since transforms do not generally map sets to sets. Note that dotbracket is different: all elements of `aSet.[pred]` are distinct, even though the expression normally designates a list. However, you can ensure it's a set by including `filt` as a set-valued method of set.

To complement the chain operators, d8m provides infix syntax for literals of two built in and specially interpreted tuple types:

    val rangeI = tuple(low, high, step: integer, inclusive:boolean)
    val rangeF = tuple(low, high, step: float, inclusive:boolean)

An expression of the form `X..Y` where `X` and `Y` infer to `integer` translates to

    [rangeI: X, Y, 1, true]

and similarly if `X` and `Y` infer to `float`:

    [rangeF: X, Y, 1.0, true]

Three dots gets the same result except that `inclusive` is set to `false`:

    X...Y --> [rangeI: X, Y, 1, false] or [rangeF: X, Y, 1.0, false]

These types are also provided with `autocvt` functions so they automatically
convert to `list(integer)` and `list(float)`. Note that there is no
special syntax for non-unit step sizes, but these can be used directly as in

    val evens = \(low,high:integer) { [rangeI: low, high, 2, true] }
    evens(0,50).{this*this}

It's usually just as convenient to put the scaling factor into the brace as in

    (0..25).{ 2*this + 17 }

The special interpretation of `rangeI` and `rangeF` applies when they
are used as lists (and hence, autoconverted to lists). They can be
used to generate actual list structure, but when used in each statements,
they will often end up being converted to control variables for loops,
rather than actual lists.

The `..` and `...` operators have another interpretation when used on integers in an index expression. The expression

    lst[a...b]

is rewritten to

    select(lst, a, b)

Select is a built in method on lists. Similarly,

    lst[a..b]

becomes

    select(lst, a, b-1)

The `map` operator does approximately the same thing as dotbrace but is syntactically like `each`:
a variable binding operator with at least one iterator and optionally, an identifier
that names the index variable. See [the discussion of each](#control-flow).
The purpose of the `map` expression is to let you name the variables explicitly,
whereas dotbrace does it for you. This is a necessity for nested mappings.
For example, if you want to enumerate all the pairs from a list you can write

    map(i^0...lst.count) map(j^(i+1)...lst.count) F(lst[i], lst[j])

Since `map` accepts multiple iterators, this can be simplified to

    map(i^0...lst.count, j^(i+1)...lst.count) F(lst[i], lst[j])

You can't do this with dotbrace because the inner `this` would shadow the outer one.
The mapping of the map expression's body is the same as for dotbrace except that the
first argument of the anonymous function is given the name you choose instead of `this`,
and there is an `index` variable only if you explicitly specify one.

`Rvalindex` and `lvalindex` aren't really chain operators; they're included in this section
because they share the property of being an external syntax that's rewritten to
functional form internally. `Rvalindex` is a built in method of `list` and `space` types;
its internal treatment makes it easy for users to define "generalized indexing"
on other types for which this makes sense. For example, dictionary types commonly
use indexing to reference and assign elements; that works by defining `rvalindex`
and `lvalindex` as methods in a user defined dictionary type. There's a syntax ambiguity
for `X[Y]`, should it be mapped to `rvalindex` or to `filt1`? This is resolved in
the typechecker: if `Y` infers to `boolean`, it's mapped to `filt1`, else to `rvalindex`.
This means you can't define dictionaries with `boolean` keys. You can decide
whether this a bug or a feature.

The chain operators are so called because they're designed to be chained.
An example was given in the introduction:

    terrains.[distance(src.centroid, this.centroid) <= radius].{population}

is a chain consisting of a dotbracket followed by a dotbrace. This is standard practice in functional programming and is a good way to write clear and concise programs. People worry about its efficiency though, and in some cases they're well justified to worry. However, in a compiled language with a minimum of def-use analysis, there is no reason to sacrifice efficiency. Even though the semantics of the above expression involve the list named `terrains` and two lists generated from it, the intermediate list has a single use by construction and can therefore be merged with the next one. Furthermore, this example involves reducing the final `list(integer)` to the sum of its elements. In that case, the final list has a single use and can also be merged. Thus, the proper translation of

    sigma(terrains.[distance(src.centroid, this.centroid) <= radius].{population})

is something like

    var sum = 0
    each(t^terrains) {
      if(distance(src.centroid, t.centroid) < radius) sum += t.population
    }
    sum

Furthermore, if you want to reduce a given list in several different ways,
each of these should be compiled into the same loop, assuming nothing is
being modded. D8m's compiler embodies
all of these strategies, so chain operations are very efficient. While this is
not really a proper matter for a language specification, it's important for
people to understand basic aspects of how a compiler works because that knowledge
feeds into their programming strategies. I consider that a proper implementation
of d8m should be at least as efficient on chain ops as the initial compiler,
because it's important for people to feel comfortable using chain ops anywhere
they make sense.

The versions of dotbracket with `=>` in the expression (mapping to `filtx` and `filt1x`) are equivalent to adding a dotbrace to the chain _except_ when the transform expression contains the symbol `index`. So a typical use of `=>` will have `index` on the right hand side, often by itself. Thus, `lst.[pred => index]` designates the list of indices of `lst` for which `pred` is true, while `lst.[pred].{index}` is something completely different and much less useful.

The body expressions in chain ops do form a scope (as was mentioned [previously](#scopes)), but must be compatible with pure or imp function literals. So `this` can never be modded, although captured nonlocal `var` symbols could be. The use of `return` is not allowed in these expressions.

Another built-in function is `xp` which stands for "transpose". The correct conceptual framework for general transposition is permutation of dimensions in higher dimensional space types. In a language with typed lists and tuples, a typed version is needed that "transposes" from tuples of lists to lists of tuples, and the same for spaces. (The other direction, starting from lists of tuples, can be trivially handled with dotbrace.) In terms of d8m's type system, this is an infinite number of functions, but they're all TC-distinct, so multibinding allows a single identifier, `xp`, to designate all of them. Here's an example of one `xp` function among the infinite number of possible ones:

    given(T1, T2::entity) {
      val xp:\(tuple(a:list(T1), b:list(T2)))->list(tuple(a:T1,b:T2))
    }

Formally, `xp` on a tuple of lists `l1`,...,`lk` produces a list of tuples whose length is the min of the lengths of attributes, i.e., `Lt` where `Lt.count == [L1, ..., Lk].{count}.min`. Our main use for `xp` is to elegantly deliver multiple lists to a function in parallel. (In python and friends, the function to do this is called "zip".) Suppose `A:list(T1)`, `B:list(T2)`, `f:\(T1, T2)->T3`, and we want to create a `list(T3)` by calling `f` on parallel elements of `A` and `B`. The solution is

    [a~A, b~B].xp.{ f(a,b) }

or equivalently

    xp([a~A, b~B]).{ f(a,b) }

The list resulting from `xp` is fed to dotbrace where the xfm is a call to `f`. In this expression, `a` and `b` get interpreted as attributes of the tuple via the "implicit `this`" convention. The symbols `a` and `b` are arbitrary -- formally, they are names of attributes of an anonymous tuple type, and informally they act like names for variables local to the function call. Of course, all the standard chain operators can be used with `xp`. For example, if the input lists `A` and `B` have the same elttype, one can get a list of the indices at which they're equal with

     [a~A, b~B].xp.[ a == b => index ]

The inelegant solution for parallel list enumeration uses indices, like so

    (0...A.count).{ f(A[this], B[this]) }

This is an option for compiler optimization of `xp` when used as above. Indeed, this optimization can be user defined with rewrite rules (see [rewrite-rules](#rewrite-rules)).

The list arguments to the `xp` function are only enumerated, so the enumonly rule applies to them. This generalizes the possible types of the attributes of the incoming tuple from `list` to any `enumerable`.

Several list functions are either built in or defined in the standard definitions file and are discussed here because they're used in this document to define functions and/or examples. One is `reduce`, which can be defined as follows:

    given(T, U::entity) {
    	val reduce = \imp(lst:list(U), fn:\(T,U)->T, init:T) {
    		var rslt = init
    		each(x^lst) rslt = fn(rslt, x)
    		rslt
    	}
    }

Reduce is a classic of functional programming. This definition is subtly different from the one given [earlier](#definitions-in-generic-context), as it allows the reducing function to accumulate into a different type than the list's `elttype`. There's an example after the remaining functions are defined. One of these is `sigma`:

    val sigma = \(lst:list(integer)) { reduce(lst, $+, 0) }
    val sigma = \(lst:list(float)) { reduce(lst, $+, 0.0) }

Another is mean:

    val mean = \(lst:list(integer)) { reduce(lst, $+, 0).to_f/lst.count }
    val mean = \(lst:list(float)) { reduce(lst, $+, 0.0)/lst.count }

In a slightly different vein are `all` and `any`:

    val all = \(lst:list(boolean)) { reduce(lst, \(a,b:boolean) { a && b }, true) }
    val any = \(lst:list(boolean)) { reduce(lst, \(a,b:boolean) { a || b }, false) }

These test whether all, or at least one, of the elements of a `list(boolean)` are true; the list is typically generated with a dotbrace, for example

    all(lst.{this > 0})

is `true` iff all elements of `lst` are positive (or `lst` is empty).

These definitions illustrate some subtleties however: the above definitions look like they apply `&&` and `||` to lists but don't inherit their properties as written. In particular, they cannot "short circuit" the evaluation of either the list or its reduction. I like to think of this as being because `&&` and `||` are not functions, but have extra properties. A more concrete definition captures the short circuit properties:

    val all = \imp(lst:list(boolean)) {
      var rslt = true
      each(x^lst) unless(x) { rslt = false; break }
      rslt
    }

and similarly for `any`. Even this definition may not sufficiently capture a programmer's intentions when generating the list argument involves modding and/or connected functions. A fuller discussion of these issues is found in the appropriate sections.

Reduce fits into the standard chainop flow, and since the functions just shown are applications of `reduce`, they fit too. When using `reduce`, it will be very common for `T` and `U` to be bound to the same type. An example of when they might be different is in defining a function that finds the maximum of some attribute of an unordered list. For example, given a definition of 2-d points as

    val pt2d = tuple(x, y:float)

you could define the maximum along the `x` axis as

    \(lst:list(pt2d)) { reduce(lst, \(p:pt2d, mx:float) { max(p.x, mx) }, SmallestFloat) }

### Built In Pseudo-Functions

A number of d8m's built in features have the syntactic form of function calls, often with special argument forms and/or restrictions. We will sometimes call these "pseudo-functions". Some have already been covered, like `typepred`. A few more are covered in this section: `cvt`, `autocvt`, `cast`, and `always`.

To define an "official" notion of conversion between two types, use `cvt` with the desired result type as an argument:

    cvt(X, T1)

This is just a convenient syntax to access a multibound function named `cvt` (or `autocvt`), so that type conversion is entirely user defined. Thus, if the inferred type of `X` is `T0`, the typechecker will look for a `$cvt` function with the signature `\(T0)->T1`. If it finds one, it replaces the `cvt` expression with a call to this function, else it declares an error. So, if you decide there should be an official string format for list(integer), you can define it with

    val cvt = \(lst:list(integer)) { "[" + lst.{cvt(this, string)}.join(",") + "]" }

Standard multibinding rules allow the same identifier for functions whose signatures differ only in the return type, as long as the return types are TC-distinct. So one can define multiple `cvt` functions for a type `T0`.

It's worth noting that it is also possible, and may sometimes be more convenient, to define a `T0` to `T1` conversion "from the `T1` side" (i.e., in the definition of `T1`). If `T1` is a tuple type, you can do this with a `lithook` method:

    method lithook = \mod(aT0:T0) { ... }

Then, given `x:T0`, you would write `[T1: x]` instead of `cvt(x, T1)`.

D8m extends the idea of `cvt` with `autocvt`. This is "called" implicitly, when type inference fails because the inferred type doesn't match a constraint. If an `autocvt` function is defined that would fix the failure, the typechecker will insert a call to `autocvt` to fix the failure. So `autocvt` is a perfectly normal function, but it is normally called via the typechecker rather than explicitly. There are built in `autocvt` functions from `integer` to `float`, from `rangeI` to `list(integer)` and from `rangeF` to `list(float)`. Users can define `autocvt` functions in the usual way and thereby affect typechecking in potentially deep ways. Occasionally, it's necessary or convenient to convert explicitly when an `autocvt` function is defined. Accordingly, `cvt` will also match if an appropriate `autocvt` function is defined.

The `cast` pseudo-function has syntax identical to `cvt`, but its purpose is to affect typechecking. `Cast` never generates runtime code and is not affected by function definitions. The expression `cast(X, T)` means that the typechecker should interpret `X` as a `T`. You can use `cvt(X, T)` whenever an appropriate `cvt` function is defined, and the result is a call to that function, which typically creates a new entity. On the other hand, `cast(X, T)` is an error unless the typechecker can, in fact, interpret `X` as a `T`. For example, one can say `cast(X, T)` if the inferred type of `X` is `T`. This is formally superfluous, but can be useful when style considerations suggest it. It can disambiguate multibound symbols, as in

    cast($+, \(float, float)->float)

to get floating point addition, perhaps when passing one function to another. As a third example, suppose `OT = ortype(a:T1, b:T2)` and `f:\(OT)->T`, and we write

    f(p(a) ? [T1:...] : [T2: ...])

This is semantically correct but will fail because function arguments are typechecked without constraints -- then and else branches appear to be incompatible. You can fix this with cast:

    f(cast(f(p(a) ? [T1:...] : [T2: ...]), OT))

You can use cast for certain cases where related types might be considered [subtypes](#methods-and-more). Specifically, suppose that

    val T1 = extend T0 where {...}
    val x0: T0 = ...
    val x1: T1 = ...

and suppose that no attributes are declared in the where clause defining `T1`. Then both of the following cast expressions work:

    cast(x0, T1)
    cast(x1, T0)

Finally, there are two ways of using `cast` to implicitly apply an ST pair (see [stmap statement](#stmap)).
Normally, you do this with `applyST` (see [applying](#applying-stmaps-and-rewrite-rules)).
To apply an ST pair to a _specific expression_, wrap it in a `cast` whose type argument is the target type of the ST pair.
Thus, `cast(X, T)` is legal if `X` infers to `S` and there is an ST pair `[S, T]`.
Casting also works in reverse on ST pairs; I call this "back casting". Consider an `ST` pair `[T1, T0]` and `x:T0`, then the expression

    cast(x, T1)

says to interpret `x` as a `T1`. The intended use of this is when some concrete type, perhaps externally stored,
can be interpreted as a more abstract one. For example, we might have an ordered list `XL`,
that is a list whose elttype `T` is provided with a `$<` method, and for which we know there
are no duplicate elements with respect to `$<`. Also suppose there's an `ST` pair describing how to implement a
set as an ordered list with no duplicates. Then you could write

    val xlset = cast(XL, set(T))

to create a set-valued symbol `xlset` that is identical to `XL` but works in terms of set methods.
The practical implication is that when you have a big thing that's stored in
some efficient way, you can treat it as an abstract type without an expensive conversion step.

The `always` pseudo-function is different from the others in that it is a variable binding operator
and neither of its arguments are types. Roughly, `always(X, P)` designates `X` while asserting that
it always satisfies the predicate `P`. In order for `P` to refer to `X`, it's given a name.
A more complete schematic form is

    always(x~X, P(x))

where now the occurrence of the symbol `x` in `P` is explicit. Always is formally equivalent
to an immediately applied lambda expression. Specifically, `always(x~X, P)` can be rewritten to

     \(x:T) { assert P; x }(X)

where `T` is the type inferred for `X`.

Since `always` involves `assert` the situations where the language spec requires `always`
to do something devolve from `assert`; a common one is establishing a guard condition.
Thus, a canonical use of `always` is in expressions of the form

    always(x~X, x != nil)

since the assertion removes `nil` as a possibility for the otherwise nilPossible expression `X`.
However, asserted expressions are also required to be manifest (see [assert](#assert)) so

    always(x~X, x > 0)

causes `X` to be evaluated in a context where its positiveness is asserted, which can affect the outcome of rewrite rules.

D8m borrows Go's `fmt.Print` and `fmt.Println`, written as `print` and `println` respectively. Note that these are connected functions in the d8m sense. They are vararg "functions" as in Go but whereas the semantics of Go's functions correspond to their `%v` formatting option, formatting in d8m depends on a built in method `stringify`.

More discussion of stringify is forthcoming.

### Equality

D8m has two equality predicates, written as `$==` and `$===` and defined on all types; both of these are infix so in normal use they are written `x == y` and `x === y`. We characterize these functions as "extensional equality" (for `$==`) and "object equality" (for `$===`). Roughly speaking, `x` and `y` are equal extensionally when they have the same "values" and they are equal "as objects" when they occupy the same memory location. Object equality defined in this way sounds very low level but a more abstract definition can be made, as we will see.

For each equality predicate there is a corresponding negative predicate: `!=` and `!==`. There is no difference between `x != y` and `!(x == y)` and similarly for `!==`.

Before we give definitions for `$==` and `$===`, we note that these are the only functions defined on every type. As such, it makes sense to slightly relax typechecking restrictions on them, especially regarding ortypes. Whereas most functions require their arguments to be of a specific type or one of its subtypes, equality can be less rigorous in typechecking. Consider an expression of the form `A == B`, `A === B`, `A != B`, or `A !== B`, and suppose the typechecker infers `T0` for `A` and `T1` for `B`. The typechecker accepts the expression if `T0` and `T1` are in a subtype relationship. As stated in the relevant [section](#methods-and-more), subtyping in d8m is exclusively about ortypes. Informally, this means that the typechecker accepts "plausible ortypes" for equality checking.

The built in `copy` function is related to `$==` and `$===`; it is defined for every moddable type, and returns a "shallow copy" of its argument. This means that for any moddable `x`,

    copy(x) == x

but

    copy(x) !== x

And "shallow" means that given any list `L`

    all([orig~L, copy~copy(L)].xp.{orig === copy})

and given any `S:space(T, [n])`

    val cS = copy(S); all((0...n).{S[this] === cS[this]}

and given any tuple `T`, every attribute of the copy is `===` to the corresponding attribute of the original.

Formally, `$==` is defined as follows. First, if the arguments are of different type families, `$==` is false. Otherwise, for basic types, the main idea is that `x == y` just in case the literal values of `x` and `y` are equal as strings. This is precisely correct for types `label`, `boolean`, and `nil`; but not for numeric and string types. For integers, one needs a canonical way of writing them (for example, in decimal radix with no leading zeros and an optional leading '`-`' character). For floats, the same applies plus there are precision limits; the correct definition is actually rather concrete and amounts to characterizing which numeric strings map to distinct bit patterns in IEEE 64-bit floating point. The way to get a canonical representation of strings depends on the details of string interpolation but the main idea is that each pattern of `list(byte)` is a distinct string and string identity is based on this.

We extend the definition of `$==` to non-basic types as follows. For `x, y:list(T)` `x == y` iff `x.count == y.count && all([a~x,b~y].xp.{a == b})`. In other words, the list lengths are equal and each element is equal. This is a recursive definition; the inner test applies to elements of `T`. The definition is the same for `space` types; the length of a `space` is part of its type identity, but since the `count` method is defined, this changes nothing in the definition. For `tuple` types, let `attribs(T)` be the list of attributes of tuple type `T`. Given `x,y:T` we have `x == y` iff for each `a` in `attribs(T)`, `x.a == y.a`. Again, this is recursive with each attribute potentially having a different type. Finally, for function types, `$==` is defined to be the same as `$===`. For ortypes, `$==` is defined by first checking that the tags are equal and if so, applying `$==` to the values. As mentioned earlier, the typechecker allows `$==` between an ortype `OT` and entities whose inferred types are in the variants of `OT`. For such cases, the tag of the non-ortype is known, call it `L`; the definition of `$==` is that the tag of the ortype is `L` and its value is `$==` to the other argument.

When types are recursively defined, the above definition is well founded, but the code that evaluates `$==` in compiled programs will generally need to keep track of which entities it has already seen in order to prevent infinite loops. (Ironically, this requires using the `$===` function!)

Now let's give the definition of `$===`. It behaves the same as `$==` with respect to arguments of different type families. For basic types, `$==` and `$===` are the same function. We have already seen that this is also the case for function types. This leaves `list`, `space`, and `tuple` types which happen to be the moddable types. (This is not a coincidence.) We define `$===` by positing a function `p_addr:\(M)->label` where `M` is any moddable type. This function isn't currently required to be a builtin function but that could change, because it's actually quite a useful function, which can be made safe. (Defining its result as a label is a key part of ensuring safety, since there are few methods defined on label and type `label` cannot be extended.) We define how `p_addr` behaves, then define `$===` as `\(x,y:M) { x.p_addr == y.p_addr }`. Each time a litform is encountered in the execution of a program, the entity designated by that litform has a `p_addr` distinct from any other in the execution state of the program. Given an entity `x:M`, `x.p_addr` is preserved by every call to a mod function with `x` as receiver. This covers most cases. To complete the definition, we need to discuss built in methods and functions (including pseudo-functions). The `copy` function always creates a new `p_addr`; `always` never does. Mod methods of `list` and `space` preserve the `p_addr` of their receivers and if the return type equals the receiver type, the return value is the receiver. Other `list` and `space` methods that return `list` or `space` entities return new `p_addr`'s.

The properties of user defined functions with respect to `p_addr` can be inferred from the rules given so far using standard CDFA rules.

It is never allowed to redefine `$===`, but `$==` can be user defined (as a method) for new types. A canonical use for this is in defining sets. Many other uses are possible. For example, one might want a list type whose notion of equality is object equality on the list elements:

    val objlist = \(T::entity) {
    	extend list(T) where {
    		method $== = \(other:T) { count == count(other) && all([a~this,b~other].xp.{a === b})}
    	}
    }

Note that `objlist` is not TC-distinct from `list` (because its affordances are identical), and that semantically, `objlist` is identical to `list` unless its `elttype` is moddable.

Although `$===` implies `$==`, one needs to use this fact carefully for moddables. Given a moddable type `M`, a function `f:\mod(M)->M`, and `m:M`, it's possible for `m === f(m)` while `copy(m) != f(m)`. (And it's always the case that `copy(m) !== f(m)`.) We'll still have `m == f(m)` but the extensional test that's nominally applied here compares the value of `m` with itself _after_ `f` has modified it.

### The @ Operator

The `@` token is an infix binary operator that connects a situated entity (on the left) with a simulated time (on the right); we'll write this schematically as `X@t`. We sometimes interpret `X` by extension as an unsituated entity that contains situated entities, in a way we'll describe later. `X@t` always designates `X` at time `t`. There are various ways for this to happen. It's not really possible to specify precisely how `@` works until we have described `ontime` handlers. Accordingly, we postpone the details to that section ([ontime-and-simulation](#ontime-and-simulation)).

## Statements

All of d8m's statements start with a reserved keyword. The many classical ones are covered in the first sub-section,
while the unusual ones each get a sub-section to themselves. Several of the statements are covered either in
the [declarations section](#declarations) or in the [meta section](#meta), as they respond to issues covered there.

### Control Flow

Many of the control flow statements are lifted directly from C, syntax intact. One reason for adopting this syntax is that I like using braces to set off parts of these statements, and I often use them in single line form. While that's not always appropriate, it sometimes is, and when it is, it can make programs more concise and readable. Using braces means that line breaks are never required just to make the syntax work, and I count that as a good thing.

Several of the control flow statements can also be expressions: `if`, `unless`, and `case`.

The `if` statement is like C:

    if(condxpr) stmt
    if(condxpr) { stmt; stmt; ... }
    if(condxpr) { stmt; stmt; ... } else stmt
    if(condxpr) { stmt; stmt; ... } else { stmt; stmt; ... }
    unless(condxpr) { stmt; stmt; ... }
    unless(condxpr) { stmt; stmt; ... } else { stmt; stmt; ... }
    if(condxpr) { stmt; stmt; ... } else if(condxpr2) { stmt; stmt; ... }

And so on. The typechecking rules are as follows. The inferred type of the `condxpr` must be `boolean`. If the statement is not used as an expression, the `else` clause may be omitted, and the only other requirement is that clauses must typecheck if present. Otherwise, the `else` clause must be present and both clauses must infer to the same type or one clause may infer to `exit`. The inferred type of the whole statement is that of both clauses, or of the non-exit clause. The ternary infix operator `?:` translates to if-then-else before typechecking, so these rules apply. The `unless` keyword is exactly like `if` except that the sense of the `condxpr` is inverted.

When the `condxpr` part of an `if` (or `unless`) statement concerns ortypes, the statement may act as a guard, either for its then and else clauses or, if these include control flow statements like `continue`, `break`, or `return`, for statements occurring later in the scope. These issues are covered in [nil](#special-properties-of-type-nil).

The four looping statements are `each`, `map`, `while`, `loop`. `Map` is always an expression and was introduced in [chain-operators](#chain-operators). `Each` doesn't occur in C, its syntax is

    <eachstmt>		::= 'each' '(' <loopexprs> ')' <stmts>
    <loopexprs>		::= <iterator> | <ident> ( ',' <loopexprs> )*
    <iterator>		::= <ident> '^' <expr>

The `<loopexprs>` declare variables that are used in the `<eachstmt>` body. Most commonly, there is a single `<iterator>`. (As shown here, an `<iterator>` is an `<ident>` bound to an `<expr>` with `^`.) An additional `<ident>` following the `<iterator>` is treated as an index variable for the loop. More than one `<iterator>` (and optional `<ident>`) translates into nested each statements, nested in left to right order. (Thus, the right-most iterator is most deeply nested). In each `<iterator>`, `<expr>` must designate an enumerable; see [abstract-types](#abstract-types) for details. Each `<ident>`, whether for iteration or indexing, is declared as a symbol in `<stmts>` which forms a local scope; the `<iterator>`'s type is the elttype of the inferred type of `<expr>`. These variables may not be assigned to, but may otherwise be treated as `var` if the type is moddable. `Each` is always a statement, i.e., its type is `nothing`.

The full truth about the `map` expression is that one can introduce indexing variables in the same was as for `each`. Its syntax within the parentheses is identical to `each`.

`Each` statements normally occur in `imp` and `mod` functions, and take their effect through mod function calls or assignments. The enumerated entity may not be modified in the body. In general, the type checker must perform conservative dataflow analysis to enforce this rule, similar to the rules for `imp` functions.

The syntax of `loop` and `while` statement is what you'd expect:

    <loopstmt>        ::= 'loop' <stmts>
    <whilestmt>	   ::= 'while' '(' <expr> ')' <stmts>

where `<expr>` must infer to `boolean`. A `while` statement of the form `while(X) S` is equivalent to

    loop {
    	if(!X) break
    	S
    }

With looping statements comes the need to modify control flow within them. Accordingly, we define `continue` and `break` statements. Neither of these take arguments; they are always statements and they affect the loop they are directly contained in. The `return` statement also affects control flow, by returning from the function it is contained in. Return takes an optional argument. If present, the return statement's inferred type is that of the argument, else nothing. All three of these statements must be the last in their scope, since any statements that follow them are unreachable. This is an error in d8m.

The `case` statement/expression has one syntax used in two ways: either for analyzing entities with non-manifest types, or for chaining if-then-else statements (or expressions). Either can be a statement (`nothing`-valued) or an expression. The syntax is a list of "clauses" wrapped in curly brackets and, for the first variant, preceded by an expression. A "clause" is two expressions separated by `=>`, e.g. `expr1 => expr2`. The expression after `=>` may always be a statement list; if it has more than one statement, it must be wrapped in braces: `{` `}`.

The type analysis form of `case` has an expression after the keyword, which designates the entity whose type we're going to analyze, so its type must be non-manifest (i.e., `ortype` or declared as a typepred). Each clause has a type, or typepred, before the `=>` and the code corresponding to that type after it. Note that ortype tags are ignored here, but since ortype variants must be TC-distinct, nothing changes. For example, if `X` has type `XT` = `ortype(a:T1, b:T2, c:T3)`, one could write

    case X {
    	T1 => foo(...)
    	T2 => bar(...)
    	T3 => other(...)
    }

The branches act as guards in the same sense as for `if`. Thus, in the `T1` branch of this example an occurrence of `X` infers to T1; in the `T2` branch to `T2`, and so on. Types in clauses should be pairwise disjoint; there is no implied evaluation order for clauses.

The second form of `case` statement has no expression after the keyword, and the expression on the left of `=>` is the condition for its clause, a `boolean`-valued expression. For example

    case {
    	foo < 12 => something(...)
    	foo + bar > 17 => somethingElse(...)
    	else => defaultCase(...)
    }

This "boolean case" does have an implied order, which is that of the clauses. Thus, it is entirely equivalent to an if-then-else chain, including with respect to guarding behavior.

Exceptionally, the word `else` designates the default case for either form of case. If used, `else` must be the last branch.

### Assignment

Assignment is always a statement in d8m. It uses infix `=`, which is shared with the token for binding in declarations. Being a statement (nothing-valued) rules out multiple assignment, so `a = b = c` is always an error. Similarly, one cannot do the shortcut thing of writing `if(a = b) {...}`; I consider this a good thing.

A limited and fixed set of _op assignment_ operators is defined, namely `+=`, `-=`, `*=`, and `/=`. These work the same as in most languages. What is perhaps less usual is that expressions of the form `x[y] = z` are rewritten to a function call of the form `lvalindex(x, y, z)`. This is related to the fact that expressions of the form `x[y]` that are not flagged as dotbracket by the typechecker, are rewritten to `rvalindex(x, y)`.

### Import and Export

Every programming language has a file-level mechanism for structuring larger programs. Since d8m compiles to go, using go packages is part of this mechanism in d8m. The other part is importable files written in d8m, which we call _modules_. An `export` statement distinguishes a module from other files containing d8m code. You access both go packages and d8m modules the same way, by using the `import` statement. Its options control two things:

1. whether the imported thing is a module or a go package;
2. how the symbols defined therein are accessed.

The first option is expressed with the word `go`, its presence signals a go package. The second option has a default that you can change with the word `melted` or a clause naming a qualified identifier. Saying `melted` means to "melt" the definitions into the local scope. Otherwise they are held in a separate symbol table that's accessed through a qualified identifier which defaults to the last component of the path but can be explicitly named. Thus, the syntax of import is

    <importstmt>	::= 'import' 'go'? <stringlit> 'melted'? ( 'as' <ident> )?

Here, `<stringlit>` is a string literal with the syntax of a filename, that is, which can include '/' characters and identifiers. It's known in this context as a _path_. If the word `go` is present, the path identifies a go package; else, a d8m module. If the `melted` keyword is present, the exported symbols of the module or package are injected directly into the local scope. In this case, the compiler does the usual error checking to ensure the rules on redefinition of existing symbols are followed. (See [typechecking-details](#typechecking-details) for details.) If `melted` is not present, exported symbols are accessible via a dotted reference whose first component is the _qualified identifier_, which defaults to the last component of the `<stringlit>` (the module/package name). If the `as` keyword is present, the `<ident>` is used instead of this default. At most one of `melted` and `as` is allowed in an import statement.

A natural way to look at the set of files constituting a large program is as a directed graph in which files are the nodes and an import statement in a file `F` creates an edge from `F` to the imported file. (Or the other way around.) Any coherent scheme for modularizing large programs has to require that such graphs be acyclic; d8m is no exception. The presence of go packages in d8m's modularization scheme doesn't complicate this aspect of it; in terms of d8m programs, each go package is a leaf node of the graph (i.e. has out degree 0).

The export statement identifies a source file as a module. It may occur anywhere in the file; as such, it is the only exception in d8m to the rule that symbols may not be used before they're defined. I normally put the export statement near the top of the file so that it can announce that the file is a module and what it exports. The export statement just lists the exported symbols:

    <exportstmt>	::= 'export' <daident>+
    <daident>		::= <ident> | <ident> ':' <typeexpr>

A `<daident>` is an identifier, possibly disambiguated with type information. Disambiguation is used to export only specific definitions of a multibound identifier. If a multibound identifier is exported without qualification, all its definitions are exported. (Note that this includes the case where an identifier is bound to both a type and an entity.) A generic definition is exported as such. It counts as a single definition.

The `<ident>`'s in the `<exportstmt>` are normally symbols bound to types and entities defined in the module. They may also be the qualified identifiers of packages or modules imported in the module. (This rule only applies to modules, since go packages don't explicitly import packages.) Symbols imported in modules are not visible outside the module unless the qualified identifier of the import is given in the export list.

Importing a d8m module is semantically equivalent to reading the file, except that the details of how it affects the binding environment are different. Only the exported symbols enter the binding environment; these definitions may use the non-exported definitions, but programs importing the module will not have access to them.

Modules can contain statements that affect the bdgenv in ways other than symbol definition. For example, they can contain `STMap` statements, `applyST` statements, `rewrite` statements, `xprftn` definitions, various effective assertions, and more. These are always "exported" if they refer to types or symbols that are exported. In other words, when a file is imported, such statements always modify the bdgenv. Therefore, it is an error for a module to include such statements about types that are locally defined and not exported.

Imports are scoped: an import statement occurring in a scope has no effect on the bdgenv outside that scope. Shadowing of qualified identifiers is not allowed; the compiler must declare an error if an import statement attempts to add a qualified identifier that's already in use (in the scope). Also in error is any attempt to introduce a given module or package with distinct qualified identifiers, or both melted and unmelted, in the same scope. Otherwise, it is legal to import a given `<stringlit>` multiple times. The effect of importing twice in the same scope must be same as importing once.

Some examples:

    import go "math/rand" melted

imports the standard go package `math/rand`; its exported symbols are melted into the bdgenv. For example, `Intn` becomes defined in the scope as a `\()->integer` and `Intn(100)` designates a pseudo-random integer in the range `0..99` using the standard seed, obtained by calling the go package function.

    import go "os"

imports the standard go package `os`; its exported symbols become available by writing `os.X`. For example there should now be a type `File` accessed as `os.File`, a function

    Open:\(string) -> ortype(ok:File, err:Goerror)

accessed as `os.Open`, and quite a bit more.

I've found it pleasant to import basic infrastructure (set, dict, olist, and similar) melted, and everything else unmelted (perhaps renamed with the `as` clause).

The way the d8m compiler comes to know which go packages exist and what symbols they contain is compiler-dependent. Consequently, so is the process (if any) for extending the set of packages the compiler recognizes or the go symbols they include. Since anyone can define go packages, a general purpose solution to this must be able to read symbol tables of binary files and figure out how they map to the d8m type system. The initial d8m compiler doesn't provide such a general purpose solution. Instead, a subset of symbols is defined for a subset of the "official" go packages; extensions to new go packages require modifying the compiler source code and recompiling. Many go package definitions involve functions returning multiple values, a feature d8m doesn't have. In such cases, the d8m definition's return type will be either an ortype or a tuple type.

The d8m compiler comes with a standard set of modules which I'll call "the official modules". To support a larger and less standardized ecosystem of modules, d8m provides that prefixes of the paths in module import statements may be mapped to arbitrary locations specified with a standard syntax, either URI or unix filenames. Here, the components of a path are defined in the obvious way, as the `list(string)` resulting from `strings.split(path, "/")`. The mapped paths are held in a file that we'll call the modules DB. The compiler reads the modules DB file on startup and uses it to interpret paths in import statements. Each path in the modules DB has an associated string (its "location" link) representing a URI or unix filename (with the ~/ prefix explicitly allowed) which identifies a directory containing d8m modules. Such directories are searched for the module named by identifier(s) following the prefix. The modules DB entries may have other links, such as to documentation; the compiler ignores any such links. There may be duplicated paths in the modules DB; if so, all the associated location links are searched for the named modules. The names of modules defined in the set of locations associated with any given path should not be duplicated; the code that adds new module DB entries should detect and report violations of this rule.

Thus, module import paths containing a single identifier can only be official modules. Those containing two identifiers may be modules associated with a module DB entry having a single identifier _or_ a subdirectory in the official modules directory. Paths with three identifiers can come from a sub-sub-directory of the official modules directory, a two-identifier modules DB path, or a sub-directory in a one-identifier modules DB path. And so on. When there are multiple interpretations of a given import path, it is explicitly allowed for some of the interpretations to involve subdirectories and others module DB entries. For example, "codecs/json" is an official module; the module in file json.d8m resides in the "codecs" subdirectory of the official modules directory. It is possible for other codecs to be accessed with a path sharing the first word, e.g. `codecs/yaml` might be in a repository defined by a module DB entry. There are also ways to have alternative json modules, but they would have to use a different path, for example, `codecs/alternatives/json`.

These rules are intended to allow module repositories to use a standardized naming scheme, and avoid in-program dependencies on how and where components are installed in a given developer's environment. Everything about the details of the modules DB is compiler dependent: its format, whether it has other links than the location link, how one adds and modifies its entries, etc.

It can be useful to read a file that isn't a module, or to read modules "directly", on occasions when their unexported symbols are needed. This can allow access to non-exported symbols in modules, and can be used to break larger programs into multiple files that are all merged as part of a compilation. To do this use the `load` pseudo-function:

    load(path)

loads the file identified by `path`, which is a string literal. As with import, `path` is a slash-separated list of identifiers. Here, we would like the option to interpret the path as a filename or a module. Accordingly, if the path starts with "modules/", that first identifier is removed and the rest is interpreted exactly like an import path.

In most cases, imported modules should not have "free variables", ie should not use symbols whose definition is intended to be found in the binding environment when the module is imported. But occasionally, such modules are useful, so they are allowed. Good style includes documenting the expectations of such modules when they occur.

### Oncondition

The `oncondition` statement defines a _handler_. In normal programming usage, this means some code that is run when a particular condition applies, such as an "event". As such, the notion is deeply operational, yet it has a declarative flavor too, since the compiler takes care of the coupling between the condition occurring and the handler executing.

The `oncondition` statement defines a handler that runs a given set of code whenever some state changes. Its syntax is as follows:

    <oncond>	::= 'oncondition' <condition> '{' <stmtlist> '}'

where the `<condition>` is a pure, boolean-valued expression that refers to at least one moddable entity. `Oncondition` statements may occur globally, in function literals, and in `extend` expressions over moddable types. In `extend` expressions, the `<condition>` must reference `self` (explicitly or not); this means such handlers apply to every instance of the type in a running program.

`Oncondition` handlers are useful as "probes" for logging and debugging, as well as complex behavior patterns like event-based modeling. The body of the `<oncond>` defines a scope; when occurring in a type definition, it can be usefully thought of as a zero argument, nothing-valued, mod method of the type; one that is called "by condition" rather than by name. When occurring in a function, the `<oncond>`'s `<stmtlist>` must follow the purity rules of that function. In other words, mod calls may not occur in handlers inside pure functions. For logging and debugging, the relevant calls can be purified (see [purified](#purified)) so as to be allowable.

If the `<condition>` part of an `oncondition` statement is manifest, it will be evaluated at compile time. `Oncondition` statements may occur in global context if their `<condition>` is manifest. Manifestness is discussed in [meta](#meta); details of useful built in manifest pseudo-functions are found there as well. The main use of manifest `oncondition` statements is to organize definitions relevant to the meta portion of d8m, especially regarding dependencies among them.

In the rest of this section, we'll assume `oncondition` statements are not manifest. A couple of special expressions are allowed in the `<condition>` and nowhere else. First, for `<oncond>`'s occurring in functions, the word `exit` is allowed as the `<condition>`; this condition obtains when the function is about to exit (after evaluating the `return` expression if any). This closely corresponds to Go's `defer` statement. Second, one can call a pseudo-function named `changed`. Expressions of the form `changed(X)` in the `<condition>` trigger the handler when `X` changes. `X` must be symbol chain of moddable type. The right way to think about `changed(X)` is to imagine that the compiler creates a state variable `Xb` initialized to `X`, replaces the condition with `X != Xb`, and inserts an assignment `Xb = X` as the first thing in the body, so that `changed(X)` becomes `false` as soon as the body is executed.

An operational way to think about `<oncond>` is that it causes the compiler to calculate the set of moddable entities on which its condition depends and to insert code after each code point that might modify any such entity to check whether the condition now obtains and run the `<stmtlist>` if it does. D8m's restrictions on modding entities simplifies this analysis. More sophisticated compilers can run analysis to detect some cases where a change can be proven to not make the condition true and skip the code insertion. Oncondition handlers create some complexities that aren't immediately obvious. To formulate the most important of these precisely, we define two functions:

    refset(H)	the set of moddable entities (possibly) referenced in H's condition
    modset(H)	the set of entities (possibly) modded in H's body

A problem arises when the intersection of `modset(H1)` and `refset(H2)` is non-empty. (This can occur when `H1` == `H2`.) A concrete example should make this clear. Assume the intersection for a handler `H` is `{a}` where `a` is a variable. The intuitive meaning of `H` is that the compiler inserts a statement of the form

    if(H.cond) H.body

after each assignment to `a` and each mod call with `a` as receiver. (And likewise for other elements of `refset(H)`.) But since `a` is modded in `H.body`, at least one of those code points is inside `H.body`. This causes an infinite regress.

There are a few ways we could resolve this issue. We could attempt to forbid overlaps between refsets and modsets, or make the pragmatic but semantically sketchy rule that handlers don't apply inside handlers. I've chosen a more complex option, which resolves the issue in a semantically clean way and lets handlers apply everywhere. The details are in an appendix ([here](#oncondition-handler-interference-details)). The intuition is that intersecting modsets and refsets define a kind of connection that induces a directed graph on handlers; strongly connected components of that graph cause the compiler to insert code more complicated than an `if` statement after every code point where refsets can change. This code repeatedly runs interacting handler bodies while any of their conditions remains true. Most of the time, and especially if your use of handlers is reasonable, all of this can be ignored.

While we're on the topic of complexities associated with `<oncond>` handlers, let's talk about non-determinism. The refset/modset issue was serious because it suggested handler semantics might not be well defined. Whereas non-determinism is just a fact of life when multiple handlers have conditions that can become true at the same time. The question is in what order are they run and the answer is that d8m leaves the order undefined. It's ok for a compiler to put them in any order, or even to run them in separate goroutines or some other version of "in parallel".

### Ontime and Simulation

The `<ontime>` statement defines how state changes in entities of a given type when (simulated) time passes. It may occur only in definitions of tuple types (including situated types), in other words, it may only occur in `extend` expressions that extend `tuple`. Its syntax is similar to `<oncond>` but the condition is much more restricted and the body is different as well:

    <ontime>	::= 'ontime' <condition> <ftnlit>

The `<ftnlit>` (corresponding to the body of the handler) is best thought of as a nothing-valued mod method of the type with a single duration-valued argument. As with all method declarations, the receiver is named `self` and is omitted from the declaration. The `mod` keyword is not required (or allowed). The duration argument is there so that the handler can receive the actual duration that has passed since the last call to the handler. A handler using constant interval sampling will usually ignore this argument.

The `<condition>` can take two forms which correspond to sampling based and event based updating of a situated entity. The sampling based condition may only occur in `extend` statements over situated types; it is of the form `delta(X)` where `X:duration`. Here, `delta` is a pseudo-function that triggers when the situated time of the associated entity has increased by `X` since the last trigger. We call these _delta handlers_. The sampling interval need not be constant. Constant interval sampling is generally the most convenient for model definition though.

The event based condition may occur in either situated or tuple types and takes the form

    X.simT == t0

optionally preceded by some other conditions which will be expressed with `&&` as in

    g1() && g2() && X.simT == t0

In either of these forms `X:situated(T)` and `t0:time`, so that the intuitive meaning of the last clause is that the simulated time of `X` is `t0`. (Recall from [types](#types) that `situated` types have a single built in method `simT:\()->time`.) The optional conditions allow writing code that deactivates the condition, for example when `X` or `t0` is undefined. Note that both `X` and `t0` here designate general expressions with the given types, not necessarily constants or symbols. We call these _event handlers_.

For `<oncond>` we stated that the body is like a mod function that's called "by condition" rather than explicitly. The body of `<ontime>` is a pseudo-method with analogous behavior. The kinds of conditions that cause it to be called arise from certain uses of the `@` operator, whose details we will describe next. As was hinted earlier, an occurrence of `X@t` can mean three different things:

1. a literal form that designates a situated entity, namely (unsituated) `X` at time `t`
2. an expression that designates the state of a situated entity `X` at time `t`
3. some entity derived from or including one or more situated entities; the interpretation in this case arises from a syntactic manipulation of the original expression, which will be defined shortly.

A brief example is in order to motivate this. Suppose we want a timeseries of daily temperatures based on a model represented by a situated type `M`; let's give `M` an attribute `temp:temperature`. (We can define `temperature` as a quantity type, but here we'll assume it's a synonym for `float`.) To get a timeseries, we can create an instance of `M` situated at time `start`, then request a list of samples:

    val M0 = [M:...]@start
    (0...50).{ val aDay = start+this*day; M0.temp@aDay }

This designates a list of modeled temperatures for 50 days starting at the initial state of `M0`. The definition of `aDay` uses built in arithmetic on `time` and `duration` types to get the succession of days.

The example illustrates all three ways of using `X@t`. The line defining `M0` has it as a literal form, turning the unsituated litform for `M` into a situated one by supplying a `time`. (The keyword `start` is a built in `time`-valued literal intended for use in "non-historical" modeling applications.) The second line has `M0.temp` in the role of `X` and `aDay` in the role of `t`. It's clear that `M0.temp:float` is unsituated but "includes" a situated entity (namely `M0:M`). This qualifies under the third form above, as we'll see when we make precise definitions. In fact, it's equivalent to `(M0@aDay).temp` and this `X@t` expression is properly interpreted under rule 2 above, as the state of `M0` at the designated time.

Now let's proceed to a more formal definition of how `X@t` is interpreted. The first case is when `X` is a litform tagged as a situated type. The litform cannot define the `simT` of a situated type, so in this case, `@t` completes the litform. Note: it's immaterial if `X` in this case is interpreted as a call to a `lithook` method. The second case is when `X:S` where `S::situated`. Then `X@t` designates `X` with its state updated to time `t`. This is the payoff for everything to do with situated types and the `@` operator. Note that if `X` is a symbol or attribute reference, this does _not_ mean that that entity is modded. Interpretational details are given shortly but `@` is not a mod operator.

In every other case, `X@t` is either an error, or is interpreted via a _syntax based transform_ which I refer to as a "push down" of `@` into the `X` expression. Informally, this means to rewrite the expression by removing the `@t` from the top level and reproducing it in all the "appropriate" components of the expression. Each new `X@t` sub-expression is then interpreted and the rewrite is retained for those sub-expressions where interpretation succeeds. If no interpretation succeeds, the whole expression is in error.

This transform is syntactic in that sub-expressions are rewritten based on their form; the following table shows how. In this table assume the following type definitions: `L` is `list` or `space`, `T` is a `tuple` (not situated), `S` is a symbol (not a general expression) of unsituated type.

| Form              | Rewrite             | Comments                                                                                                   |
| :---------------- | :------------------ | :--------------------------------------------------------------------------------------------------------- |
| `X.a@t`           | `(X@t).a`           | This refers only to the attribute reference form for dot syntax, not its interpretation as a function call |
|                   |                     |                                                                                                            |
| `f(a0,...ak)@t`   | `f(a0@t,...ak@t)`   | The function is not rewritten, only the arguments                                                          |
| `[L:a0,...,ak]@t` | `[L:a0@t,...,ak@t]` |                                                                                                            |
| `[T:a0,...,ak]@t` | `[T:a0@t,...,ak@t]` |                                                                                                            |
| `S@t`             | `B@t`               | Only if S is declared as `val S = B`, else no interpretation                                               |

Any other expression form has no interpretation and is treated as an error. Expressions like `12@start` are in this category. On the other hand, if `X` is situated, then `f(X.a, 12)@start` has an interpretation, namely

    f((X@start).a, 12)

which arises by first pushing `@start` into both of the function arguments, then pushing it into the attribute reference. The second argument has no (situated) intepretation but the first does, so the entire expression succeeds.

Now we need to describe how case 2 works, i.e., how `X@t` is calculated when `X` is situated. Since `X` is situated, it extends a tuple type so it is stateful, and it has an associated time, accessible by `X.simT`. We'll assume henceforth that `t0` is set to `X.simT`. We require time to go from past to future, so it is an error to evaluate `X@t` if `t0 > t`. Note that a compiler will sometimes be able to tell for a given instance of `X@t` whether this is the case; if so it should declare a compile time error and otherwise it must arrange for a runtime panic if the condition fails.

Evaluating `X@t` when `t0 == t` is trivial: `X@t` designates `X`. Things only get interesting when `t0 < t`. Let's say `X:S` so `S::situated`. Everything depends on `<ontime>` handlers: both delta handlers defined in `S` and event handlers associated with any type for which entities can occur in the query and can designate `X`, according to conservative dataflow analysis. We create a simulated time variable, call it `St`, initialized to `t0`. For each delta handler defined for `S` we create a duration valued state variable, initialized to 0; call these `del0`, `del1`, etc. We note each possibly relevant event ontime handler.

With these initial conditions set, we then "continuously sweep" `St` forward until it reaches `t`. As we do this, we increment each of `del0`, `del1`, etc. to reflect the amount of time since it was last triggered. Delta handlers trigger when their state variable reaches the value of delta in that handler's condition. Similarly, event-based handlers trigger when the following 3 conditions are met:

1. any terms conditioning the `simT` comparison term are true
2. the term calling `simT` designates `X`
3. `St` is equal to the time given in the condition.

Running handlers are mod methods of the situated entity, so triggering them usually changes the state of `X` and these state changes are accumulated as `St` is swept forward. Whenever two or more handlers could trigger "simultaneously" the language does not constrain the order in which they are run, or whether they run concurrently.

The purpose of event handlers is to provide a mechanism allowing user code to define an appropriate notion of events and how they work. The design philosophy here is similar to the type system, where a minimum set of type generators is built in and everything else is user defined, with the standard annotation mechanisms available to affect compilation. A sketch for one concept of event handlers is given in the following code fragment:

    import "olist" melted   // defines ordered lists type generator
    val hasWhen = typepred(_ ::entity, attribute when:time)
    given(eventT::hasWhen) {
      val hasCallback = typepred(S::situated, callback:\mod(S, eventT)->nothing)
      given(S::hasCallback) {
        val baseTuple = tuple(who:S, private events:olist(eventT, time, \(y:eventT) { y.when }))
        val eventlist = extend baseTuple where {
          method lithook = \(...) {...}
          method add = \mod(x:eventT) { events.add(x) }
          ontime events != [] && events.head.when = who.simT \(d:duration) {
            who.callback(events.first)
            events.popf()
          }
        }
      }
    }

Here, type `eventlist` is defined to feed a stream of events to a situated entity held in the `who` attribute, calling its `callback` method when an event occurs. D8m's approach to generic types allows this definition to be used in a single program for a variety of situated types and event types, as long as each situated type has a `callback` method with an argument of the appropriate event type. Other event models can be developed.

Since `@` is not a mod operator, evaluating `X@t`doesn't change the state of `X` even though its state normally changes during the evaluation. D8m's query semantics simplifies but doesn't eliminate the complexities this poses to a compiler. I believe the design just shown for defining and evaluating time-based models is powerful, flexible, and convenient; that it separates better than most the issues of defining models from those of extracting desired information from them. In particular, combined with d8m's query semantics, it allows one to define complex models and then get the compiler to automatically ignore irrelevant complexities when queries are run. For example, querying a full financial model of a business for a cash on hand timeseries should compile code that ignores the depreciation part of the model.

While the simulation model is quite abstract, I believe that in a broadly useful set of cases, it can be compiled into high performance code. However, doing this can be challenging. For example, recall the query for the example we gave earlier:

    (0...50).{ val aDay = start+this*day; M0.temp@aDay }

This corresponds internally to

    maplst(0...50, \(this:integer) { val aDay=...; (M0@aDay).temp} )

which calls for 50 distinct calculations of `M0` in the naive semantics. A naive compilation would be an _O(n^2)_ implementation of an _O(n)_ algorithm, which programmers would not appreciate. It's pretty trivial to optimize this into a single, sequential state update that's sampled 50 times but if the sampling sequence isn't constant and trivially monotone increasing, the optimization is more challenging.

# Meta

A key design premise for d8m is that to get both "abstract statement of problems" and "efficient and scalable compiled code" you need somewhat independent ways to talk about both. For types, the strong version is: **a source type that always translates the same way isn't an abstract type**.

The `STMap` statement is described [below](#stmap); it tells how instances of a (target) type `T` can implement instances of a (source) type `S`. `STMap` is the declarative part of how types can be translated in different ways. The `applyST` [pseudo-function](#applying-stmaps-and-rewrite-rules) is the operational part of this.

Rewrite rules are the other main piece of what d8m brings to this topic, and are covered [here](#rewrite-rules). They are defined as "xprftns" and applied with a `rewrite` pseudo-function. Thematically, rewrite rules are less focused than `STMap`, mainly because they can do so many things.

In sum, there are four statement types that affect compiler behavior via ST mapping and rewriting: `STMap` defines and `applyST` applies; `xprftn` defines and `rewrite` applies. While these are typechecked in the normal way when definitions are made, these statement types behave differently from others when a query occurs. Then their effect, if any, happens entirely at compile time. To define this as well as possible, we need to pay more attention than most programming language definitions to the notion of _manifestness_.

An expression is _manifest_ if a compiler can evaluate it at compile time. In classical compiler writing, what's manifest isn't part of the language specification, though there are conventions. Classically, `12 * 15 + 3` is manifest, so is `true && false`; but these are conventions -- facts about the compiler, not the language. D8m's treats manifestness by convention conventionally, but in addition, it stipulates that _asserted expressions are manifest._ This gives users some control over what the compiler does.

To help a little more, d8m provides a built in pseudo-function

    manifest:\(boolean) -> boolean

which is defined so that `manifest(X)` is true just in case `X` is both true and manifest, else false. So `manifest` is manifest (and will always be evaluated by the compiler as it compiles queries). Clearly, assertions affect the definition of this function. Also, it is logically sound to infer from `manifest(X)` that `manifest(Y)` for any `Y` entailed by `X`. This gives a convenient entry point for an advanced compiler to make inferences about asserted expressions such that it's reasonably easy for users to understand and influence the compiler's behavior.

In fact, `manifest` need not evaluate the expressions it's called on, except to infer logical entailments should the compiler be capable of this. So calls to declared but undefined (i.e. unbound) predicates can have some effect. For example, one can define

    given(F::function) val injective:\(F)->boolean

which says there exists a predicate on functions named `injective`. Code _inside manifest_ can use this undefined function, so

    if((manifest(injective(myFn)) && something) || somethingElse) { ... }

calls either `something` or `somethingElse` depending on whether `myFn` is injective. In a context where `assert injective(myFn)` is active, the first clause will be run, else the second.

A second built in pseudo-function is also manifest:

    defined:\(<identifier> ',' <qualifier>) -> boolean

which is true if its argument is defined. Here, the `<qualifier>` is either a type literal or the word "`type`". The typical use of `defined` is in the `<condition>` of an `oncondition` statement, where the body of the statement contains definitions that require the `<identifier>` to be defined in a way that's consistent with the `<qualifier>`.

In sum, I claim that the "meta" part of d8m provides a framework for very advanced optimization based on source to target type mapping, term rewriting, user control over how these things are applied via assertions, and potentially, logical inference to generate entailments that are known to matter. The following sub-sections illustrate and explain this claim. Of course, turning this framework into a fully working set of tools is a big job, one that is clearly outside the scope of this document.

## Assert

The `assert` statement is simply:

    assert <expression>

where `<expression>` is boolean-valued. It's a statement, so it designates nothing. Its effect, if any, is by changing the bdgenv. Assert statements may occur anywhere: in global context, in type definitions (`extend` expressions), and in functions. In d8m, `assert` is not shorthand for "check and exit"; indeed, `assert` never generates code directly. (Both `oncondition` handlers and standard conditional statements can supply the check and exit functionality.) Certain uses of assert require specific behavior, but it can also be used just for documentation. Before we discuss these we need to say precisely how `assert` is interpreted logically.

Except for _effective assert predicates_, which are covered shortly, each asserted `<expression>` must contain at least one symbol that is either unbound or moddable, and the assertion _quantifies over_ all such symbols. So the following assertions are in error because unquantified:

    assert 2 == 4
    assert 2 == 2 || "abc" == "abc"

Note that the truth of the assertion is immaterial here. Note as well that the symbol quantified over needn't be literally present in the expression if it is `self` or `this`. Indeed, admissible assert statements in `extend` expressions always quantify over an arbitrary instance of the type, which is typically written as an `X` that means `self.X`. That quantification applies to moddable symbols implies that it's over executions -- an assertion like

    val foo = \imp(lst:list(T)...) {
    	var y:list(T) = []
    	each(x^lst) { ...if(P(x)) y.pushb(F(x))... }
    	assert y.count * 2 >= lst.count
    	...
    }

quantifies over `y` and applies at the code point where the assert statement is. (The assertion quantifies over `lst` too but `lst` is constant in the scope; it's `y`'s moddability that limits the extent of the assertion.) From this assertion, you and I can infer (though most compilers cannot) that in the loop, `P(x)` was true at least half the time, causing an element to be added to `y`.

Note that by logic an assertion of the form `assert X && Y` is equivalent to `assert X; assert Y`. This clarifies that every clause of a conjoined assertion must be quantified. Stylistically, the conjoined form is discouraged.

A compiler is allowed to add `assert` statements to a query; this rule provides the formal basis for compiler inference to interact with optimization rules, either built in or user defined. User-supplied rewrite rules can do this as well.

An assertion in a scope holds from the statement after it to the first statement that invalidates it, or the end of the scope. An assertion `assert C` is invalidated by statements of the following forms

1. an assignment whose left side contains a prefix of a symchain in `C`
2. a mod funcall whose receiver contains a prefix of a symchain in `C`
3. any control flow statement one of whose nested scopes has a statement matching these descriptions

Now we're ready to talk about when assertions have a required effect. This is:

1. when they occur in a call to `manifest`
2. when they cause guarding behavior
3. when they use _effective assert predicates_
4. for assertions made in types, when types are matched in applying STMaps and xprftns

We discussed manifestness earlier, and it will come up in later sections as well. With respect to guarding behavior, there are two main differences from conditional statements.
Guarding inferences due to an `assert` statement persist through the end of the scope in which it occurs, or until invalidated by a mod or assignment (see [here](#guarding-behavior)).
And, unlike in an `if` statement, the `<expression>` leading to the guard is not compiled into code, but is simply assumed to be true (since it was asserted to be). For example, some property of an algorithm might ensure that on entry to a loop, some list `lst:list(T)` is non-empty. If we need to extract `lst.head`, we don't want to have to write a conditional to check `lst`. Instead, we can write

    loop {
       assert lst != []
       val foo = lst.head
       ...
    }

and `foo` will infer to `T` rather than `nilPossible(T)`. (Note that if the assertion is not true, the compiled code will panic at runtime.)

The _effective assert predicates_ are a particularly operational use of `assert`. They are discussed in the next section. Further discussion of assertions and `manifest` is in the following section.

### Effective Assert Predicates

The built in functions listed in the following table may only occur _by themselves_ in `assert` statements.
Each takes a single argument that in many cases must be a symbol. Some of them are only allowed in certain contexts.
They are formally predicates but they have a defined operational effect and are best understood as compiler directives that are invoked via `assert`. All of them automatically satisfy (or if you prefer, bypass) the quantification requirement.

| predicate      | notes                                                                    |
| :------------- | :----------------------------------------------------------------------- |
| `enumerable`   | Occurs in `extend tuple(...)` body; argument names one of its attributes |
| `rcvrLinked`   | Occurs in function literal; argument names one of its formal arguments   |
| `purified`     | Argument names a mod function that returns something                     |
| `noInline`     | Argument names a function                                                |
| `alwaysInline` | Argument names a function                                                |
| `connected`    | Argument is a type                                                       |
| `Gomethod`     | Argument is a method of the type                                         |

#### enumerable

Allowed only in an `extend` statement whose base type is a `tuple` type.
Asserting it tells the compiler to enumerate instances of the type by enumerating the named attribute (which should be enumerable).
It is customary, but not required, for this attribute to be `private`.
A canonical example is a set type whose contents are held in a list.

#### rcvrLinked

Allowed only in the body of a `mod` function literal; argument must not be the receiver.
Asserting it tells the compiler that the named argument is "part of" the receiver and so, may be modified.
This is useful for mod methods on linked structures involving some "focus" element in addition to the "root".

#### purified

Allowed anywhere. Asserting it tells the compiler to treat the named `mod` function as (nearly) `pure`.
Intended for PRNG's, logging functions, and the like.
Purified functions can be called in pure functions, but may not be eliminated like true pure functions.

#### noInline

Allowed anywhere. Asserting it tells the compiler not to inline the named function.

#### alwaysInline

Allowed anywhere. Asserting it tells the compiler to inline the named function even if it thinks the function is too big. Note that some restrictions on inlining cannot be bypassed, so this directive doesn't guarantee that a function will be inlined.

#### connected

Allowed anywhere. Asserting it makes the named type connected. See [here](#the-outside-world).

#### Gomethod

Allowed in types. Asserting it causes the compiler to emit the named method as a method of the go version of the type. (Note that the compiler is allowed to emit whichever methods it chooses as go methods; the effect of `Gomethod` is to _require_ the named method to be emitted as such.)

## Applying STMaps and Rewrite Rules

Definitions of STMap pairs and rewrite rules are at least somewhat declarative. Applying them is all about compiler behavior -- about how queries/compilations are to be run. Declarations should be part of the infrastructure of a well developed d8m ecosystem. Applying particular STMaps or rewrite rules is mostly part of developing a particular query or program. (Some rewrite rules are part of general purpose compiler optimization, and some STMappings might be always desirable. These would be applied globally.) Since it can be helpful to understand how these things are applied before we get into the details of defining them, we cover that topic here. Built in pseudo-functions `applyST` and `rewrite` apply these transformation options. Since both of these affect how a compilation proceeds, they and their arguments are evaluated at compile time. We'll talk more about what this means after we describe the functions themselves.

`ApplyST` is a nothing-valued "function" of two arguments: a type and either a symchain or a predicate. The type should be the target type of some `STMap` pair. If the second argument is a symchain, then the mapping is applied to the entity it designates. So

    applyST(T0, x.y)

means to map the `y` attribute of `x` to type `T0`. It's an error to make this statement unless there is an STMap pair in the environment that maps `S0` to `T0` where `S0` is the type of `x.y`.

If the second argument is a predicate, it must take a single argument. It means to map every entity that matches the predicate to the target type. The body of the predicate is implicitly wrapped in a call to `manifest`; this is equivalent to saying the predicate is evaluated at compile time (on every entity of its argument type) and the standard rules for `manifest` apply. For example,

    applyST(T0, \(lst:list(S0)) { lst.count > 5000 })

means that every `list(S0)` occurring in the query whose length is manifestly greater than 5000 will be mapped to type `T0`.

The sense in which source types match, for purposes of `applyST`, is different from typechecking. For typechecking, we have the computable notion of type equality, whose inverse we call "TC-distinct". This uses only identifiers and signatures to compare methods, and ignores certain aspects of types, assertions in particular. For `applyST`, the same notion of equality is used except that in addition, assertions in the source type of the `STMap` statement must be satisfied (i.e. manifestly true) of the query in order for it to match.

When considering the totality of `applyST` statements in effect at any given point in a query, our intuition is that they should form a partial function on the entities in the query. In other words, at most one `applyST` statement applies to any given entity, either by being named (symchain based) or via its type (predicate based). Since it's not really possible to enforce this condition, we don't impose it. However, we do require the compiler to prioritize symchain based rules over predicate based ones. Otherwise, when multiple `applyST` rules apply, the compiler should pick one arbitrarily.

`Rewrite` is a pseudo-function which applies a rule unconditionally. With a single argument, it directs the compiler to look for and rewrite parts of the query within the scope in which it applies. This argument takes the form either of a "call" to a rewrite rule (with manifest arguments) or, for rewrite rules with no arguments, of just its name. In this form, `rewrite` is nothing-valued. If given a second argument, the rule in the first argument applies only to the expression in the second, and the rewrite expression infers to the type of the second argument. This "direct form" can be used to control application of rewrite rules quite precisely, at the expense of potentially writing a lot of very specific code.

For example, in

    val extractionSortRW = \xprftn() { ... }
    val mySortedList = rewrite(extractionSortRW, myList.sort(myfn))
    rewrite(extractionSortRW)

both calls to `rewrite` apply an xprftn named `extractionSortRW`; the first applies it to the expression `myList.sort(myfn)`, binding the resulting expression to `mySortedList`; the second applies it unconditionally until the end of the current scope. In the direct form, there's no requirement that the second argument matches the rewrite rule, although it would be odd if it didn't.

The fact that `applyST` and `rewrite` are evaluated at compile time has several implications. Some of these have already been discussed, specifically, those related to evaluating the arguments to the functions. A subtler implication concerns when the calls to these functions are active. There are two rules about this. The first is scope-related: since these calls normally affect compiler behavior within their scope, the compiler must "interpret" this aspect of execution (by adding and deleting rules) as it traverses the reachable parts of a query. The second is more subtle: an application in the body of an if statement should be applied _in the scope of the statement_ if the condition is manifestly true. For example, consider a code slightly different from the `applyST` example given earlier:

    if(lst.count > 5000) applyST(T0, lst)

According to the first rule, the `applyST` should be active throughout the scope of the then part of the if statement, but there's nothing else in that scope. The second rule says that if `lst.count` is manifest (or more precisely, if `lst.count > 5000` is manifest), then the `applyST` should be active from this point on in the current scope. In effect, if we can manifestly evaluate conditionals containing `applyST` and `rewrite` calls, we should do so. The meaning differs from the example given earlier in that it applies a length threshold only a particular list rather than to lists in general.

A good way to think about both of these rules is that they make specific requirements on the "symbolic evaluation" a compiler must do on code that uses `rewrite` and `applyST`. Namely, it must add them to active use as they are encountered and delete them at scope exit, and it must symbolically evaluate manifest conditionals containing calls to `rewrite` and `applyST`, somewhat like the "guarding rules" on ortypes. Of course, the manifestness rule for assertions can affect the manifestness of conditionals like the one above.

## Back End Hooks for Go Features

Go has a number of features that d8m does not. D8m provides access to some of these via STMapping and/or rewrite rules, in order to increase the opportunities for optimization. Specifically, d8m provides `Gomap` for STMapping, and `Gochannel` and `Gocall` for rewrite rule results, to create target code respectively involving the `map` type, the `chan` type, and goroutines.

The type generator

    val Gomap = \(K, V::entity) { ... }

is built in but restricted to be used in just two ways:

1. as the type argument in `applyST`
2. as the target type in `STMap`

The upshot of these restrictions is that `Gomap` cannot be used as a source level type but only as the target of `STMap`.
Entities mapped to `Gomap` will appear in generated code as `map` types.
Go's `map` type cannot handle any entity as the key; it is the compiler's responsibility to ensure that d8m semantics is maintained or else that an error is declared. (For example, list types cannot be the key of `Gomap` unless the compiler can prove that no list occurring as a key is modded in the lifetime of the map. If it can so prove, it can implement such a map with pointer to slice but otherwise, it must declare an error.)

The following tables lists the methods of this type generator. As shown in `lithook`, the general idea is that it corresponds to the d8m type `list(tuple(key:K, value:V))`. In the table, `M` is defined as `Gomap(K, V)`:

| method        | signature                                   |
| ------------- | ------------------------------------------- |
| `rvalindex`   | `\(M, K) -> nilPossible(V)`                 |
| `rvalindexWD` | `\(M, K, V) -> V`                           |
| `lvalindex`   | `\(M, K, V) -> nothing`                     |
| `delete`      | `\mod(M, K) -> nothing`                     |
| `count`       | `\(M) -> integer`                           |
| `lithook`     | `\mod(M, list(tuple(key:K, value:V))) -> M` |
| `init2const`  | `\mod(M, list(K), V) -> M`                  |
| `keys`        | `\(M) -> list(K)`                           |
| `values`      | `\(M) -> list(V)`                           |

As usual, `rvalindex` and `lvalindex` are normally written in square bracket form. The `rvalindexWD` method (`WD` stands for "with default") provides for index to return a value in all cases, but has no special syntax. Note that in general, `rvalindex` must generate the "multiple assignment" code form in go that detects presence of a key in order to conform to standard d8m semantics. A good compiler will recognize that unchecked go code is possible when `rvalindexWD` is called with the third argument bound to go's zero value for type `V`.

A `Gomap` is enumerable and its `elttype` is `tuple(key:K, value:V)`. This means that `Gomap` can be used to translate source level entities that are enumerated. The `keys` or `values` methods allow enumerating just one or the other.

`Gocall` is a built in pseudo-function on a single argument that must be a function call and can designate anything (including `nothing`). Go channels are accessed through built in functions that correspond to send and receive operations, along with a way to declare them. The built in functions on channels are

    given(T::entity) {
      val channelReceive:\(Gochannel(T))->T
      val channelSend:\(Gochannel(T), T)->nothing
    }
    val channelMake:\(T::entity, dir:label, bufferDepth:integer)->Gochannel(T)

A call to `channelReceive(C)` will produce go code like

    <- C

Since `channelReceive` is connected, its return value may be ignored. Similarly, `channelSend(C, X)` produces statements like

    C <- X

Note that `chanelMake` is a pseudo-function that takes a type since in general, there's no way to infer the channel type without it. Create a `Gochannel` with `chanelMake`; the `dir` argument shoud be one of the labels, `:both`, `:send`, `:receive`, set the `bufferDepth` to a positive integer for a buffered channel; any other value creates an unbuffered channel.

## STMap

The compiler's state (bdgenv) includes a set of _ST pairs_; each pair consists of source and target types, and translation rules. An ST pair `[S`, `T]` asserts that rewriting expressions involving entities of type `S` into type `T` using the rules preserves the semantics of programs. Each ST pair is defined by an `STMap` statement. Since `STMap` statements can occur in the body of `given` statements, they can tell how to translate type generators as well as types. An `STMap` statement affects the scope in which it occurs, so the set of ST pairs expands and contracts as scopes are entered and exited, and modules imported. This section describes the syntax of `STMap` statements and how expression rewriting works.

Informally, we consider `S` the source type and `T` the target type, and we think of `S` as more "abstract" or "higher level" than `T`. However, both are d8m types. (A minor exception is `Gomap`, which is described elsewhere and is only allowed as a target.) Given pairs `[S, T0]` and `[T0, T1]` it's clear that we can write down rules for defining `[S, T1]`. Mathematically, this says the relation induced by the set of `[S, T]` pairs is transitive. In fact, it's almost an equivalence relation, but not quite, because it's not always possible to run the translation rules backwards.

At any rate, we almost always organize ST mapping in the "forward" direction, from `S` to `T`, possibly with composition. The one exception is back casting, for which see [here](#built-in-pseudo-functions).

The syntax of the `STMap` statement is as follows

    <STMstmt>		::= 'STMap' '{' <stsrc> <sttgt> <directstmt>? xltrule* '}'
    <stsrc>		  ::= 'source' '=' <typexpr>
    <sttgt>		  ::= 'target' '=' <typexpr>
    <directstmt> 	::= 'direct' ( <symbol> | <bdgpt> ) ( ',' ( <symbol> | <bdgpt> ) )*
    <xltrule>		::= <expr> '=>' <expr>

An `STMap` statement will generally occur inside a `given` statement, both for the common case that source and target types are generic, and to define generic entity symbols that are often needed in the rewrite rules. The body of the `STMap` statement starts with statements that define the source and target types; the remaining statements define translation rules, which we'll call `<xltrule>`'s or just _xltrules_ henceforth. The `direct` statement applies to the simplest possible case, when a method named `M` exists for both source and target types, has the same argument types (other than the receiver), and is the correct translation (roughly: "means the same thing"). If such methods exist, the `direct` statement gives a list of them, possibly qualified to disambiguate multibound symbols. At most one `direct` statement is allowed.

Otherwise, the `<xltrule>` form is used; we'll schematicize this as `<lhs> => <rhs>`. Quite naturally, the `<lhs>` matches source type expressions while the `<rhs>` matches target type expressions. However, the rules for building `<lhs>` and `<rhs>` expressions are not symmetric. Both use `<self>` to designate the thing being translated, but the `<rhs>` of rules are arbitrary terms (containing `<self>`) while the `<lhs>` are limited to attribute references and method calls on `self`, plus a special expression for enumerations. More formally, the allowed expressions on `<lhs>` are

1. `self`
2. `f(self,...)` for `f` any method of the type
3. `self.a` for any attribute `a` of the type
4. `enumerable(self)`

In the direct statement and in xltrules, `source` and `target` designate the corresponding types.

While the `<rhs>` can be an arbitrary expression, its inferred type must either match that of the `<lhs>` or, if the `<lhs>` infers to `S` the `<rhs>` should infer to `T`. For `enumerable` rules, the `<rhs>` must infer to `list(E)` where `E` is the elttype of `S`. (This is well defined because elttype is defined for any enumerable type.) Also, if the `<lhs>` infers to `nothing`, the `<rhs>` may be something else; in which case its value will be ignored when translated and back casting of this STItem is not supported.

The essential move in applying a `[S, T]` pair is identifying the subterms of the query that match a `<lhs>` and rewriting them to the corresponding `<rhs`. Methods arguments other than the receiver are matched as in rewrite rules, with generic entity symbols (defined in a `given` statement that wraps the `STMap` statement) serving as matchables.

For example, consider a mapping between two collection types defined by type generators `S` and `T` that both have a method to add an element to the collection, but imagine that these methods have different names and calling patterns. To write the `<xltrule>`, we need a generic instance of the element to add. The `STMap` statement might look like this

    given(ET::entity, anElt:ET) {
      STMap {
        source = S(ET)
        target = T(ET)
        self.add(anElt) => self.insert(anElt, -1)
          ...
      }
    }

Generic entity symbols defined in an enclosing `given` statement can also occur in the target or source type specifications. For example, one can define a type generator `C` with a parameter for some (fixed size) underlying storage structure, then translate from a more abstract type `A` with an `STMap` statement like this

    given(T::entity, mxSize:integer) {
      STMap {
        source = A(T)
        target = C(T, mxSize)
        ...
      }
    }

In this case, `mxSize` gets set by the [`applyST`](#applying-stmaps-and-rewrite-rules) statement.

We're ready to state the rules for applying a `[S, T]` pair to a query `Q`. Conceptually, we handle symbol chains (symbols and attribute references) first, then everything else. For symbol chains, we check the active `applyST` statements to see whether the expression is translated or not. If so, a `T`-valued symbol or attribute is created to replace the `S`-based one, and its binding is translated to `T` (by applying these rules recursively). Each translated part, `X`, induces further translation steps when it occurs in the following ways in the query:

1. `X` is in an attribute reference or method call
2. `X` is the left side of an assignment statement
3. `X` is the return value of a function
4. `X` is the entity enumerated by an `each` statement or a function argument that typechecks by virtue of the enumonly rule

Case 1 is by far the most common. Attempt to match each `<xltrule>` to a part of the query with `X` matching the occurrence of `self` in the `<lhs>`. If there is a match, note the type `R` of the matched expression. Substitute the corresponding `<rhs>` for the matching expression, with the `T`-valued version of `X` substituted for `self` therein.
If there is no such rule, and the query match is to an attribute reference, translation fails.
If the query match has `X` in a call to method `F`, attempt to translate `F` (using the rules defined here) and replace it in the current function call.
In the special case that `F` is defined as generic on the translated argument position, the above rule is equivalent to instantiating the generic `F` with type `T`; this always works if `T` matches the type predicate associated with the generic argument.
Otherwise, if `F` fails to translate, the entire query translation fails. Note that methods of built in types can never be translated.
Repeat each translation step on the resulting expression as long as `R` was equal to `S`. (In that case, it will have become `T`, otherwise it must be unchanged by the translation, so no further translation is needed on this sub-expression.)

In case 2, replace the assignment statement with the `T` version of `X` on the left and the right side translated to `T`. In case 3, we are in a function literal, call it `F`; its return type changes from `S` to `T`. Rewrite `F` and if it is bound to a symbol, mark each point where it is called as requiring translation.

Case 4, is covered by the `<xltrule>` whose left side is `enumerable(self)`, if there is one.
If there is not, attempts to translate enumerations of `S` will be treated as errors. Note that every
enumerable type has an inferable elttype; the elttypes of `source` and `target` must be the same. That is,
the type checker should declare an error if this condition is not met.

The compiler treats translation failure as an error; this situation can be thought of as caused by a STMapping that is _partial_ being applied outside its domain. For example, `T` does not provide some essential method `M` of `S` and for a given query, some `S` that's required to be translated uses method `M`.

As noted briefly in [types](#structural-aspects), concrete source types containing ELTs will match `integer` in the `source` statement of STMap items. Thus, for any type generator `TG`, a rule with `source = TG(integer)` matches type `TG(anELT)` where `anELT` is some enumerated label type. Furthermore, the integer type inferred from `anELT` will have assertions documenting its minimum and maximum values, with the minimum set to 0 and the maximum set to one less than the number of enumerated elements.

Another special case is when type `list` (or a type derived from it) translates to a tuple type. Then we need a way to translate list literals to some lithook form of the target type. Accordingly, we treat `list(T)` as having a `lithook` method of the form

    method lithook = \mod(list(T)) { ... }

only for the purpose of stating xltrules for this case.

An earlier section described d8m's special support for `Gomap`. Here, we give an example of using it to implement sets. Assume

    val set = \(T::entity) { extend tuple(private x:list(T)) where { ... } }

where the methods include `$in`, `add`, `remove`, `count`, `subset`, and so on.

The `STMap` statement should look something like the following:

    given(T::entity, aT:T, ...) {
      STMap {
        source = set(T)
        target = Gomap(T, boolean)
        direct count
        enumerable(self) => self.keys
        $in(self, aT) => rvalindexWD(self, aT, false)
        add(self, aT) => lvalindex(self, aT, true)
        remove(self, aT) => delete(self, aT)
      }
    }

I've left `subset` untranslated here, possibly as an illustration of something untranslatable but more likely because set's `subset` method is defined in terms of other set methods and will therefore be translated by rewriting the method (automatically). You may notice the use of the "with default" version of `rvalindex` for lookup, since `Gomap`'s `rvalindex` method is nilPossible, while `set`'s `$in` method is not.

Once this rule is defined, you can apply it to `SS:set(account)` with

    applyST(Gomap(account, boolean), SS)

## Rewrite Rules

Term rewriting is a good way to express program transformations, and d8m's rewrite rules are focused on this task. Since any term rewriting scheme worth the name is Turing-complete, d8m's scheme isn't compatible with guaranteeing that a compiler will be well behaved. There are straightforward engineering techniques to deal with these issues, such as limiting the number of times a rule can be applied. In addition to issues like infinite looping, rewrite rules can produce code that fails to typecheck, or fails other internal consistency requirements. This mainly affects the process of debugging rewrite rules, since code that does pass the usual compiler checks is safe.

Term rewriting necessarily involves talking about _terms_, whereas coding normally involves using terms but not talking explicitly about them. The term rewriting analogy for variables involves _patterns_ that include symbols that match terms based on type, syntactic pattern, etc. D8m already has various notions that resemble this; the potential for confusion is real. Thus, the _generic symbols_ defined in `given` statements and typegen literals are also distinct from standard programming language variables, but aren't what we need for term rewriting. Besides, we need generic symbols _in addition to_ pattern matching symbols in order to create rewrite rules that are generic. Consequently, rewrite rules get their pattern matching power through another kind of symbol, which we call a _matchable_. Matchables are declared in rewrite rules with `val` statements having particular forms.

You define rewrite rules similarly to functions; various "generalized types" and rewrite-specific functions are added, and some limitations are imposed relative to "full" d8m. But to a first approximation, the rewrite rule engine interprets a large subset of d8m at compile time. These definitions use the function literal syntax with `xprftn` as the "purity word" to distinguish them from standard functions. So

    \xprftn(...) { ... }

defines a rewrite rule, which must be bound to a symbol. (Anonymous rewrite rules aren't allowed.) The identifier "xprftn" is intended as shorthand for "expression function" since rewrite rules produce d8m expressions. The following rules define restrictions on full d8m in the rewrite rule engine:

1. a return type is not allowed in the signature part of a function literal tagged with `xprftn`
2. the following types can occur in the result but are not interpreted: quantity types, `space(...)`, `situated(...)`, `time`
3. built in methods of other types are interpreted, except for certain mod methods of list types
4. `var` declarations and assignment follow `imp` rules
5. declarations can use certain "generalized types", defined below
6. xprftns may not call user defined functions (or xprftns), nor may they define or use function literals
7. the `case` control flow expressions are not allowed
8. each xprftn must contain exactly one call to the `matchquery` function, as the first statement after zero or more declarations; this defines the enabling pattern and optionally, some auxiliary conditions
9. an `xprftn` may additionally call the `skipIf` support function which defines additional conditions on applying the rule
10. the final statement gives the "return value", which must be a term

Most `xprftns` will not have formal arguments; if present, they condition the application of the rule in some way that must be manifest.

Rewrite rules are applied as part of the "back end" transformation process that takes a source level query and produces an equivalent low level program that is translated to go. Rewrite rules are applied in a "iterate to fixpoint" process within a larger iterate to fixpoint process that includes optimizations like function inlining and substitutions between `val` symbols and their bindings. Thus, every phase of applying rewrite rules involves matching every active rewrite rule against every part of the query, until no rule applies. When a rewrite rule matches, a second level of filtering is available via the `skipIf` function, discussed below.

This section contains a discursive introduction to the "language" of xprftns, intended to convey design motives and how to use them. Detailed lists of the xpr related functions and other formal aspects of xprftns are in an [appendix](#xprftn-details). The following section gives examples of rewrite rules in use.

The "generalized types" mentioned in rule 5 are described in the next table

| generalized type | matches                                                              |
| ---------------- | -------------------------------------------------------------------- |
| `xprterm`        | any term                                                             |
| `xprterms`       | a list of terms (comma separated in source)                          |
| `xprsym`         | any symbol                                                           |
| `xprftn`         | any function-valued term                                             |
| `xprptn`         | a pattern                                                            |
| `xprstmts`       | a list of statements (separated in source by semicolons or newlines) |
| `matchinfo`      | only occurs as the result of the `matches` function                  |
| `type`           | any type                                                             |

The generalized types allow one to express certain constraints that the ordinary type system does not, most notably the distinction between symbols, statements, and terms. Since symbols and statements are also terms, we define `xprterm` as a supertype of the others. Thus, a function requiring a `xprterm` argument may be supplied with a `xprsym` and so on. The types `xprterms` and `xprstmts` are used only in "flex" matchables that bind to lists of expressions or statements of whatever length causes the larger pattern to succeed, including zero length.

So an `xprftn` has the syntactic form of an `imp` function, with the limitations and generalizations listed above. There's a characteristic flow to `xprftn`'s that comes from their role as rewrite rules: declare some matchables, call `matchquery` to define what this rule matches, gather information about the matched term that's needed for secondary filtering, do secondary checking with `skipIf` as needed, make definitions needed in the output term, and finally, create the output term, which is the return value.

We define matchables and patterns, then consider what questions we'll ask with these tools.
Matchables take two forms, which I call "type enabled matchable" or _TEM_, and "pattern enabled matchable" or _PEM_.
A type enabled matchable is just a symbol declared with `val` and without a binding.
Often, it has a generalized type, though this is not required.
For example, `val m:xprterm` declares `m` as a matchable matching any term, while `val m:integer` makes `m` match
terms with type `integer`.
A pattern enabled matchable is a type enabled matchable whose binding calls the `matching` function,
whose success becomes a condition of matching. We'll see examples after we discuss how patterns are applied.
Essentially, TEMs match based on types while PEMs match on patterns.

Patterns are best understood as terms built in the normal ways, but also allowing matchables as symbols.
There is a limited form of disjunctive pattern, which can be understood as a list of terms.
Given that patterns _consist of_ terms and are _matched against_ terms, we need to carefully identify the role of terms in matching.
We'll usually say "pattern terms" for the former and "ground" or "target" terms for the latter.

Matchables have _state_; specifically, they can be _bound_ or _unbound_. Matchables are unbound when declared and can become bound when they occur in a pattern that's matched.
We say "can become bound" because some of the pattern matching functions don't preserve bindings; details are given shortly.
Also, the identifier "\_" is reserved for a matchable of type `xprterm` that's never bound.
This matchable may not be declared; when used in a pattern, it matches any term and is then forgotten, even if used in a pattern matching function that normally binds matchables.
When a matchable is bound, it gets the value of the subterm at the position in the ground term corresponding to its position in the pattern term.
When a pattern is matched successfully using a function that preserves bindings, every unbound matchable in it becomes bound.
Previously bound matchables act as constraints on matching, allowing a match only when the corresponding ground term equals the bound term.
The equality used here is that of terms.

Generally speaking, the questions we want to ask about patterns (`P`) and terms (`T`) are

    1. does P match on T
    2. does P match on any subterm of T
    3. on how many subterms of T does P match
    4. give me a list of subterms of T for which P matches

In addition, there are different ways to apply pattern matching -- for PEMs, while generating a rewritten term, and deciding if an xprftn matches. To signal PEMs, we use a function called `matching`. In the rule body, we use functions named `match`, `contains`, `matchcount`, and `matches` to answer the 4 questions just listed. Finally, the `matchquery` function always asks the same question as `match`, and the compiler ensures that this pattern is attempted on every possible term.

The `matching` function gets a pattern and two boolean arguments which control (1) whether matching
must match the given pattern or searches for all subterms; (2) whether bindings of matchables in the
pattern are retained on success. We can think of `matching` as having the following signature (though it's not really
a function, as noted below):

    val matching = \(pattern:xprptn, matchAnywhere: boolean, retainBindings: boolean) -> boolean { ... }

For example,

    val mbl:xprterm = matching(P1, true, false)

defines a symbol `mbl` that's a PEM (because bound to a call to `matching`). It matches a term of any type as long as it or some subterm matches pattern `P1`. Bindings of matchables in `P1` are not retained. If we had written instead

    val mbl:integer = matching(P1, false, true)

then `mbl` matches only `integer`-valued terms that also match `P1` directly and do retain the bindings of matchables in `P1`.
Unlike any other "function call" the value of `matching` is not relevant and in fact, not defined. It is simply syntax that creates a PEM.

Next, let's consider the five matching functions that you can call in the body of xprftn's. Here are their signatures:

    matchquery:\(xprptn)->nothing
    match:\(xprptn, xprterm)->boolean
    contains:\(xprptn, xprterm, ...)->boolean
    matchcount:\(xprptn, xprterm, ...)->integer
    matches:\(xprptn, xprterm, ...)->list(matchinfo)

The first three of these bind matchables, while the last two do not: `matchcount` simply forgets bindings after it succeeds,
while `matches` creates `matchinfo` entities which contain binding information, as detailed below. The `...` in the
last three function signatures indicates the option for multiple xprterms; the combining operator is `||` for `contains`, `+` for `matchcount`,
concatenation for `matches`.
The function `match` requires its second argument to match the pattern; all the others check subterms of the given term(s). Note that `contains` stops as soon as it finds a match; others examine all subterms.
Disjunctive patterns are not allowed in `matchquery`, and are not useful in `matches` (since such patterns may not bind matchables).
Thus, their use is restricted to the other matching functions.

The `matchinfo` entities returned by `matches` act like tuples with the following affordances: `term` designates
the matched term, and for each unbound matchable `M` in the pattern, `M` designates the binding of that matchable.

For example, given the following definitions

    val What, Amt: xprterm
    val incrs = matches("incr(What, Amt", S0))

the rewrite rule interpreter will bind `incrs` to the set of calls to binary functions named `incr` anywhere in term `S0`.
Within the rule body, `incrs` acts like a list of tuples with three attributes: `term`, `What`, `Amt`. Thus, one can
write expressions like

    val whats = incrs.{ this.What }

to extract information about the matched terms.

The argument to `matchquery` has a convenience option: it may consist of clauses joined with `&&` where the first is a pattern and the ones following are boolean. This is syntactic sugar for the matchquery with just the pattern followed by a `skipIf` statement on the negation of the conjoined clauses. It can be thought of as "encouraging" the compiler to start running the rule only if the conjoined clauses are satisfied along with the pattern. Symbols appearing in these extra clauses must be parameters (formal arguments) to the `xprftn` or matchables bound in the `matchquery` pattern. We'll see this feature used in the examples in the next section.

Patterns can be constructed from manifest strings, lists, and the term construction functions (like `mkterm`) that are available in `xprftn`'s.
The list form creates a disjunctive pattern, which may not include matchables other than "\_".
Thus, disjunction is most useful for extending patterns to sets of ground terms.
A litform being used as a pattern can contain a mix of string literals and term construction terms.
String literals are parsed and typechecked in the local scope, so matchables are interpreted as such.
In matching functions where a pattern is required, a string literal will be parsed into a term automatically.
In other cases, where a term is most conveniently written as a string, you can explicitly call `parseRW`.
Strings can't express certain patterns, including many that involve declarations.
When they work, they're usually easier to read than constructed patterns.
The fact that string literals are interpreted as patterns implies that you cannot create a pattern that matches a string literal.

Usually, patterns are best constructed at the point of use, as the first argument to some pattern matching function. In this case, a pattern constructor can be used directly. If more emphasis is desired, the pseudo-function `pattern` can be used to disambiguate. Thus, the two expressions on the following lines are equivalent:

    contains("if(X) Y")
    contains(pattern(mkterm(:if, X, Y)))

assuming `X` and `Y` are previously declared matchables.

Occasionally, it can be helpful to name a pattern. The symbol will have the (generalized) type `xprptn` and will be accepted where a pattern is expected. The only situation where the `pattern` pseudo-function is required is to define a pattern valued symbol without specifying its type, as in

    val ptn = pattern("X(Z)")

The following contrived example shows some of the features defined so far:

    val S0, S1, EB:xprstmts, IV:xprsym, L0:list(integer)
    val EP:xprterm = matching("each(IV^L0) EB", false, true)
    val ptn = pattern(mkstmts([S0, EP, S1]))

The three lines define 6 matchables and a pattern symbol; one of the matchables (`EP`) is a PEM and the others are TEMs. The pattern named `ptn` matches any statement list containing an `each` statement that iterates over a `list(integer)`. If called with `match` or `contains`, it binds all 6 matchables to the corresponding part of the matched term. Note that either or both of `S0` and `S1` can be bound to empty statement lists in this case. Since the search parameter to the `matching` call for `EP` is false, `ptn` only matches terms with the `each` statement directly in the statement list. If that argument had been true, then statement lists with `each` statements in nested scopes would also match.

The ground terms that are matched have an internal form derived from d8m source code. The accessor and constructor functions used in examples and defined in the [appendix](#xprftn-details) are most conveniently implemented in a compiler whose internal representation is essentially identical. In other words, a different compiler might favor a different set of `xprftn` functions which would require changing the rewrite rule infrastructure. That's not an issue I feel needs special attention just now. A more important practical issue is that writing `xprftn`'s requires understanding aspects of the internal representation, such as how dotbrace is rewritten. These aspects are presented throughout the spec as they occur.

### RewriteStore

Occasionally, it can be helpful to have multiple rewrite rules cooperating with each other. Doing this requires a means for rewrite rules to find out what others have done. To support this requirement, we define a special scope named `rewriteStore` to let rewrite rules define _global variables_ so that a set of rewrite rules can memoize things they've done, or cooperate in other ways. The `rewriteStore` is empty when a query compilation starts and changes accumulate until that compilation finishes. Rewrite rules may declare symbols in the rewriteStore and may get and set symbols that have been previously declared. The built in functions for this are straightforward:

    val rewriteStoreDeclare:\(xprsym, xprtype, xprterm) -> nothing
    val rewriteStoreGet:\(xprterm) -> xprterm
    val rewriteStoreSet:\mod(xprterm, xprterm) -> nothing

The arguments to `rewriteStoreDeclare` are the symbol identifier, type, and binding.
The second argument of `rewriteStoreSet` is optional. If two arguments are present, the function acts like an assignment; if one argument that argument should be a function call to a mod function.

Rewrite rules that use the rewriteStore should declare all of the symbols it uses before its `matchQuery` statement. Uses of `rewriteStoreSet` and `rewriteStoreGet` occur after that statement. When the rewrite rule interpreter encounters a `rewriteStoreDeclare` statement, it checks whether the declared symbol is already present and ignores the statement if so. Therefore, a symbol is defined in the rewriteStore when the first rule that uses it
matches, subsequent matches of this or other rewrite rules cause no change due to `rewriteStoreDeclare` statements.

Normally, rewrite rules transform matching expressions, causing source to source transforms on the internal form of the source code as it moves from the d8m level of abstraction to a lower level. Occasionally, and especially when memoizing is needed, there can also be a need for rewrite rules to create auxiliary definitions. D8m supports this need with a built in `defineGlobal` function whose signature is

    val defineGlobal:\(xprsym)->nothing

For the time being, you can only define globals. Rewrite rules must arrange for these to avoid conflicting with existing global definitions. These definitions may use the otherwise forbidden pattern of two initial underscore characters, which is used internally in the compiler.

### Manifestness

When working with rewrite rules, you write d8m that interprets d8m, so it's easy to get confused about what gets executed versus what gets generated, and hence, what needs to be manifest. The rule is that you must construct output terms, using constructors like `mkterm`, `mkstmts`, `parseRW`, and the like. Those terms get assigned to variables local to the rewrite rule and are not executed in the interpreter. All the code that's "directly" in the rule runs in the interpreter, and all arguments to such code must be manifest.

### Rewrite Rule Examples

Consider the following problem: given a list of tuples `biglist:list(T)` where `T::tuple`, and an ordering `fn:\(T,T)->boolean`, find the smallest `K` elements according to `fn`. The abstract problem statement in d8m is clear and simple:

    biglist.sort(fn).[index < K]

In words: sort `biglist` according to `fn` and take the first `K` elements of the result.

Now suppose `biglist` is very big, `T` has many attributes, and `fn` compares a single (ordered) attribute of `T`. Then, it might be advantageous to do the following instead: extract from `biglist` the attribute value needed to compute `fn` and sort that list while remembering the indices in the original list. Then, the remembered indices of the first `K` elements of the extracted and sorted list are the desired indices in the original list. This is faster because you're moving less memory around and/or using cache more effectively; in case `biglist` doesn't fit in memory and the extracted list does, the improvement can be even larger.

In code, assuming `fn` uses just the `a1` attribute of `T`:

    val xT = tuple(a1:T1, inx:integer)
    val xfn = \(a, b: xT) { ...code derived from fn... }
    biglist.{ [xT: a1, index] }.sort(xfn).[index < K].{ biglist[inx] }

The four elements of this chain extract a `list(xT)` from `biglist`, sort it on `a1`, select the first `K` elements, and use the `inx` attribute of those elements (which are the indices of the corresponding elements in `biglist`) to get the selected elements of `biglist`.

We're going to show how to define a rewrite rule to do this transformation, how to solve certain problems that come up, and how to write some simple generalizations of the solution. In the previous section we noted the customary structure of rewrite rules; this example follows that structure.

Everything in the example code below is generic on `T`, the element type of `biglist`, and on `OT`, the ordered type that's compared in the sorting function we'll derive. In other words, all of the code below will be enclosed in a `given` statement of the form

    given(T::entity, OT::ordered) { ... }

First, the xprftn definition and matchquery is

    val extractAndSort = \xprftn() {
      val biglist: list(T), fn:\(T,T)->boolean, K:integer
      matchquery("sort(biglist, fn).[index < K]")
      ...
    }

The pattern is essentially identical to the code we wrote above. The line above it declares the necessary matchables. The pattern doesn't establish that the `fn` is of the required form. Since it takes several lines of code to set up the test, this is best done with `skipIf`. Here's the code:

    val A = fn.fmlargs[0], B = fn.fmlargs[1], AT:xprsym
    val relop:xprsym = matching([:<, :<=, :>, :>=].{ methodNamed(OT, this) }, false, false)
    val fnPtn = pattern(mkterm(:funcall, relop, [mkterm(:symchain, A, AT), mkterm(:symchain, B, AT)]))
    val fnFormOk = fn.defn.stmts.count == 1 && match(fnPtn, fn.defn.stmts[0])
    skipIf(family(T) != :tuple || !fnFormOk)

The `relop` symbol is a PEM using a disjunctive pattern -- it matches function symbols for all four comparison predicates -- "<" and so on. Since the matchable `fn` was bound in the `matchquery`, `A` and `B` are ordinary symbols (that infer to type `xprsym`) while `AT` is a matchable bound when `fnPtn` runs. The ordinary symbol `fnFormOk` checks that the body of `fn` consists of a single statement and that this statement matches `fnPtn`, which requires it to compare the same attribute of the two function arguments. Note that `relop` is generic (on `OT`) when created and that this generic type symbol gets bound when `fnPtn` matches as `relop` is bound. For example, if `fn` was

    \(x, y:T) { x.a1 >= y.a1 }

the match would succeed and set `AT` to `a1`, `OT` to the type of `a1`, and relop to (the symbol named) `$>=`.

An alternate way to write the last 3 lines of the above code extends `fnPtn` to the whole function body:

    val funargs = [mkterm(:symchain, A, AT), mkterm(:symchain, B, AT)]
    val fnPtn = pattern(mkstmts([mkterm(:funcall, relop, funargs)]))
    skipIf(!match(fn.defn, fnPtn))

To emphasize that matching is a syntactic process, we note that neither of these patterns correctly handles a comparison function with an explicit return statement:

    \(x, y:T) { return x.a1 >= y.a1 }

Now we're ready to construct the sub-expressions needed to generate the rewritten expression. We need to define the modified ordering function and the expression for building the simplified list we'll be sorting:

    val XT = mktype(:tuple, [AT, mksym(:inx, integer)])
    val xfnargs = fn.fmlargs.{ mksym(this.ident, XT) }
    val xfnbody = fn.defn.subst(fn.fmlargs, xfnargs)
    val xfn = mkftn(typesubst(fn.type, T, XT), xfnargs, xfnbody)
    val itm = mksym(:item, T)
    val ZZ = mkterm(:litform, XT, [mkterm(:symchain, itm, AT), mksym(:index, integer)])

The local name for the modified sorting function is `xfn`; we construct it by substituting re-typed arguments into the original definition. We then construct a term `ZZ` that constructs literal values of the new type; we intend to substitute this expression into the output expression where the symbol named `item` will be captured by an enumeration.

The final step is to put these elements into a template for the desired code. This is a string that we'll give to parseRW:

    val output = "map(item^biglist) { ZZ }.sort(xfn).[index < K].{ biglist[this.inx] }"
    parseRW(output)

When the string named `output` is parsed and typechecked, the bdgenv includes all the symbols defined locally in earlier statements of the xprftn: `XT`, `xfn`, `ZZ`, `biglist`, `K`, and so on. The definitions of such things are substituted into the parsed expression. (None of `XT`, `xfn`, etc. occur in the output -- what occurs are the `xprterms` bound to these local symbols.) Other symbols in the parsed expression are defined locally, in the usual way. For example, the iteration variable to map is named `item`, and as we mentioned earlier, this is intentionally captured by `ZZ` so the litform gets the correct part of each list element.

While this rewrite rule will give the desired result, it has an important error. The output code has a sub-expression of the form `lst.sort(fn).[index < K]`, so the result will match the `matchquery` pattern. If this rewrite rule is enabled, it will trigger indefinitely, which corresponds to an infinite loop. To fix this, we need to add another condition to the rule. An easy solution is to require that the elttype of `biglist` should be a _complex_ tuple type, with more attributes than the extracted list. So a reasonable solution is

    skipIf(T.attributes.count <= 2)

This ensures the rule is applied at most once.

In fact, this test should be integrated into the `matchquery` rather than as a `skipIf`:

    matchquery("sort(biglist, fn).[index < K]" && T.attributes.count <= 2)

It's worth digesting the implication of this example: any rewrite scheme that's expressive enough to be useful can cause infinite loops. Compilers need to handle this gracefully, for example by limiting the number of times a given rewrite rule can run.

Next, let's consider some limitations of this rule and some generalizations to address them. Consider the following examples:

    val fn0 = \(x, y:T) { x.a1 < y.a1 }
    val fn1 = \(x, y:T) { return x.a1 < y.a1 }
    val fn2 = \(x, y:T) { y.a1 >= x.a1 }
    val fn3 = \(x, y:T) { val foo = x.a1; foo < y.a1 }
    val fn4 = \(x, y:T) { x.a1 < y.a1 || (x.a1 == y.a1 && x.a2 < y.a2) }

Extensionally, `fn0` through `fn3` are identical functions, but only `fn0` is guaranteed to match the rule. The last function, `fn4`, corresponds to the useful pattern of having a primary and secondary sort key; in a robust rewrite rule library we might want to extend the rule to handle this.

To handle `fn1` it suffices to write a disjunctive pattern. `Fn2` inverts the order of formal arguments in the comparison; we can brute force this with another level of disjunctive pattern, or we can add a separate rewrite rule that reorders the definition. Let's explore the second option, partly for the issues it raises. Here, the rule (enclosed in the same kind of given statement as before) might be

    val canonicalOrderFn = \xprftn(lst:list(T), fn:\(T, T)->boolean) {
      matchquery("sort(lst, fn)")
      val A = fn.fmlargs[0], B = fn.fmlargs[1], AT:xprsym
      val relop:xprsym = matching([:<, :<=, :>, :>=].{ mksym(this, \(OT,OT)->boolean) }, false, false)
      val aref = mkterm(:symchain, A, AT), bref = mkterm(:symchain, B, AT)
      val fnPtn = pattern(mkterm(:funcall, relop, [bref, aref]))
      val fnFormOk = ftn.defn.arg0.count == 1 && match(fnPtn, fn.defn.arg0[0])
      skipIf(!fnFormOk)
      val xfn = mkftn(fn.type, fn.fmlargs, parseRW("!relop(aref, bref)"), :pure)
      parseRW("sort(lst, xfn)")
    }

This rule involves many of the same information extraction operations as before, but this time we look for a "wrongly ordered" comparison and reverse it, preserving semantics by inverting the function (with "!"). The rule only triggers on ordering functions called by `sort`, and reconstructs the sort call with the canonicalized ordering function.

This rewrite rule won't cause infinite loops because it can't retrigger once the "wrongly ordered" comparison is fixed.

Turning to `fn3`, the language does not actually define whether this definition would match our original rewrite rule or not. We can all agree that `fn3` is semantically identical to `fn0` but the language definition doesn't require the internal form to eliminate foo or not. I don't have a definitive position on this issue at the moment.

There are numerous other ways to generalize the patterns we've seen. As one example, we could implement reverse sorting by rewriting the ordering function. We're going to consider a different generalization: what about the case where you want to sort on properties that are more complex than an attribute of the list's element type? To make this concrete, suppose `biglist` has a `loc:geocoord` attribute and we're interested in the 25 elements whose `loc` is closest to Omaha (which is in geocoord `locOmaha`). We could sort with an ordering function like this:

    \(x, y: T) { distance(x.loc, locOmaha) < distance(y.loc, locOmaha) }

This would not match the rewrite rule we've written, and we could definitely generalize that rule to handle situations like this. This generalization would cache the distance of every tuple from Omaha in an attribute of the extracted list.

Another approach is to define a version of sort with a function that extracts the ordered entity to compare:

    given(T::entity, OT::ordered) {
      val sort = \(lst:list(T), extract:\(T)->OT) {
        sort(lst, \(x, y:OT) { extract(x) < extract(y) })
      }
    }

Then we can define a rewrite rule to make this `sort` function more efficient. If the `extract` function simply returns an attribute of `T`, this produces the same result as the previous rule but if it's `\(x:T) { distance(x.loc, locOmaha) }` then it's an improvement. Here's our new rule:

    given(T::entity, OT::ordered) {
      val xsort2 = \xprftn() {
        val lst: list(T), fn:\(T)->OT
        matchquery("sort(lst, fn)")
        val AT = mksym(gensym(), OT)
        val XT = mktype(:tuple, [AT, mksym(:inx, integer)])
        val xtnm = mksym(gensym(), :type, XT)
        val xtdecl = mkterm(:val, xtnm)
        val xfnargs = fn.fmlargs.{mksym(this.ident, XT)}
        val lt = mksym(:<, \(OT, OT)->boolean)
        val fnstmts = mkstmts([mkterm(:funcall, lt, xfnargs.{mkterm(:symchain, this, AT)})])
        val xfn = mkftn(typesubst(fn.type, T, xtnm), xfnargs, fnstmts)
        val fnargs = [mksym(:this, T), mksym(:index, integer)]
        val ZZ = mkterm(:litform, xtnm, [mkterm(:funcall, fn, fnargs)])
        val output = "{xtdecl; lst.{ ZZ }.sort(xfn).{ lst[this.inx] }}"
        parseRW(output)
      }
    }

Here, the new function compares each argument's `AT` attribute (whose actual name is derived from the call to `gensym`). Those attributes are initialized to the result of calling the extraction function, `fn`, in the term named `ZZ`. This time, `ZZ` is embedded in a standard dotbrace so the identifier associated with the enumerated element is `:this` instead of `:item`.

This xprftn doesn't have the infinite loop issue because the call to sort in the output is a call to a different function than the one in the input. If we define a version of `sort` as above along with `xsort2`, then write `rewrite(xsort2)` so the rules is enabled unconditionally, the standard definition of (this version of) `sort` will never be run. For even better behavior, we can beef up the definition above to call standard sort with a synthesized comparison function in the case noted above (where extract simply returns an attribute).

Overall then, we've shown a way to ensure efficient compilation of a nicely "abstract problem statement" that seems inefficent and not susceptible to standard compiler optimization. Some programmers might object that you have to know about the rewrite rule in order to realize that the "inefficient" definition is not actually used. An alternate approach is to bake the transform into the definition of the new version of `sort`:

    given(T::entity, OT::ordered) {
      val sort = \(lst:list(T), extract:\(T)->OT) {
        val XT = tuple(xtd:OT, inx:integer)
        lst.{ [XT: extract(this), index] }.modsort(\(x,y:XT) { x.xtd < y.xtd }).{ lst[inx] }
      }
    }

While this is certainly harder to follow, it doesn't "hide" the fact that a more efficient transform is always done. Overall, it's a matter of taste or, as the saying goes, YMMV.

We close this section with a completely different usage of xprftns, to create what I like to call scaffolding. On programming fora nowadays you very commonly see recipes for doing X, Y, or Z with available library functions. Despite the fact that much code is already written, debugged, and documented, it can still be difficult to string it together in the right way to do some specific task. The standard go packages are like this &mdash; dozens of functions with sometimes complex requirements on calling order to get specific results. A rewrite rule has the flexibility needed to automatically fill in a template so that much less manual effort is needed.

Go's time package defines 8 types and a few dozen functions (some couched as methods). Some are pure, like the `String` method of type `Month`. Others are connected, like the `Now` method of type `Time`. The task I'm interested in automating is to write a program that will do some action at regular intervals of real time. Let's start by imagining how we might want to write such a program in d8m. Let's give ourselves a type `rtclock`, constructed with a function

    val rtclock:\(interval:duration)->rtclock

Imagine that an `rtclock` has an attribute `ontick:boolean` that becomes true every `intvl` duration of real time. Now our program is simple:

    import "rtclock"
    val ticker = rtclock(1.0*second)
    oncondition ticker.ontick { ...the action... }

How do we get this to actually work? Everything connected in d8m ultimately happens via go packages, and here we need the `time` package, since its `Ticker` type does exactly what we want. So, a d8m program needs to import go's `time` package and create a `Ticker` with the desired interval, expressed as an integer number of nanoseconds. The `Ticker` contains a channel that returns `Time` entities at the requested interval. As mentioned [here](#back-end-hooks-for-go-features), there are ways to access go channels. In this case, we just need `channelReceive`, since `time.NewTicker` creates the channel. Thus, the low level d8m program we would write to get the behavior we want is

    import go "time"
    val rx0 = time.NewTicker(floor(1.0*cvt2nanosec))
    loop {
      channelReceive(rx0.C)
      ...the action...
    }

The first program we wrote is abstract and conceptually simple. The second is low level and requires understanding go's `time` package, `Ticker` type, channels, etc. A rewrite rule can bridge the gap between these. Furthermore, it can document in a formal and verifiable way the steps you need to take to use the `time` package to get this particular functionality.

Normally, you would put all the needed code into the d8m module named rtclock, which we imported in the first program. Thus, the rtclock module would look something like this:

    import go "time"
    val cvt2nanosec = 1e9
    val rtclock = tuple(interval:duration, ontick:boolean)
    val rtclock = \(ia:duration) { [rtclock: ia, false] }
    assert connected(rtclock)
    val rtclockRW = \xprftn() {
      val action:xprstmts, tkr:rtclock, s0, s1:xprstmts
      val ontick = mksym(:ontick, boolean)
      val stmts = [s0, mkterm(:oncondition, mkterm(:symchain, tkr, ontick), action), s1]
      matchquery(mkstmts(stmts))
      val tkbody = parseRW("time.NewTicker(floor((tkr.interval/second) * cvt2nanosec))")
      val RTTK = mksym(gensym(), time.Ticker, tkbody)
      val tkdecl = mkterm(:val, RTTK)
      parseRW("{s0; tkdecl; loop { channelReceive(RTTK.C); action }; s1}")
    }
    rewrite(rtclockRW)

This rule replaces the `oncondition` statement in the query with the loop defined above, preserving the order of statements occurring before and after it. The rule takes advantage of the special properties of xprstmts matchables, both for matching and later, for generating the output. This example also shows how rewrite rules can include local definitions whose identifiers are guaranteed not to clash with existing symbols in the bdgenv.

The module definition shown here completely encapsulates the steps needed to make this pattern work. It imports the relevant go package, defines rtclock as a connected type with a magical `ontick` attribute that's used in source level programs but doesn't actually appear after rewriting. Then it defines and applies the rewrite rule that allows users to work at the level of abstraction of our first program while getting the functionality of the second one.

# Appendices

## Lexical Details

D8m programs are written in ascii, except for string literals, which are encoded in utf-8. D8m's byte type is unambiguously an 8-bit value; d8m does not have a "character" type, so the issue of "runes" doesn't present itself.

Define a line as an ascii string terminated by newline or end of string. A program consists of a list of lines; in processing programs, the compiler first strips comments, then maps the lines into a list of tokens. End of line can be thought of as a special token that serves as a statement separator if needed. Also, a compiler is allowed to require that when an expression crosses a line boundary, the final token before the newline "extends" the expression. (More formally, the expression should not be reducible after the last token on the line is read.) There are two conventions for comments: nestable pairs of `/*` and `*/` character pairs ("bracketed comments") and `//` to end of line ("line comments"). Line comments are ignored inside a bracketed comment.

Tokens are classified as symbols, literals, or punctuation. Symbol tokens are defined in the usual way: letting A mean alphabetic or underscore and D mean decimal digits, the regular expression is `(A|D)*` using standard regular expression notation. Most symbols are available for user definitions but some are reserved words and others not formally reserved in the syntactic sense are nevertheless "effectively reserved" because they are so broadly defined as built in functions or values. Also, every symbol beginning with two underscores is reserved for use by the compiler.

In the body of this document, I've used the word "keyword" loosely. D8m has reserved keywords, mainly used to indicate the
beginning of statements. Numerous identifiers have special meanings in specific contexts, but are not reserved; examples
include `as` and `melted` in `import` statements. Quite a number of identifiers use function calling syntax in extended ways
that require special parsing and/or typechecking: these include `typepred`, `cvt`, `always`, and more. Finally, numerous
identifiers have built in bindings that designate functions
or values: examples include `reduce` (a generic function on collections) and `start` (a specially interpreted `time`).
The next appendix [here](#reserved-words) tries to cover all of these cases.

Literals include forms that designate values of numeric, label, string, byte, and time types. Numeric values are further
classified as integer and float literals. Integer literals are decimal of the form `D+`; or hex with prefix `0x` or `0X`
followed by `H+` where `H` is decimal digits plus `abcdef` and `ABCDEF`. Floating literals take fraction and scientific
form: fraction is `D+.D*` and `.D+`; scientific form is fraction form suffixed with `e` or `E`, an optional `-`, and `D+`.
(Note that the period character `"."` is not a meta character here.) Label literals are symbol tokens plus certain
punctuation tokens, prefixed by `:`. The operator tokens allowed as labels are listed below; `!`, `@`, and so on.

String literals are enclosed in double quotes or backticks (\`); we'll adopt Go's "raw string" terminology for the second form.
Raw string literals allow neither quotation nor interpolation, but can contain arbitrary characters other than \`, so
they can be written across multiple lines. For double quoted strings, interpolation is the only significant difference
from Go. Double quoted strings may contain any utf-8 code points except carriage return and linefeed, which are represented
in the string by `\r` and `\n` respectively. An occurrence of double quote terminates the string unless it is quoted, also
by `\`. In addition, the `\` character must be quoted with a second `\` character, and tab characters can be written
as `\t`. Thus, double quoted strings admit five backslash quoted characters: `"\rnt`.

String interpolation syntax is borrowed from Ruby: substrings of the form `#{expr}` embedded within a double quoted string
are extracted, code is synthesized to convert each extracted expression to a string, then to concatenate these strings
with the non-interpolated parts. Extracted expressions are converted by wrapping with `cvt` if a `cvt` to string
function is defined on the expression's inferred type; otherwise, the default behavior is to convert `X` with `fmt.Sprint(X)`.
In order to accomodate including the sequence `#{` in double quoted strings, the character
sequence `\#{` is treated as `#{` but without triggering interpolation. An example: the string

    "My name is #{this.name}"

corresponds to

    val s1 = cvt(this.name, string)
    "My name is " + s1

where `s1` is an identifier not used elsewhere in the scope.

Byte literals are enclosed in single quotes and should be either single ascii characters, one of the backslash quoted characters "rnt\'" preceded by backslash, or 1-3 decimal digits preceded by backslash, which is interpreted as a decimal encoding of the corresponding ascii code. Thus, `'a'` is the byte value whose ascii code is `a`; `'\97'` means the same thing, as does `'\097'` (there is no octal option for byte literals). The non-numeric quoted literal forms are as usual: `'\r'` is carriage return, and so on.

Punctuation tokens consist of pure punctuation tokens and operator tokens. The pure punctuation tokens are

    ( ) [ ] { } ~ . , ; ? : :: \ \\ " ' $ ^

The operator tokens are

    ! @ % && || & | * / - + = == === != !== < <= > >=

These are all parsed as infix binary operators except for `!` which is prefix unary, and `-` or `+` which are either infix binary or prefix unary. Operator tokens may be preceded by `:` to designate labels and, with the exception of `&&` and `||` may be preceded by `$` to designate "passive" tokens (i.e. parsed as identifiers).

### Reserved Words

All the identifiers that the parser treats as reserved are given in the following table:

| **assert**   | **break**    | **case**        |
| ------------ | ------------ | --------------- |
| **continue** | **each**     | **else**        |
| **export**   | **extend**   | **given**       |
| **if**       | **import**   | **in**          |
| **loop**     | **map**      | **oncondition** |
| **ortype**   | **return**   | **STMap**       |
| **tuple**    | **typepred** | **unless**      |
| **val**      | **var**      | **while**       |

The next table lists identifiers that are meaningful to the parser in special contexts
but that are otherwise useable as ordinary identifiers:

| identifier  | how used                            |
| ----------- | ----------------------------------- |
| `as`        | in `import` statement               |
| `attribute` | in `extend` expression              |
| `go`        | in `import` statement               |
| `imp`       | in `ftnlit` expression              |
| `melted`    | in `import` statement               |
| `method`    | in `extend` expression              |
| `mod`       | in `ftnlit` and type expressions    |
| `ontime`    | in `extend` expression              |
| `private`   | in `extend` and `tuple` expressions |
| `pure`      | in `ftnlit` and type expressions    |
| `source`    | in `STMap` statement                |
| `target`    | in `STMap` statement                |
| `direct`    | in `STMap` statement                |
| `where`     | in `extend` expression              |
| `xprftn`    | in `ftnlit` expression              |

In addition, quite a few identifiers get standard treatment by the parser but
special treatment by the typechecker, when used in function position in function calls.
These are listed and explained in the following table:

| identifier   | how used                                       |
| ------------ | ---------------------------------------------- |
| `always`     | [see here](#built-in-pseudo-functions)         |
| `applyST`    | [see here](#applying-stmaps-and-rewrite-rules) |
| `cast`       | [see here](#built-in-pseudo-functions)         |
| `changed`    | [see here](#ontime-and-simulation)             |
| `connected`  | [see here](#effective-assert-predicates)       |
| `cvt`        | [see here](#built-in-pseudo-functions)         |
| `enumerable` | [see here](#effective-assert-predicates)       |
| `Gocall`     | [see here](#back-end-hooks-for-go-features)    |
| `purified`   | [see here](#effective-assert-predicates)       |
| `rcvrLinked` | [see here](#effective-assert-predicates)       |
| `rewrite`    | [see here](#applying-stmaps-and-rewrite-rules) |

Gotta mention lithook somewhere in here...

And finish with a list of BI ftns reqd by the lang, or perhaps by refncg a compiler release docmt.

## Typechecking Details

### Literal Forms

We covered the syntax for literals of basic types in [structural-aspects](#structural-aspects) and [lexical-details](#lexical-details). Function literals are covered [here](#declarations). For entities of the three "complex" type families (`tuple`, `list`, and `space`) we use a shared syntax for literals, which we call _litforms_. Litforms were described [here](#structural-aspects) and have been extensively used in examples throughout this document. In this section, we give a careful definition with some caveats we haven't seen previously. Let's start by formalizing the syntax:

    <litform> ::= '[' ( <type> ":" )? <lfelts> ']'
    <lfelts>  ::= <empty> | <taggedelts> | <untaggedelts>
    <taggedelts> ::= <tagelt> ( ',' <taggedelts> )*
    <tagelt>		 ::= <ident> "~" <expr>
    <untaggedelts> ::= <expr> ( ',' <untaggedelts> )

The `<tagelt>` form tags each expression with an identifier using `~` as an infix operator, and is allowed only for tuple types. The identifier is interpreted as the name of the attribute whose value is the corresponding `<expr>`. The `<ident>`'s are _anchors_ so litforms using them are _anchor-tagged_. The optional `<type>:` part is a _type-tag_.

In the untagged form with no type-tag, the typechecker will infer a `list` whose elttype is the inferred type of the first element, unless there is guidance. Guidance includes an explicit declared type when the litform is the binding of that entity, or a litform occurring in `cast`. Thus, for example

    val foo:tuple(a,b:integer) = [1,2]

is accepted, while in

    val foo = [1,2]

the symbol `foo` infers to `list(integer)`. Except in situations like this, the type-tag is required for space types, and for non-anchor-tagged literals of tuple types.

The formal syntax shows that the `<tagelt>` form requires _all_ of the elements to be tagged. Such anchor-tagged litforms may always be in any order, and _may_ also be type-tagged. An anchor-tagged litform that is also type-tagged is unambiguous as to the type; the attributes must be compatible with the type given in the type tag. An anchor-tagged litform that is not type-tagged is typechecked by searching for a previously declared type (i.e. one bound to a symbol with `val`) that has the same set of attribute identifiers, whose types are compatible with the types inferred for the anchor-tagged `<expr>`'s. If such a type exists, the typechecker infers that type. If no such type exists, the typechecker infers a tuple type with no methods having the attributes inferred from the litform (in the order given). We call this an _anonymous_ tuple type and the litform an _anonymous tuple litform_.

These rules enable numerous convenient programming patterns, such as the "xp trick" shown in [chain-operators](#chain-operators), but they have some surprising consequences too. Suppose we've defined

    val tt1 = extend tuple(a, b, c: integer) where { ... }

then a litform like `[a~1, b~2, c~9]` will infer to `tt1` but if we later attempt to use a "different" tuple type anonymously, like `[c~15, b~-3, a~7]` it will _also_ infer to `tt1` because of the rule that the attributes of anchor-tagged litforms can be written in any order. This could conceivably cause capture of a method (of `tt1`) inadvertently.

Also, suppose we've defined more than one tuple type on a set of attributes that's indistinguishable to the typechecker, that is, where the attributes differ only in their order. For example, given the above definition, suppose we also defined

    val tt2 = extend tuple(b, a, c:integer) where { ... }

This is a type distinct from `tt1` but its attributes are a permutation of those of `tt1`, so we can't tell whether a litform like `[a~1, c~5, b~8]` is a `tt1` or a `tt2`. In order to address this, we make the rule that if there are two or more tuple types defined in a scope whose attributes are permutations of each other, then anchor-tagged litforms must be in the defined order of the inferred type. (Of course, this only applies to litforms that are not type-tagged.)

In practice, I have found that anchor-tagged litforms are mostly useful for anonymous tuple types and that issues like those just addressed are exceedingly rare. A convenient way to define the rest of the rules for litform typechecking more formally than we have done in the main body of the text is to pass everything through the rules for the `lithook` method.

Methods named `lithook` may be defined in `extend` expressions for `tuple` types. Such methods may not be called with the usual function calling syntax, but are "called" by writing a type-tagged litform whose elements infer to the arguments of the method (excluding the receiver). In addition, a `lithook` for a type `T` whose only explicit argument is a `list(U)` matches a `T`-tagged litform with any number (including zero) of elements that infer to `U`.

### Normal Typechecking

In normal context, typechecking maps identifiers to symbols (to which a type is associated) and fills in the types of expressions. It infers if there is no _guidance_, else it verifies that the type of an expression is compatible with the guidance. Guidance is provided

1. to the binding of `val` or `var` statements with explicit type declarations
2. to the first argument of `cast` and `cvt` by the second
3. to the components of statements by their expected types
4. to the final statement of a ftnlit body by the return type in its signature, if any

Expected types for each kind of statement are

| statement      | rules                                                       |
| -------------- | ----------------------------------------------------------- |
| `if`, `unless` | see below                                                   |
| `case`         | inferred types of all clauses must be compatible            |
| `loop`         | `nothing`                                                   |
| `while`        | `nothing`                                                   |
| `return`       | `nothing`                                                   |
| `return X`     | the return type of the enclosing function literal, if given |

`If` and `unless` designate `nothing` if the `else` part is missing. In this case, the `then` part must modify or assign to something. Otherwise, both parts are inferred (or verified against the guidance). These must be compatible or else one must be either `nothing` or `exit`. If one is `exit` the inferred type is that of the other branch; if one is `nothing`, that is the inferred type. Types `T0` and `T1` are compatible if they are extensionally equal or one is `<=` the other (see [methods-and-more](#methods-and-more)). In case `T0 <= T1` and `T0 != T1`, the inferred type is `T1`, the less general.

The types of literals are always unambiguous, with mild caveats for anonymous tuple literals, which were covered in [literal-forms](#literal-forms). Typechecking of symbols is where the multibinding rules come into play. If a symbol has multiple bindings in a scope, or an unacceptable binding in a scope and other binding(s) in outer scope(s), the typechecker works as follows. With very few exceptions, context tells us whether a type or entity is expected. Given a symbol and that context requires a type, look for type-valued symbols, else entity-valued ones. If context doesn't tell us (such as in a `val` statement), prefer entity-valued symbols.

Other than type versus entity checking, multibinding applies only to functions. Functions with different numbers of arguments are always TC-distinct; those which have any corresponding pair of TC-distinct argument types, or TC-distinct return types are TC-distinct. Type generators and other definitions based on ftnlit forms do not admit of multibinding. Argument variations of these must be bound to distinct identifiers. Also, as explained in [import-and-export](#import-and-export), qualified identifiers associated with packages and modules through `import` statements must be unique and must not overlap with symbols having other bindings.

### Typechecking Exceptions

When "normal" typechecking fails one of the standard moves is to look for an `autocvt` function. The situation is: expression `X` infers to `T` which is not an acceptable type. If that's due to guidance, we know the acceptable type, call it `T1`. Also, if `X` is a function argument and there is a function symbol whose type in `X`'s position is `T1`, we know that is acceptable. Look for a symbol `autocvt:\(T0)->T1` and if one is found, rewrite `X` to `autocvt(X)` and insert it in the typechecked result. A different but related situation occurs when checking for a match on a multibound function. Each argument has guidance, even if tentative, which can be used in the usual way to check for autocvt. However, this does not apply to the return type, which may or may not have guidance. If it does and the return type of the multibound function fails to match, autocvt will not be applied to make the function match, on the grounds that doing so would extend too far the cognitive load required to read code involving multibinding. In cases where such conversions are needed, use `cvt` explicitly.

### Guarding Behavior

We've seen in the main sections of this document that both `if` conditions and `assert` statements can interact with typechecking of ortypes. A useful way to think about the requirements on `if` conditions is to imagine that when the compiler sees

    if(cond) { S1; S2 } else { S3; S4 }

that it immediately rewrites this to

    if(cond) { assert cond; S1; S2 } else { asssert !cond; S3; S4 }

One makes the obvious extensions of this to `unless` and to `if` statements without an `else` part, plus the slightly less obvious extension that an `if` or `unless` without `else` whose `then` part ends with `continue`, `break`, or `return` inserts an `assert !cond` as the following statement. Given that asserted conditions are manifest, we can then reduce a more formal description of the requirements for guarding behavior to statements about how manifest conditions affect type inference for ortypes.

Suppose `X:nilPossible(T)` and `D:ortype(...)` where `X` and `D` designate arbitrary expressions (possibly symbols). A compiler must disambiguate `X` to `T` (or `nil`) in scopes having assertions of the form `X == nil` and `X != nil`. In addition, if the form of `X` is `F(Y)` where `F` is one of `head`, `tail`, or `last`, then assertions of the form `X == []`, `X.count == 0`, and `X.count > 0` along with their negations, must disambiguate in the same way. For assertions of the form `tag(D) == :tg`, a compiler must map `D`'s type to that of the `tg` tag, while its negation maps the type to the original type without its `tg` variant.

The imagined rewriting described above does a bad job of handling the requirements on `&&` because this accumulates requirements but does so immediately: in `if(cond1 && cond2)` any guarding behavior required by `cond1` must be active in `cond2`. Thus, `if(x != nil && x.a > 12)...` works if the non-nil variant for `x` has an `a:integer` attribute.

Similarly, `||` in a condition cancels guarding behavior: given `if(x != nil || y > 7) TP` nothing about `x` is asserted in `TP`.

A compiler may (but is not required to) infer from an assertion of `X > c` that `X > d` for all `d < c`. Various similar inference capabilities are possible but not required, such as that `X > c` implies that `X >= c+1` for integer `c`, `c < X`, and so on.

## Xprftn Details

This appendix assumes familiarity with the material in the main section on [rewrite rules](#rewrite-rules), such as the definition of the various generalized types, and so on. We classify the built in rewriting support functions informally into a few classes: pattern constructors, matching, substitution, term and type constructors, and getters. Most apply to entity-valued expressions, but a few to type-valued expressions.

All of the functions defined in this section are called as part of the compilation process, to match terms for applying rewrite rules, or as part of the rewriting process. When they are called with the wrong kind of argument, they generate an error, which ends compilation. The tables below define various error conditions for cases when a correct generalized type has an incorrect value; we leave implicit the possibility of errors being declared when calling xprftn functions with arguments that have the wrong kind of generalized types.

The following three tables list the names and signatures of all the built in rewriting support functions. The first table covers pattern construction and matching; the second covers getters; the third covers constructors and substitution. After all three of the tables, we describe the rules for structure of xprterms and cover in some detail what each of the functions does.

| Function     | Arguments                                   | Returns           |
| ------------ | ------------------------------------------- | ----------------- |
| `matchquery` | pattern or `&&` chain starting with pattern | `nothing`         |
| `skipIf`     | `boolean`                                   | `nothing`         |
| `pattern`    | string, xprterm, or list(xprterm)           | `pattern`         |
| `match`      | pattern, xprterm                            | `boolean`         |
| `matchcount` | pattern, xprterm, ...                       | `integer`         |
| `contains`   | pattern, xprterm, ...                       | `boolean`         |
| `matches`    | pattern, xprterm, ...                       | `list(matchinfo)` |

Note that the pattern arguments above can take any of forms noted in the `pattern` function.
String patterns should be processed (i.e. parsed and typechecked) at the time the xprftn is read.
(If this is not done, xprftns defined in modules with patterns matching entities defined therein will build in assumptions about the QI the imported module uses.)

Next, the getters.

| Function              | Arguments                   | Returns                         | Notes                                                       |
| --------------------- | --------------------------- | ------------------------------- | ----------------------------------------------------------- |
| `methods`             | `type`                      | `list(xprsym)`                  |                                                             |
| `methodNamed`         | `type`, `label`             | `xprsym` or error               | If no method with given name                                |
| `pkgSymbol`           | `string`, `string`          | `xprsym` or error               | path then symbol in package; error if no such pkg or symbol |
| `lookup`              | `label`                     | `xprsym` or error               | If no symbol with given name                                |
| `lookupType`          | `label`                     | `type` or error                 | If no type-valued symbol with given identifier              |
| `rewriteStoreDeclare` | `symbol`, `type`, `xprterm` | nothing                         |                                                             |
| `rewriteStoreSet`     | `xprterm`, `xprterm`        | nothing                         | First arg involves rewriteStore attribute                   |
| `rewriteStoreGet`     | `xprterm`                   | `xprterm`                       | Arg must be rooted in a declared rewriteStore attribute     |
| `setDefined`          | string or label, `xprterm`  | nothing                         |                                                             |
| `getDefined`          | string or label             | `xprterm`                       |                                                             |
| `lookupType`          | `label`                     | `type` or error                 | If no type-valued symbol with given identifier              |
| `family`              | `type`                      | `label`                         |                                                             |
| `handlers`            | `type`                      | `list(xprterm)`                 |                                                             |
| `asserts`             | `type`                      | `list(xprterm)`                 |                                                             |
| `attributes`          | `type`                      | `list(xprsym)` or error         | Error unless argument is a tuple type                       |
| `variants`            | `type`                      | `list(xprsym)` or error         | Error unless argument is an ortype                          |
| `argtypes`            | `type`                      | `list(type)` or error           | Error unless argument is a function type                    |
| `rettype`             | `type`                      | `type` or error                 | Error unless argument is a function type                    |
| `purity`              | `type`                      | `label` or error                | Error unless argument is a function type                    |
| `elttype`             | `type`                      | `type` or error                 | Error unless argument is a list or space type               |
| `dimension`           | `type`                      | integer-valued xprterm or error | If argument is not space type                               |
| `type`                | `xprterm`                   | `type`                          |                                                             |
| `ident`               | `xprsym`                    | `label`                         |                                                             |
| `fmlargs`             | `xprterm`                   | `list(xprsym)` or error         | Error unless argument is function-valued                    |
| `defn`                | `xprterm`                   | `xprterm` or error              | Source code if function-valued argument, else error         |
| `termtag`             | `xprterm`                   | `label`                         |                                                             |
| `arg0`                | `xprterm`                   | `xprterm`                       |                                                             |
| `arg1`                | `xprterm`                   | `xprterm` or error              | If argument has at least 2 terms                            |
| `arg2`                | `xprterm`                   | `xprterm` or error              | If argument has 3 terms                                     |
| `args`                | `xprterm`                   | `list(xprterm)` or error        | If argument has a list of terms                             |
| `stmts`               | `xprstmts`                  | `list(xprterm)`                 |                                                             |
| `uses`                | `xprterm`                   | `list(xprterm)`                 | All symbols and symchains in arg                            |
| `usescount`           | `xprterm`, `xprterm`        | `integer`                       | Counts instances of second arg in first                     |
| `defs`                | `xprterm`                   | `list(xprsym)`                  |                                                             |
| `mods`                | `xprterm`                   | `list(xprterm)`                 |                                                             |
| `anyConnected`        | `xprstmts`                  | `boolean`                       | true if connected funcalls in stmts                         |
| `source`              | `xprterm`                   | `string`                        |                                                             |

A getter normally takes a single argument that's required to be of a specific generalized type, and returns a property of that kind of thing. Thus, getters can be conveniently divided according to the generalized type of their argument: getters on types, on xprterms, and on xprsyms. Most of the getters on types are family specific and return errors if called on the wrong type family. However, the `methods`, `handlers`, and `asserts` getters apply to all types, and return the corresponding property of the type. The `methodNamed` getter expects a label with the name of the desired method, and exits with an error if such a method is not found. Similarly, `lookup` looks for an entity-valued symbol with the given ident, while `lookupType` does the same for type-valued symbols. Each returns the symbol in the closest enclosing scope, if any, but with no provision for multibinding.

The family specific getters on types are: `attributes` for tuple types; `argtypes`, `rettype`, and `purity` for function types; `elttype` for list and space types; `dimension` for space types. The token `type` can be used as a getter on any xprterm (which includes xprsyms, etc). The only getter specifically for xprsym is `ident`, which returns the identifier as a label. The only getter specifically for xprstmts is `stmts`, which returns a `list(xprterm)` of the statements. For ftnlit xprterms, the getters `fmlargs` and `defn` return the `list(xprsym)` of formal arguments and the definition (as an xprterm which will always be an xprstmts) respectively. Certain built in functions do not have source code definitions and for these, `defn` will fail. (This is a version of "is not manifest".)

Most of the remaining functions allow you to pick apart xprterms: `termtag` works on any xprterm and returns a label that gives information about its structure. Further details are given below following the table of tag values. The getters `arg0`, `arg1`, `arg2` return components of xprterms with specific tags. Details are given below. The final functions in the list allow to retrieve dataflow information about symbols and terms. `Uses` and `mods` are given an xprterm (typically a statement list) and return a list of symbols used and modded, respectively, in the term. The `mods` result includes the lval of assignments and the receiver of mod function calls as long as they are symbols or symchains; symchains get the root symbol. Thus, if statement `S` is `a.b = c`, then `mods(S)` is `[a]`, while `uses(S)` is `[a, c]`. The order of elements in these lists is unspecified and there will be no duplicates -- they are really sets typed as lists. `Defs` is very similar but only returns symbols, namely the symbols defined (locally) in the term. `Defs` only finds symbols defined in the scope of the term, not in nested scopes. `Usescount` gets two terms and returns the number of times the second term occurs in the first. `AnyConnected` searches a xprstmts for calls to connected functions, returning a boolean result. `Source` returns a string corresponding to the source code from which the given xprterm was derived. Note that this can be problematic, for example if the xprterm is synthesized in the xprftn or is derived from source code via numerous transformations. The resulting string should at minimum parse to an equivalent xprterm, but the behavior of `source` will be compiler dependent.

The next table has functions for substitution and construction:

| Function       | Arguments                                                 | Returns    |
| -------------- | --------------------------------------------------------- | ---------- |
| `subst`        | `xprstmts`, `list(xprterm)`, `list(xprterm)`              | `xprstmts` |
| `typesubst`    | `type`, `type`, `type`                                    | `type`     |
| `parseRW`      | `string`                                                  | `xprterm`  |
| `gensym`       |                                                           | `label`    |
| `typedAs`      | `xprterm`, `type`                                         | `xprterm`  |
| `renameLocals` | `xprstmts`, `xprstmts`                                    | `xprstmts` |
| `mktype`       | `label or symbol`, more...                                | `type`     |
| `mktypex`      | `type`, `list(xprterm)`, `list(xprterm)`, `list(xprterm)` | `type`     |
| `mkterm`       | `label`, more...                                          | `xprterm`  |
| `mksym`        | `label`, `type`, optional `xprterm` (binding)             | `xprsym`   |
| `mkftn`        | `type`, `list(xprsym)`, `xprstmts`                        | `xprterm`  |
| `mkstmts`      | `list(xprterm)`                                           | `xprstmts` |

The substitution functions are named `subst` and `typesubst`. `Subst` receives a statement list and two lists which define a mapping from old to new terms. It returns a new statements list in which the expressions in the first list have been replaced by the corresponding ones in the second. `Typesubst` maps types to types and substitutes the third argument for the second in the first. Note that `typesubst` handles one type at a time while `subst` handles lists of terms. `ParseRW` converts a string to internal form. A d8m term in string form is parsed and typechecked in the current bdgenv. After typechecking, the bindings of bound matchables are substituted for occurrences of the matchables.

`Gensym` returns a label guaranteed to be distinct from any identifier used in any accessible scope. It is normally used to create the identifiers for symbols created with `mksym`. `TypedAs` designates the incoming term with the indicated type.
`RenameLocals` is a rather specialized function that receives two `xprstmt` arguments and returns the second modified so that identifiers of its locally declared symbols do not interfere with those declared in the first. This makes it easier to correctly merge two statement lists in user code.

The constructor functions all have names beginning with "mk" (as in "make"). `Mktype` mimics the action of standard type constructors while `mktypex` does things normally done with `extend`. Note that `mktype` isn't needed for basic types, for example, to designate type integer write `integer`.

The first argument to `mktype` is a label whose value is a type family name chosen from the list of type families (i.e. `:list`, `:space`, `:tuple`, `:function`, etc), or the name of a type generator. The other arguments depend on the label. For type family names, the arguments are in the following table:

| Family      | Arguments                                         |
| ----------- | ------------------------------------------------- |
| `list`      | `type`                                            |
| `space`     | `type`, `list(xprterm)` integer valued            |
| `tuple`     | `list(xprsym)`                                    |
| `function`  | `list(type)`, `type`, `label` (`:pure` or `:mod`) |
| `integer`   |                                                   |
| `float`     |                                                   |
| `string`    |                                                   |
| `Gomap`     | `type`, `type`                                    |
| `Gochannel` | `type`, `label`                                   |

If the first argument of `mktype` is a type generator name, the other arguments should be appropriate for it. For example, if the bdgenv contains a binding for `set:\typegen(T::entity)`, you can write

    mktype(set, mktype(:list, integer))

to get the type of sets of lists of integer.

`Mktypex` accepts a type and three `list(xprterm)` arguments, each possibly empty. These define respectively the methods, handlers, and asserts to be added to the type in the first argument. Methods will normally be created with `mkftn`; the signature must include the receiver. Handlers and assertions can be created with `mkterm`.

`Mkterm` accepts a label for the tag followed by arguments whose number and type requirements depend on the value of the tag. Full details are found in the following paragraphs.

Both of `mktype` and `mkterm` require the initial (label-valued) argument to be literal.

`Mksym`, `mkftn`, and `mkstmts` all have quite simple argument structures. `Mksym` assembles an identifier (presented as a `label`), a `type` or the label `:type`, and optionally, a term that's bound as the value; `mkftn` takes a (function) type, a list of formal arguments, and a body; `mkstmts` takes a `list(xprterm)` of statements. The `mkterm` constructor is more complicated because its arguments depend on the value of the label (tag) in the first argument. Details are found below.

The tags associated with terms are compiler dependent, as discussed [here](#rewrite-rules). The following definition applies to the initial compiler; it is not formally part of the language specification.

| Tag            | Arguments                                    | Notes                                    |
| -------------- | -------------------------------------------- | ---------------------------------------- | --- | --- |
| `:val`         | `xprsym`                                     | previously assembled symbol              |
| `:var`         | `xprsym`                                     | same                                     |
| `:gdref`       | `xprterm`, `type`                            | for guarded expr                         |
| `:assert`      | `xprterm`                                    | what is asserted                         |
| `:litform`     | `type`, `list(xprterm)`                      | argument list                            |
| `:symchain`    | `xprterm`, `xprsym` or `list(xprsym)`        | tuple-valued thing, attribute(s)         |
| `:funcall`     | `xprterm`, `list(xprterm)`                   | function, argument list                  |
| `:and`         | `list(xprterm)`                              | clauses of `&&`                          |
| `:or`          | `list(xprterm)`                              | clauses of `                             |     | `   |
| `:eq`          | `xprterm`, `xprterm`                         | operands of `$=`                         |
| `:eqeq`        | `xprterm`, `xprterm`                         | operands of `$==`                        |
| `:not`         | `xprterm`                                    | operand of `!`                           |
| `:arrow`       | `lhs` `rhs`                                  | pair of exprs                            |
| `:if`          | `xprterm`, `xprstmts`, `xprstmts` (optional) | condition, then, else                    |
| `:ifcase`      | `list(xprterm)`                              | clauses, each an arrow expr              |
| `:typecase`    | `xprterm`, `list(xprterm)`                   | subject then clauses, each an arrow expr |
| `:each`        | `xprsym`, `xprterm`, `xprstmts`              | iteration variable, enumerable, body     |
| `:loop`        | `xprstmts`                                   | body                                     |
| `:oncondition` | `xprterm`, `xprstmts`                        | condition, body                          |
| `:asgn`        | `xprterm`, `xprterm`                         | lval, rval                               |
| `:ontime`      | `xprterm`, `xprftn`                          | condition, body                          |
| `:irange`      | `xprterm`, `xprterm`, `xprterm`, `xprterm`   | integer range                            |
| `:frange`      | `xprterm`, `xprterm`, `xprterm`, `xprterm`   | float range                              |
| `:import`      | `string`, optional `label`                   | module import, label can be "melted"     |
| `:importpkg`   | `string`, optional `label`                   | package import, label can be "melted"    |
| `:rewrite`     | `xprterm`                                    | rewrite pseudo                           |
| `:applyST`     | `type`, `xprterm`                            | applyST pseudo                           |

A few notes on these tags. For `:val` and `:var` one should assume that the internal form splits multiple declarations at source level into multiple, single-declaration "statements" internally. Thus, in the table, each term makes a single declaration. Regarding `:asgn`, the lval may be a symbol or symchain. I believe most of the rest is self explanatory.

When the `termtag` getter is called on an `xprterm` with one of the above forms, it returns the corresponding label. When called with a symbol, it returns `:symbol`. When called with a literal of some basic type it returns `:baseliteral`.

## Methods on Built In Types

### Numeric Types

Integer and float have similar but not identical sets of builtin methods. Those in common are given in the following table where `N` represents either integer or float but bound identically throughout a given signature:

| Name  | Signature         | Notes                 |
| ----- | ----------------- | --------------------- |
| `$+`  | `\(N,N)->N`       | Addition              |
| `$-`  | `\(N,N)->N`       | Subtraction           |
| `$*`  | `\(N,N)->N`       | Multiplication        |
| `$/`  | `\(N,N)->N`       | Division              |
| `$<`  | `\(N,N)->boolean` | Less than             |
| `$<=` | `\(N,N)->boolean` | Less than or equal    |
| `$>`  | `\(N,N)->boolean` | Greater than          |
| `$>=` | `\(N,N)->boolean` | Greater than or equal |

In addition certain methods are defined only for integer or float, or to convert between them. In the table we use `I` for integer and `F` for float.

| Name      | Signature   | Notes                                               |
| --------- | ----------- | --------------------------------------------------- |
| `$%`      | `\(I,I)->I` | Remainder                                           |
| `autocvt` | `\(I)->F`   | Silent integer to float conversion for typechecking |
| `floor`   | `\(F)->I`   | smallest integer <= function arg                    |
| `ceiling` | `\(F)->I`   | smallest integer >= function arg                    |
| `round`   | `\(F)->I`   | round(x) == floor(x+0.5)                            |

Note that there is no automatic conversion from float to integer; you must specify using one of the three built in functions how you want this conversion to happen. (You can, of course, define different rounding functions, which can be useful in certain numerical situations.)

Finally, the identifiers `SmallestInteger`, `LargestInteger`, `SmallestFloat` and `LargestFloat` are predefined to the smallest (most negative) and largest representable values respectively.

### Byte

| Name      | Signature                      | Notes                                                                          |
| --------- | ------------------------------ | ------------------------------------------------------------------------------ |
| `set`     | `\mod(byte, integer)->nothing` | set a bit of the byte; no op unless bn in 0..7                                 |
| `clear`   | `\mod(byte, integer)->nothing` | clear a bit of the byte; no op unless bn in 0..7                               |
| `isset`   | `\mod(byte, integer)->boolean` | test whether the given bit is set; false unless bn in 0..7                     |
| `autocvt` | `\(byte)->integer`             | Silent conversion to integer in range 0..255 (no option for signed conversion) |

There will also be various built in functions to convert `space(byte, [N])` to and from integer for values of `N` like 2, 4, 8, etc.

### String

| Name        | Signature                                     | Notes                                          |
| ----------- | --------------------------------------------- | ---------------------------------------------- |
| `$<`        | `\(string, string)->boolean`                  | std lexical compare                            |
| `$>`        | `\(string, string)->boolean`                  | std lexical compare                            |
| `$<=`       | `\(string, string)->boolean`                  | std lexical compare                            |
| `$>=`       | `\(string, string)->boolean`                  | std lexical compare                            |
| `$+`        | `\(string, string)->string`                   | string concatenation                           |
| `count`     | `\(string)->integer`                          | string length                                  |
| `rvalindex` | `\(string, integer)->byte`                    | get indexed element, `s[i]` in std syntax      |
| `select`    | `\(s:string, lo:integer, hi:integer)->string` | substring of `s` from `lo` to `hi` (exclusive) |

### Time and Duration

In the table we abbreviate `time` with `T` and `duration` with `D`

| Name | Signature    | Notes                                 |
| ---- | ------------ | ------------------------------------- |
| `$+` | `\(T, D)->T` | time plus duration is a time          |
| `$-` | `\(T, D)->T` | same for subtraction                  |
| `$-` | `\(T, T)->D` | subtracting two times gets a duration |

The idea with time literals is that the slash separated ones model "historical times" while times offset from `start` model "unanchored" or non-historical ones. Normally, one should not mix anchored and unanchored times in a given query, but the details are compiler-specific. Similarly, time units are marvelously complicated and d8m makes no attempt to standardize them.

### Label

Label has just one method, `<` whose properties were described in the introductory section on types.

### List

List is a "workhorse" type family, with a rich set of built in methods and functions. We start with the methods. In the table, `T` is the element type and the unlike the previous sections, the receiver (which is of type `list(T)`) is not shown -- so the signatures below are like those in a `method` statement. Also, `U::entity` is used as needed, `NP` abbreviates `nilPossible`, and arguments are named (as in a function literal) to facilitate understanding of what they mean.

| Name          | Signature                                                           | Notes                                                                                                                                              |
| ------------- | ------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| `select`      | `\(lo, hi:integer) -> list(T)`                                      | new list of elements in given (exclusive) range                                                                                                    |
| `count`       | `\() -> integer`                                                    | length                                                                                                                                             |
| `index`       | `\(elt:T) -> NP(integer)`                                           | index of first occurrence of `elt`, else `nil`                                                                                                     |
| `head`        | `\() -> NP(T)`                                                      | first element of list, `nil` if empty                                                                                                              |
| `tail`        | `\() -> NP(list(T))`                                                | rest of list, `nil` for `[]`                                                                                                                       |
| `last`        | `\() -> NP(T)`                                                      | last element or `nil` for `[]`                                                                                                                     |
| `reverse`     | `\() -> list(T)`                                                    | new list with elements in reverse order                                                                                                            |
| `rvalindex`   | `\(i:integer) -> T`                                                 | get ith element, written `lst[i]`                                                                                                                  |
| `lvalindex`   | `\mod(i:integer, v:T) -> nothing`                                   | change ith element, written `lst[i] = v`                                                                                                           |
| `replace`     | `\mod(ofs:integer, other:list(T)) -> nothing`                       | generalizes lvalindex to replace multiple elements starting at ofs with elts of other; panic if ofs > self.count; otherwise extends self as needed |
| `insert`      | `\mod(i:integer, nv:T) -> list(T)`                                  | insert nv at index i and push following elements up 1                                                                                              |
| `insert`      | `\mod(i:integer, other:list(T)) -> list(T)`                         | insert (all of) other at index i and push following elements up length of other                                                                    |
| `append`      | `\mod(other:list(T)) -> list(T)`                                    | append other to self                                                                                                                               |
| `removeElt`   | `\mod(e:T) -> nothing`                                              | mod self by removing first occurrence of e (via `$=`); no effect if `e` not found                                                                  |
| `copy`        | `\mod(src, dest, len:integer) -> nothing`                           | move elements within a list: from `src` to `dest` for `len` elements; all of `src`, `dest`, `src+len`, and `dest+len` must be < `self.count`       |
| `copy`        | `\()->list(T)`                                                      | shallow copy of `self`                                                                                                                             |
| `copyFrom`    | `\mod(ofs:integer, from:list(T)) -> nothing`                        | copy `from` into `self` starting at `ofs`; error if from.count+ofs > self.count                                                                    |
| `removeIndex` | `\mod(i:integer) -> nothing`                                        | mod self by removing element at index `i`; panic if self's length < i                                                                              |
| `removeSeq`   | `\mod(lo, hi:integer) -> nothing`                                   | remove elements between `lo` and `hi`; panic if `lo > hi`                                                                                          |
| `swap`        | `\mod(i0, i1:integer) -> nothing`                                   | swap elements at given indices                                                                                                                     |
| `maplst`      | `\(fn:\(T, integer) -> U) -> list(U)`                               | see [chain-operators](#chain-operators)                                                                                                            |
| `filt`        | `\(fn:\(T, integer) -> boolean) -> list(T)`                         | see [chain-operators](#chain-operators)                                                                                                            |
| `filtx`       | `\(fn:\(T, integer) -> boolean, xfm:\(T, integer) -> U) -> list(U)` | see [chain-operators](#chain-operators)                                                                                                            |
| `filt1`       | `\(fn:\(T, integer) -> boolean) -> NP(T)`                           | see [chain-operators](#chain-operators)                                                                                                            |
| `filt1x`      | `\(fn:\(T, integer) -> boolean, xfm:\(T, integer) -> U) -> NP(U)`   | see [chain-operators](#chain-operators)                                                                                                            |
| `$in`         | `\(elt:T) -> boolean`                                               | true if `elt` is in `lst`                                                                                                                          |
| `$+`          | `\(other:list(T)) -> list(T)`                                       | new list with other appended to self                                                                                                               |
| `$+`          | `\(other:T) -> list(T)`                                             | new list with other appended to self (aka non-mod version of pushb)                                                                                |
| `pushf`       | `\mod(elt:T) -> list(T)`                                            | mod self by pushing elt onto front of self; return self (aka non-mod version of append)                                                            |
| `pushb`       | `\mod(elt:T) -> list(T)`                                            | mod self by adding elt after last elt of self; return self                                                                                         |
| `popf`        | `\mod() -> NP(T)`                                                   | mod self by popping first elt                                                                                                                      |
| `popb`        | `\mod() -> NP(T)`                                                   | mod self by popping last elt                                                                                                                       |
| `uniq`        | `\() -> list(T)`                                                    | new list consisting of distinct elements of self in order                                                                                          |
| `contains`    | `\(other:list(T)) -> boolean`                                       | true if all elements of other are also elements of self                                                                                            |
| `disjoint`    | `\(other:list(T)) -> boolean`                                       | true if no element of self equals any element of other                                                                                             |

In addition, `flatten1` is a built in function on lists of lists that concatenates them using `reduce`:

    given(T::entity) val flatten1 = \(lst:list(list(T))) { reduce($+, lst, []) }

And `zerolist` is a function that creates lists with a specified number of elements all equal to the zero value of the type:

    given(T::entity) val zerolist = \(cnt:integer) { built in }

### Space

As stated in [types](#types), the `space` type generator is designed to accomodate tensor-like
structures of indefinite rank. A fully fleshed out version of this design will have numerous
methods that differ per rank, for example to provide matrix operations on 2 dimensional spaces,
etc. In the initial implementation, the `space` type generator is restricted to a single
dimension, which means space types are fixed length vectors, corresponding to array type
in Go. As a result, only a small set of methods is provided:

| Name        | Signature                                | Notes                                                   |
| ----------- | ---------------------------------------- | ------------------------------------------------------- |
| `select`    | `\(lo, hi:integer) -> space(T, [hi-lo])` | new space of elements in given range                    |
| `count`     | `\()->integer`                           | always manifest since length is part of the type        |
| `copy`      | `\()->list(T)`                           | shallow copy of `self`                                  |
| `rvalindex` | `\(i:integer) -> T`                      | get ith element, written `lst[i]`                       |
| `lvalindex` | `\mod(i:integer, v:T) -> nothing`        | change ith element, written `lst[i] = v`                |
| `move`      | `\mod(lo, hi, dest:integer) -> T`        | moves the contiguous set of elements at lo...hi to dest |

As with list, methods are also provided to create zero valued spaces of various types:

| Name               | Signature                             | Notes                           |
| ------------------ | ------------------------------------- | ------------------------------- |
| `zerospaceByte`    | `\(n:integer) -> space(byte, [n])`    | space of n zero valued bytes    |
| `zerospaceInteger` | `\(n:integer) -> space(integer, [n])` | space of n zero valued integers |
| `zerospaceFloat`   | `\(n:integer) -> space(float, [n])`   | space of n zero valued floats   |

In addition, there are some built in methods for the type `space(bit, [64])`; similar
methods may be added at some point for other bit-space types and possibly `list(bit)`.
For convenience in the following table, we write `space(bit, [64])` as `bits64`, and
we write them as functions (with the `self` argument explicit). Autocvt is defined for
`integer -> bits64` but you must use `cvt` to get back to integer.

| Name      | Signature                                | Notes               |
| --------- | ---------------------------------------- | ------------------- | ----- |
| `lshift`  | `\(self:bits64, amt:integer) -> bits64`  | shifted bit array   |
| `rshift`  | `\(self:bits64, amt:integer) -> bits64`  | shifted bit array   |
| `and`     | `\(self:bits64, other:bits64) -> bits64` | self & other        |
| `andnot`  | `\(self:bits64, other:bits64) -> bits64` | self & ~other       |
| `or`      | `\(self:bits64, other:bits64) -> bits64` | self                | other |
| `bitsNot` | `\(self:bits64) -> bits64`               | bit inversion       |
| `xor`     | `\(self:bits64, other:bits64) -> bits64` | logical xor of bits |

In addition, the following built in functions are provided:

    b64mask = \(n:integer) -> bits64
    bitnum = \(n:integer) -> bits64
    cvt = \(n:integer) -> bits64
    cvt = \(b:bits64) -> integer

Mask creates a `bits64` whose first `n` bits are 1 whereas `bitnum` sets just bit `n`; `cvt` functions work
in both directions.

### Tuple

The only built in method shared by all tuple types is `copy`:

    given(T::tuple) val copy:\(self:T)->T { returns shallow copy of self }

### Quantity Types

Each quantity type has built in methods to multiply by numbers (both integer and float) and to add and subtract commensurable quantities. Thus, given `Q::quantity`

| Name | Signature          |
| ---- | ------------------ |
| `$*` | `\(Q, integer)->Q` |
| `$*` | `\(Q, float)->Q`   |
| `$/` | `\(Q, integer)->Q` |
| `$/` | `\(Q, float)->Q`   |
| `$+` | `\(Q, Q)->Q`       |
| `$-` | `\(Q, Q)->Q`       |

Note that one can also use `*` and `/` on quantity types themselves to generate new quantity types, as discussed in [types](#types); this is a distinct use of these operators.

## Oncondition Handler Interference Details

In [oncondition](#oncondition) we promised full details on the resolution of the problem when the `refset` of a handler intersects with the `modset` of a handler. It was explained in the earlier section that this leads to infinite regress in the naive description of what an `<oncond>` handler does, which normally involves a conditional insertion of the handler's body after mods to entities in the `refset`. Consider a case where

    intersection(refset(H), modset(H)) == {a}

meaning the entity with shared reference is bound to symbol `a`. That means there's a use of `a` in the conditional of `H`, and a statement, call it `X`, in the body of `H`, that mods `a`. Instead of inserting a conditional after `X`, we can insert a loop:

    X; while(H.cond) H.body

This replaces an infinite regress of `if` statements with a single, well-formed statement that has the right meaning. It is the basic idea of the approach we adopt in d8m.

The full detail involves cases where refset/modset overlaps occur among _different_ handlers. Let's restate the definition of `refset` and `modset`, both functions of `<oncond>` handlers whose range is a set of (moddable) _entities_, which are generally referred to by a symbol or attribute reference:

    refset(H): entities referred to in the condition part of H
    modset(H): entities modded in the body of H

Given all the handlers `H{i}` occurring in a scope, we define a directed graph whose nodes are the `H{i}` and with an edge from `Hi` to `Hj` just in case

    intersection(modset(Hi), refset(Hj)) != emptyset

We then calculate the strongly connected components (_SCC_'s) of this graph. For each _SCC_ consisting of a single handler with no self loop, we insert the usual `if` statement at each code point where any element of the `refset` might be modded. For a single handler with a self loop, insert the `while` statement given above. For the general case, suppose a _SCC_ consists of handlers `H1` and `H2`. At each code point where any element of the union of the refsets of the two handlers might be modded, insert the following code:

    while (H1.cond || H2.cond) {
      if(H1.cond) H1.body
      if(H2.cond) H2.body
    }

The generalization to more handlers should be clear.
