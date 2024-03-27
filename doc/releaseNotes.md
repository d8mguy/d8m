
# Release Notes

## Using the compiler

The release includes three "driver" files corresponding to 3 ways of using the compiler. The file names are
- cdriver.go
- driver.go
- testdriver.go

Before we cover the details of what these drivers do, let's talk about what the compiler is looking for. A long term vision for d8m is to run as a REPL where you can interactively enter a sequence of statements and/or expressions, and where declarative statements modify the compiler state and expressions are treated as queries that produce answers, or in certain cases, compiled executables. 

Making a compiler that can reliably compile many times with the same state is a bit challenging. Somewhere along the way I decided it wasn't a priority. So, the standard operating mode of the compiler today is to read a sequence of declarative statements followed by a __query__, which is typically an expression, often a function call. It compiles the query and exits. 

One of the reasons the REPL experience isn't so important is that for non-trivial problems you'll be doing some planning, so you're going to be loading pre-edited files anyway. The d8m compiler is fast, so processing the declarative statements multiple times doesn't slow you down. 

Note that whereas Go, in its opinionated glory, requires you to name your query `main()`, d8m lets you call any function you like. 

Cdriver.go creates the standalone d8m compiler `d8mc`, which the INSTALL document directs you to install in `usr/local/bin` so that you can type

    d8mc myProgram.d8m
to compile your own programs.

D8mc is generally quite fast: 1-2 seconds is typical; most of that time is spent in the go compiler. 

Per the discussion just above, `d8mc` expects `myProgram.d8m` to consist of some declarative statements followed by a query. For convenience, if there is no query and the last definition in the file happens to be that of a zero argument function, it will synthesize a query that calls this function. 

Testdriver.go is used in the make target `runtests` to create the test suite runner. This is also covered in the INSTALL document.

Finally, driver.go gives you a way to make a kind of crippled REPL interface to the compiler. It reads a local file named `foo.txt` and executes the statements therein until it sees a query, which it compiles and runs. For historical reasons, with driver.go you need to wrap the query in `compile(...)`. I have used this, and still use it, in goland to debug compiler problems, and occasionally to debug problems with d8m programs.  

### Structuring Larger Programs

Modules are a key structuring mechanism for d8m, as for many other programming languages. D8m also depends on go packages for the low level stuff; both use a version of the `import` statement.

Modules are easy to define, and are generally a good way to structure programs with multiple files. Beyond modules, `load` is a built in function that loads a named file. It works for modules too, making available all the definitions in the module, not just the exported ones. (This can be useful for modules you don't want to modify, such as official ones.) 

D8m has no package like facility, so the d8m compiler cannot generate "object files". Only executables. 

### Notes on Compiler Behavior

As noted elsewhere, this is not a production compiler. I believe that if you learn the language by reading and trying out the code in this and the companion releases, and then write your own, you'll be quite successful. If you try to crash the compiler with tricky corner cases, you'll probably be successful there as well. 

I would characterize the compiler's error messages as good but not great. A key area of weakness here relates to overloading/multibinding. Since an identifier can designate many functions, the compiler tries to be extra careful when it fails to find a correct mapping (perhaps because you forgot an argument or a conversion). Then the error message may not be too helpful. Error messages usually do a good job of pointing you to where the error was detected (line and offset in line). If the message itself isn't helpful, check the location. If it's a function call, the problem is almost certainly that the typechecker couldn't match all arguments (or return types) for some reason.

One exception to the location accuracy of error messages is that when errors are detected in string interpolated expressions, _no location information_ is provided. 

The compiler quits after one error message. This isn't ideal but compilation is fast enough that debugging one error message at a time doesn't really slow you down. 

A general area of weakness is with unused symbols. Go rejects such things, so the d8m compiler must track and eliminate them. Obvious cases are handled but corner cases still cause problems. 

## Language Restrictions

The language in the specification extends beyond the language in the released compiler in a number of ways; the main differences are cited in the following list:

1. Very limited support for `oncondition`. The `oncondition` statement is parsed and typechecked, but its condition is restricted to two special cases. First, the `defined` pseudo-function, allowed only global context, is used to manage inclusion dependencies in the imports library. Second, the `exit` condition mimics Go's `defer` statement. 
2. No support for simulated time. Specifically the `ontime` statement is parsed but causes the typechecker to panic. Situated types are defined internally and the `x@t` operator is parsed, but both cause the typechecker to panic.
3. Very limited support for `space` types. Note that the spec defines the `space` type generator as designating fixed length spaces of any dimension, then explicitly restricts the dimensionality to 1 on the grounds that higher dimensions require further design work that hasn't been done. In addition to this restriction, although space types are parsed and typechecked, essentially no testing has been done on them. You should not expect to be able to use spaces in a serious way.
4. Limited support for the `ortype` version of `case` expression. This is compiled, but has received little testing.
5. Limited support for entities typed as type predicates. The current compiler will unroll and inline manifest types bound to symbols typed as type predicates but will not generate type switch type assert based code, which is the proper translation to go.
6. Quantity types are unsupported. They are defined internally as types, the `dimension` statement is parsed and typechecked; but compound quantity types (like `length/duration`) aren't typechecked, nor are quantity forming expression (like `12 * second`). 
7. "Back casting" is not implemented.
8. Optional arguments to methods are not implemented. (Note: optional arguments to functions are implemented but barely tested.)

### Known Bugs (with workarounds)
- Don't declare a local variable named `strings`; it interferes with code generation for `print` and `println`.
- Consider `val xxx = lst[lo...hi]`. This is rewritten to an expression using the `list.select` method, ie `lst.select(lo, hi)`. D8m semantics says this is a new entity; go semantics says it's a slice of `lst`. The compiler currently implements go semantics; you have to explicitly copy if the underlying slice can change.
- In a similar vein, d8m type equality is extensional while go is not. There are cases where you might define a type like


        val myType = tuple(a, b:integer)
        var x = [myType: 1, 5]
and later desire to assign a value generated with an equivalent anonymous type:

        ...x = [a~12, b~15]
This is legal d8m semantics but it generates code that the go compiler won't accept &mdash; go will want a `myType` and a `struct{a int; b int}` won't do. The workaround is to not use the anonymous type in the assignment.
- The `zerolist` function can currently only be called to bind a declaration; it should be possible to call it in a `cast` expression.
- rewrite rule activations (with rewrite(...)) are supposed to follow scopes but currently are assumed to be global.
- The expression `reduce(lst, max, -1)` fails with an obscure error message in optimization phase. Workaround: replace `max` with its definition as a function literal.
- The optimization phase does extensive inlining; this means that the formal arguments and local variables of functions are introduced potentially many times. All backend activities include extensive efforts to recognize and rename variables that shadow illegally, by adding 0,1,2,... to the identifier. The effect of bugs in this area is usually to generate go code that doesn't compile, though silently incorrect go code is possible. I am pretty sure there are bugs here, and that I cannot characterize when they might pop up. If the compiler generates uncompilable code, this is one possible reason; if the compiled code behaves bizarrely, this sort of shadowing might be worth checking for in the generated code. There is no real workaround, although small changes in the source code are likely to change what the optimizer sees in ways that might cause the problem to disappear (at least temporarily).


### Partial Support and/or Inadequate Testing

1. Minimal testing of the xprftn function `matches`, and the `Gochannel` and `Gocall` functions.
2. The `manifest` function is defined but it does not have the specified effect on assertions in all cases. 
3. The spec says that the right hand side of xltrules (in `STMap` statements) can be any expression. In fact, it can only be a call on a method of the target type. 
4. Utf-8 support is weak. The spec says that programs are in ascii while strings are in utf-8. In fact, using go packages (via modules) gets utf-8 support per go documentation, but native d8m code in modules is ascii-specific.


### Desirable But Unimplemented Optimizations

1. The compiler does not recognize or optimize common sub-expressions.
