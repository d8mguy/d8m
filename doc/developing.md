## Notes on Development

Herein are descriptions of how I work with d8m, how I debug, and so on.

I use goland as my editor and IDE. I added `*.d8m` to the list of extensions under Settings > Editor > File Types > C/C++. This gives reasonable folding and indentation of d8m code. 

I suppose there are two sub-topics: debugging d8m programs versus the compiler. 

### Debugging d8m programs

Like other programming languages, debugging d8m programs happens in stages. First, you get rid of errors reported by the parser and typechecker. As noted elsewhere, this is a one at a time process with the d8m compiler, but that doesn't really slow anything down. Next, you deal with runtime errors. 

Most of the time, I debug runtime problems with print statements and logging. It can be useful to have the generated .go file for reference, and even to insert experimental code directly. For this purpose, the `-d` flag of `d8mc` retains the intermediate .go file and formats it with gofmt.

If printing and logging aren't cutting it, the next step is to set up a golang interactive debugger with the .go file. Generally speaking, the generated output is quite readable, though quite a few of the source level variables have been renamed with identifiers that typically start with `__v`. 

As of now, there's no special support for testing d8m programs, though I have ideas. 

### Debugging the compiler

I use `runtests` to detect regressions after compiler changes. The `tester.log` file generates extensive information about what is different, it's reasonably easy to read. 

For more interactive debugging of compiler problems, I run the driver.go version (characterized in the release notes as a "crippled REPL") to see what the compiler is doing with specific code. The code I want to run goes at the beginning of `foo.txt`; the driver reads this file, and processes statements up to the first "call" to `compile`. (Unlike any other driver file, you need to explicitly call compile with driver.go.) 

I set breakpoints in relevant places and set it running in the debug environment of my IDE. Also, driver.go enables writing of the debug.log file which includes extensive information about what the optimizer is doing. (Possibly too extensive. With a large compile, this file can exceed 1M lines. When debugging an error in a large program, I try to extract a short example that will trigger the bug; this greatly reduces the amount of log code I need to slog through.)

It's also possible to generate the `debug.log` file with `d8mc`. Run 

    d8mc -h
for details.

I occasionally use delve for debugging, but most commonly debug with logger and print statements. When using delve, I very commonly add code to the compiler with a `fmt.Print()` line conditioned on whatever precise condition I'm interested in. This causes the entire compiler to be recompiled, but that only adds a couple seconds to the debug loop; it's a whole lot better than single stepping. 
