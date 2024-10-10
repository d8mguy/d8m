# D8m: A New Programming Language

## Generalities

D8m is a programming language developed as a personal project by Curtis Abbott (a.k.a. "me"). I'm releasing code and documentation for the language in this repository; in several nearby repositories you can find some things I've developed using d8m. I'm doing this because
1. I think some of the ideas in the language are worth sharing
2. I've started to feel like I am quite productive in this language, which suggests that the compiler and related infrastructure are mature enough to share.

The code I'm releasing here is definitely not a production quality compiler. It compiles lots of code without errors but plenty of known and unknown bugs remain, and it's not hardened against abusive code. I'm productive in it because I know my way around it, including areas of weakness, workarounds for bugs and missing features, etc. The release notes contain a list of these but let's face it, no list of workarounds is as good as intimate familiarity. 

Although d8m was originally conceived with an eye to modeling, manipulating, and visualizing all sorts of data, I don't currently see it as a "niche" language. Instead, I see it as as a general purpose language that lets you express a lot of useful patterns both concisely and clearly, and strikes a good balance around static and dynamic typechecking. If you get deep into the specification document, you'll find some things related to data modeling that aren't implemented in the released version &mdash; I left them in the spec because I think they're interesting and I hope to get them into this compiler at some point. The implemented aspects of d8m I think are worth sharing include

- the generic type system is damn good
- the features for functional operations on collections are concise and powerful
- the language encourages you to work with state, but restricts how you do it, with an eye to avoiding the pitfalls that state can introduce 
-  you can automatically rewrite abstract types into concrete ones &mdash; you __annotate__ your program with your choices, so it's trivial to try different options
- there is a powerful rewrite rule feature to augment the optimization phase, allowing programmers to deeply affect how it works 

The d8m compiler is written in Go and compiles to Go. It uses Go packages for all i/o and other interactions with the outside world. Like Go, d8m typechecking occurs mainly at compile time (a.k.a. _static typechecking_). Its handling of dynamic typechecking is different from Go's but not that different. 

There are also some big differences from Go. D8m has a "higher level" feel. Go's packages let you create modular structures with precompiled parts that can be assembled quickly. D8m says let's recompile everything every time. I claim that with today's processing speeds, this is the right approach. It's possible that I'm wrong at really huge scales. But none of the d8m programs I've written takes more than a couple of seconds to compile, including programs that generate non-trivial websites. And the d8m compiler, written as a single package (of ~30K lines of Go code) compiles in a couple of seconds as well. Compiling everything every time has real advantages for optimizing programs, and opens new possibilities for IDE's. Pre-compiled packages link used and unused code into executables, potentially diluting code space. Compiling everything gives the compiler maximum information about how functions and other entities are used, which maximizes the possibilities for inlining and other optimizations. 

I feel that d8m code performance at this release is generally on a par with Go or python. I think it has the potential to be a more productive language than either for coding and easier to get high performance. 

## This Repository

Here you'll find the source code for the d8m compiler, the "official" modules that go with it, documentation, a set of compiler tests, and miscellaneous files for getting everything set up properly. Both the modules and tests contain significant amounts of d8m code. The tests consist of ~90 d8m programs of varying lengths; the test driver compiles these and compares the output from running them to an expected output. It speeds up the process by first comparing the generated Go code for each test with the expected Go code, calling the Go compiler only if the generated code doesn't match. Thus, each test provides 3 files, with extensions `.d8m`, `.go`, and `.out`. The full suite runs in a couple seconds if all the tests match their `.go` files, longer if not. The suite tests all of the modules. Running it compiles ~30K lines of d8m code in all. 

Documentation consists of a specification, a "brief" introduction with examples, release notes, a few other bits of advice, and modules documentation. 


## Available D8m Code

Three additional repositories, released now or soon, contain two non-trivial d8m programs I've written, and the source code for a website that shows off what they do. One of the programs generates websites from a specification using React for the front end. I have developed a UI for very flexible generation of charts from data with this framework. The other generates performance measurements on d8m programs and organizes them into a collection of csv and json files. The website shows off various performance aspects of d8m (and python and Go) with charts, and lets you play with the chart generating UI.

The website generation program may be mature enough to get useful results quickly, and could be especially useful if your application requires charting. Taken altogether, these programs along with the available modules and tests provide a lot of example d8m code. 

## Acknowledgements

Inventing one's own language and writing a compiler for it means depending less than most on existing technology. But everybody depends on existing technology. This d8m compiler gets to focus on the high level stuff and let the Go compiler focus on the lower level stuff because it "only" compiles to Go. Go is an excellent language, with an amazingly fast compiler, and a bunch of well engineered infrastructure. No wonder it's as successful as it is. 

Elsewhere in these repositories, I use numerous software packages for website construction. React helps a lot; `recharts.js` is an excellent charting package. Pandoc is another great program that's part of the flow.  
