
## Requirements

1. The code here has been tested on Mac OS and only on Mac OS. 
2. You must have Go installed.

Notes:
1. it's possible that everything runs just fine on Linux. I haven't tested it, so I don't know. But AFAIK there is nothing Mac-specific in my use of Go or any unix tool. 
2. I don't think the installed version of Go matters. If it's really old, that might be a problem. I have never seen changes or incompatibilities as a result of updating (or not updating) my version of Go.  (This release is tested on go version 22.1.)
3. Installation dependencies/assumptions are as follows. Installation creates a directory at `~/Library/Application\ Support/D8m`, copies the modules directory there, and installs the compiler binaries in `/usr/local/bin`. It's worth knowing that the "modules DB" mentioned in the spec is a file that lives in the modules directory named `modulesDB.json`. It's an array of objects, initially empty. Other d8m related repositories (webgen, perfsis) add entries to it.

## Steps

1. download or clone this repository
2. run `sudo make install`

This does the following:
1. compile `d8mc` with go build
2. install it in `/usr/local/bin`
3. create a "d8m area" in `~/Library/Application\ Support/D8m`
4. copy `modules/` to the "d8m area"
5. create empty .go files for each of the tests in the tests directory

Other d8m programs also use the "d8m area" as a repository for files other than the executables. For example, both webgen and perfsis use it. D8mc has a special `-addmod` flag to add entries to the `modulesDB.json` file. The install scripts for webgen and perfsis use that.

To run tests, use a different make target:

    make runtests
produces `./runtests` file, then 

    ./runtests -f
should produce a couple pages of console output with the final line

    no errors, writing date to ./.lasttest.txt
This message documents that runtests writes the current date to the named file every time it runs successfully. Unless you run tests constantly, it can be hard to remember when you last ran them, so this date can be useful.

The test "methodology" is to compile and run a program, then compare its output with an expected output. A "caching scheme" speeds up testing in the expected case of success (see below for details). The tests here cover various compiler behaviors and also test most of the modules. There are no tests for proper handling of erroneous inputs, nor are there any unit tests. 

The `-f` flag tells runtests to copy its compiled go code  to the `.go` file for the test if the match fails, instead of reporting an error. In other words, thinking of the `.go` files as caching information about compiler behavior, `-f` says to put the cache in update mode instead of read-only mode. In normal development, you use `-f` when compiler changes affect the compiled .go code for a test but still produce correct output. This isn't automatic because in development mode, you want to verify not only the correctness of the compiled binary but also whether any compiler output changes are problematic.

You need to run `runtests` the first time with `-f` because the .go files in the repository are initially empty &mdash; they aren't managed under git (and shouldn't be added). They should be thought of as the contents of a cache. Thus, the first run fills in the .go files.

Runtests normally runs in a few seconds, and produces a `tester.log` file plus one line per test on stdout (more if the test fails). If the test fails, you'll find useful details of what happened in `tester.log`. 

Running the tests after installing is a good way to verify that your installation is good.
