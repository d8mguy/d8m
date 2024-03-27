## File

This module provides access to file oriented functions from Go's `os` package, plus other useful types and functions for working with files. It defines two interfaces to Go's os.File type, called `basicfile` and `streamfile`. A `basicfile` simply stores the internal file descriptor and filename; a `streamfile` is read when opened.   

#### type basicfile

    val basicfile: type = extend tuple(name: string, fd: os.File) where {
        method read = \mod(buff: list(byte)) -> integer {...}
        method write = \mod(buff: list(byte)) -> nothing {...}
        method close = \mod() -> nothing {...}
        method size = \() -> integer {...}
        method modTime = \() -> time.Time {...}
    }

Method `read`: returns number of bytes read, which is max of `buff` size and bytes remaining to be read in the file.

#### function openBasic

    val openBasic = \(nm: string)->basicfile {...}
`OpenBasic` takes a filename string and attempts to open it, returning a `basicfile` if successful and exiting otherwise.

#### function createBasic

    val createBasic = \imp(nm: string)->basicfile {...}
`CreateBasic` creates a `basicfile` with the given name, returning the `basicfile` if successful and exiting otherwise.

#### function openWithError

    val openWithError = \(nm: string)->ortype(ok: basicfile, err: string) {...}
`OpenWithError` takes a filename string and returns either a `basicfile` for it or an error message string.

#### function createWithError

    val createWithError = \(nm: string)->ortype(ok: basicfile, err: string) {...}
`CreateWithError` takes a filename string and returns either a `basicfile` for creating it or an error message string.

#### type streamfile

    val streamfile: type = tuple(name, contents: list(byte))
`Streamfile` is a slight optimization of `basicfile`; it reads and closes the file upon opening.

#### function openStream

    val openStream = \(nm: string)->streamfile {...}
`OpenStream` takes a filename string and attempts to open it. Exit if not successful, otherwise read and close the file, returning a streamfile.

#### function fileDelete

    val fileDelete = \imp(fname: string) -> nothing {...}
`FileDelete` takes a filename string and deletes it; exiting with the error message if there is an error.

#### function quickCheck

    val quickCheck = \imp(fname: string) -> integer {...}
`QuickCheck` takes a filename string and Stats it; returning -1 if it doesn't exist, 0 if it's a regular file, 1 if a directory

#### function modtime

    val modtime = \(de: os.DirEntry) -> time.Time {...}
`Modtime` extracts the `ModTime` of a `DirEntry`, panicing if the `Info` call fails.

#### function earlier

    val earlier = \(d0, d1: os.DirEntry) -> boolean {...}
Given two `os.DirEntry`, return whether the first is earlier than the second. 

#### type filenameParts

    [filenameParts: nm]
where `nm:list(byte)`. 

    val filenameParts = extend tuple(dir, base, extn: list(byte)) where {
        method out = \() -> string
    }
`FilenameParts` handles a `list(byte)` representing a filename as a triple of the directory, base name, extension. You can assign to different attributes to change them, then extract the full filename with the `out` method. 

