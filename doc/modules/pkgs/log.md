## Log

This package module provides access to go's `log` package. 

#### type Logger

    val Logger = extend tuple(...) where {
        method Fatal = \(v:...any) -> nothing {...}
        method Panic = \(v:...any) -> nothing {...}
        method Print = \(v:...any) -> nothing {...}
        method Println = \(v:...any) -> nothing {...}
        method Printf = \(fmt:string, v:...any) -> nothing {...}
    }
See the go package documentation for details.

#### function New

    val New = \(out:io.Writer, prefix:string, flag:integer) -> Logger
Returns a new `Logger`. See the go package documentation for details. A convenient way to use this is

    import go "log"
    ...
    var dbgfile = os.Create("dbg.log")
    val logger = log.New(dbgfile.ok, "", 0)

#### function Default

    val Default = \() -> Logger

Returns the standard logger used by the package-level output functions

