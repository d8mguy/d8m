## Profile

Provides access to go's profiling tools. See documentation on go profiling for details on this glue code.

#### function profileCPU

    val profileCPU = \(fn: string) -> nothing {...}

#### function memSnapshot

    val memSnapshot = \imp(fn: string) -> nothing {...}
I find the go pkg name misleading, or at least, not informative. Hence, a different name for the d8m module. This runs GC and then writes a profile of the heap. It creates and closes the file; for now there is no way to add to a single file, and at the moment of writing, I'm not sure what it would mean to do so.