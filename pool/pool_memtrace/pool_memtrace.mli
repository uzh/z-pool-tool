(** Writes a memory trace to the file in [MEMTRACE] (sampling rate:
    [MEMTRACE_RATE]), view it with [memtrace_viewer] or summarise it with
    [scripts/trace_report]. ["%p"] in the file name is replaced by the pid,
    so concurrent processes (server, worker, test executables) get a file each.
    Relative paths are resolved against the repository ([ROOT_PATH]), missing
    directories are created, and the file name is printed on stderr.

    Never fails the process: if the file can't be opened or is locked by
    another process, tracing is skipped with a warning on stderr. While
    tracing, SIGINT and SIGTERM exit through [at_exit], so the trace is
    flushed (and sanitizers get to print their exit reports). *)
val start : context:string -> unit
