(** Borrowing a connection from a pool, and the driver handling that goes with it.

    Layered on the pool cache in {!Pools}: the [Make] functor, the cache, [fetch],
    [connect] and [disconnect] are unchanged and not duplicated here. Two things
    differ from [Pools.raise_caqti_error]:

    - a failure raises {!Database_error.Failed}, which carries the label, instead of
      [Caqti_error.Exn];
    - the pool is left alone. It cannot be drained from here anyway — draining
      waits for every checked-out connection, including the caller's own — and
      caqti's pool already drops a connection that fails its check on release.

    The statement helpers ([query], [exec], [find], [transaction], …) are not
    here: they took a label and borrowed a connection per statement. Running
    statements is {!Database_new}'s job, against a context it already holds. *)

val or_raise : Entity.Label.t -> ('a, Caqti_error.t) Lwt_result.t -> 'a Lwt.t

(** [use label f] borrows a pooled connection for [f] and hands it back when [f]
    resolves, however it resolves. *)
val use : Entity.Label.t -> ((module Caqti_lwt.CONNECTION) -> 'a Lwt.t) -> 'a Lwt.t

(** Roll back, and leave the connection fit for reuse — which on MariaDB means
    undoing the [autocommit = 0] that [start] set and [rollback] does not.

    Failures are logged rather than raised: the caller is already unwinding an
    exception that matters more than this one. *)
val rollback
  :  ?tags:Logs.Tag.set
  -> Entity.Label.t
  -> (module Caqti_lwt.CONNECTION)
  -> unit Lwt.t
