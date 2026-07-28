(** A database context: a borrowed connection plus the label it came from, indexed
    by whether a transaction is open on it.

    In {!Database} every statement took an [Entity.Label.t] and borrowed its own
    connection, so a unit of work spanning several statements either paid a pool
    round-trip per statement or dropped down to [Database.transaction] and a raw
    [Caqti_lwt.connection]. Passing the connection explicitly buys three things the
    label could not:

    - nesting a transaction is a type error, because {!in_transaction} only
      accepts a [no_transaction ctx];
    - a context used after its scope closed raises {!Expired_ctx} instead of
      running on a connection somebody else now holds;
    - forgetting to thread [?db_ctx] is reported, instead of quietly opening a
      second transaction that cannot see the first and may deadlock against it.

    Failures are {!Database_error.Failed}. *)

(** {1 Contexts} *)

(** Phantom index signalling that no transaction is open on the context. *)
type no_transaction

(** Phantom index signalling that a transaction is open on the context. *)
type transaction

(** ['txn ctx] is a connection borrowed from the pool for [label], where ['txn]
    records whether a transaction was started on it. See {!connection_ctx},
    {!transaction_ctx} and {!in_transaction}. *)
type 'txn ctx

(** Runtime witness for the index. Matching on it refines the context:
    {[
    match Database.txn_of_ctx ctx with
    | Yes -> Repo.create_user u ctx (* ctx : transaction ctx here *)
    | No -> Lwt.return_error `Needs_transaction
    ]} *)
type _ txn =
  | Yes : transaction txn
  | No : no_transaction txn

val label_of_ctx : _ ctx -> Entity.Label.t
val txn_of_ctx : 'txn ctx -> 'txn txn

(** Raised when a context is used after the scope that created it has closed —
    i.e. someone let it escape its callback. *)
exception Expired_ctx of Entity.Label.t

(** Raised when a transaction is opened on a label that already has one open on
    the current Lwt fibre. *)
exception Nested_transaction of Entity.Label.t

(** {1 Requests}

    Requests are named through here rather than [Caqti_request] directly, so that
    the caqti 3.0 move of the legacy constructors into [caqti.classic] is a
    change to this alias. *)
module Request : sig
  type ('p, 'r, 'm) t = ('p, 'r, 'm) Caqti_request.t

  module Infix = Caqti_request.Infix
end

(** {1 Statements}

    raise {!Database_error.Failed} on error. *)

val exec : _ ctx -> ('p, unit, [< `Zero ]) Request.t -> 'p -> unit Lwt.t
val find : _ ctx -> ('p, 'r, [< `One ]) Request.t -> 'p -> 'r Lwt.t
val find_opt : _ ctx -> ('p, 'r, [< `One | `Zero ]) Request.t -> 'p -> 'r option Lwt.t

val collect
  :  _ ctx
  -> ('p, 'r, [< `Many | `One | `Zero ]) Request.t
  -> 'p
  -> 'r list Lwt.t

val populate
  :  _ ctx
  -> string (** table *)
  -> string list (** columns *)
  -> 'r Caqti_type.t
  -> 'r list
  -> unit Lwt.t

(** Escape hatch for what the above cannot express — driver specifics,
    [SET FOREIGN_KEY_CHECKS] around a truncate. Scope-guarded and failure-routed
    like the rest; reach for it only when nothing else fits. *)
val query
  :  _ ctx
  -> (Caqti_lwt.connection -> ('r, Caqti_error.t) Lwt_result.t)
  -> 'r Lwt.t

(** {1 Scopes}

    The context passed to a callback is invalidated when that callback resolves.
    Using it afterwards raises {!Expired_ctx}. *)

(** Borrow a pooled connection. No transaction: each statement autocommits. *)
val connection_ctx
  :  ?tags:Logs.Tag.set
  -> Entity.Label.t
  -> (no_transaction ctx -> 'r Lwt.t)
  -> 'r Lwt.t

(** Borrow a connection and wrap the callback in BEGIN/COMMIT. Rolls back and
    reraises on failure. *)
val transaction_ctx
  :  ?tags:Logs.Tag.set
  -> Entity.Label.t
  -> (transaction ctx -> 'r Lwt.t)
  -> 'r Lwt.t

(** Open a transaction on a connection you already hold, instead of borrowing a
    second one. Takes a [no_transaction ctx], so nesting is a type error; the
    dynamic check catches the cases types can't see. *)
val in_transaction
  :  no_transaction ctx
  -> (transaction ctx -> 'r Lwt.t)
  -> 'r Lwt.t

(** Run on an existing context after checking it belongs to the given label.
    Raises [Invalid_argument] on mismatch. Index-preserving: joining a
    transaction gives back a [transaction ctx]. *)
val join_ctx : 'txn ctx -> Entity.Label.t -> ('txn ctx -> 'r Lwt.t) -> 'r Lwt.t

(** {1 Forwarding an optional context}

    For functions that take a [?db_ctx] and have to work whether or not the caller
    forwarded one — the common shape once a context reaches into a call tree. *)

(** A callback that works on a context of either index. {!resolve_ctx} needs one
    because it cannot know which index it will run against — the caller's, or
    that of a connection it borrowed itself — and an ordinary closure would be
    pinned to one of them. The polymorphic field carries that instead of an
    existential around the context, so [ctx] in the callback body is a plain
    ['txn ctx] and every helper above accepts it as is. *)
type 'r callback = { run : 'txn. 'txn ctx -> 'r Lwt.t }

(** Reuse [db_ctx] if given, otherwise borrow a connection. Takes a context of
    either index, so a caller inside a transaction can forward it to a read-only
    function without that function having to care:
    {[
    let create ?db_ctx pool language =
      resolve_ctx ?db_ctx pool { run = (fun ctx ->
        let%lwt template = find_template ctx language in
        …) }
    ]} *)
val resolve_ctx
  :  ?tags:Logs.Tag.set
  -> ?db_ctx:'txn ctx
  -> Entity.Label.t
  -> 'r callback
  -> 'r Lwt.t

(** Reuse [db_ctx] if given, otherwise open a transaction. Takes a
    [transaction ctx] rather than a context of either index: the caller asked for
    atomicity, and a [no_transaction ctx] cannot provide it. The index is fixed,
    so this takes an ordinary closure rather than a {!callback}. *)
val resolve_transaction_ctx
  :  ?tags:Logs.Tag.set
  -> ?db_ctx:transaction ctx
  -> Entity.Label.t
  -> (transaction ctx -> 'r Lwt.t)
  -> 'r Lwt.t

(** {1 Value-level scope selection} *)

type _ source =
  | Direct : no_transaction source
  | Transaction : transaction source
  | Join : transaction ctx -> transaction source

(** Dispatches to the scope helpers. When given a fresh source ([Direct] or
    [Transaction]) while a transaction is already open on the label, this logs an
    actionable message naming [?db_ctx] before proceeding — that combination
    almost always means an optional context wasn't forwarded. *)
val source_ctx
  :  ?tags:Logs.Tag.set
  -> 'txn source
  -> Entity.Label.t
  -> ('txn ctx -> 'r Lwt.t)
  -> 'r Lwt.t
