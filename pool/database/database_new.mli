(** A database context: a borrowed connection plus the label it came from, indexed
    by whether a transaction is open on it.

    In {!Database} every statement took an [Entity.Label.t] and borrowed its own
    connection, so a unit of work spanning several statements either paid a pool
    round-trip per statement or dropped down to [Database.transaction] and a raw
    [Caqti_lwt.connection]. Passing the connection explicitly buys three things the
    label could not:

    - nesting a transaction is a type error, because {!in_transaction} only
      accepts a {!direct_ctx};
    - a context used after its scope closed raises {!Expired_context} instead of
      running on a connection somebody else now holds;
    - forgetting to thread [?db_ctx] is reported, instead of quietly opening a
      second transaction that cannot see the first and may deadlock against it.

    Failures are {!Database_error.Failed}. *)

(** {1 Contexts} *)

type not_a_transaction
type transaction
type 'txn t
type txn_ctx = transaction t
type direct_ctx = not_a_transaction t

(** Runtime witness for the index. Matching on it refines the context:
    {[
    match Database.txn ctx with
    | Yes -> Repo.create_user u ctx (* ctx : txn_ctx here *)
    | No -> Lwt.return_error `Needs_transaction
    ]} *)
type _ txn =
  | Yes : transaction txn
  | No : not_a_transaction txn

val label : _ t -> Entity.Label.t
val txn : 'x t -> 'x txn

(** Raised when a context is used after the scope that created it has closed —
    i.e. someone let it escape its callback. *)
exception Expired_context of Entity.Label.t

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

val exec : _ t -> ('p, unit, [< `Zero ]) Request.t -> 'p -> unit Lwt.t
val find : _ t -> ('p, 'r, [< `One ]) Request.t -> 'p -> 'r Lwt.t
val find_opt : _ t -> ('p, 'r, [< `One | `Zero ]) Request.t -> 'p -> 'r option Lwt.t
val collect : _ t -> ('p, 'r, [< `Many | `One | `Zero ]) Request.t -> 'p -> 'r list Lwt.t

val populate
  :  _ t
  -> string (** table *)
  -> string list (** columns *)
  -> 'r Caqti_type.t
  -> 'r list
  -> unit Lwt.t

(** Escape hatch for what the above cannot express — driver specifics,
    [SET FOREIGN_KEY_CHECKS] around a truncate. Scope-guarded and failure-routed
    like the rest; reach for it only when nothing else fits. *)
val with_raw
  :  _ t
  -> ((module Caqti_lwt.CONNECTION) -> ('r, Caqti_error.t) Lwt_result.t)
  -> 'r Lwt.t

(** {1 Scopes}

    The context passed to a callback is invalidated when that callback resolves.
    Using it afterwards raises {!Expired_context}. *)

(** Borrow a pooled connection. No transaction: each statement autocommits. *)
val with_connection
  :  ?tags:Logs.Tag.set
  -> Entity.Label.t
  -> (direct_ctx -> 'r Lwt.t)
  -> 'r Lwt.t

(** Borrow a connection and wrap the callback in BEGIN/COMMIT. Rolls back and
    reraises on failure. *)
val with_transaction
  :  ?tags:Logs.Tag.set
  -> Entity.Label.t
  -> (txn_ctx -> 'r Lwt.t)
  -> 'r Lwt.t

(** Open a transaction on a connection you already hold, instead of borrowing a
    second one. Takes [direct_ctx], so nesting is a type error; the dynamic check
    catches the cases types can't see. *)
val in_transaction : direct_ctx -> (txn_ctx -> 'r Lwt.t) -> 'r Lwt.t

(** Run on an existing context after checking it belongs to [label]. Raises
    [Invalid_argument] on mismatch. Index-preserving: joining a transaction gives
    back a {!txn_ctx}. *)
val join : 'a t -> Entity.Label.t -> ('a t -> 'r Lwt.t) -> 'r Lwt.t

(** {1 Forwarding an optional context}

    For functions that take a [?db_ctx] and have to work whether or not the caller
    forwarded one — the common shape once a context reaches into a call tree. *)

(** A context whose index is hidden — enough to run statements, not enough to
    start a transaction. {!join_or_connect} passes one of these because it cannot
    know which index it will have: the caller's, or that of a connection it
    borrowed itself. Unpack it in the callback with [fun (Any ctx) -> …]; the
    statement helpers take [_ t], so [ctx] then works unchanged. *)
type any = Any : _ t -> any

(** Reuse [db_ctx] if given, otherwise borrow a connection. Takes a context of
    either index, so a caller inside a transaction can forward it to a read-only
    function without that function having to care:
    {[
    let create ?db_ctx pool language =
      join_or_connect ?db_ctx pool (fun (Any ctx) ->
        let%lwt template = find_template ctx language in
        …)
    ]} *)
val join_or_connect
  :  ?tags:Logs.Tag.set
  -> ?db_ctx:'a t
  -> Entity.Label.t
  -> (any -> 'r Lwt.t)
  -> 'r Lwt.t

(** Reuse [db_ctx] if given, otherwise open a transaction. Takes {!txn_ctx}
    rather than any context: the caller asked for atomicity, and a [direct_ctx]
    cannot provide it. *)
val join_or_transaction
  :  ?tags:Logs.Tag.set
  -> ?db_ctx:txn_ctx
  -> Entity.Label.t
  -> (txn_ctx -> 'r Lwt.t)
  -> 'r Lwt.t

(** {1 Value-level scope selection} *)

type _ source =
  | Direct : not_a_transaction source
  | Transaction : transaction source
  | Join : txn_ctx -> transaction source

(** Dispatches to the scope helpers. When given a fresh source ([Direct] or
    [Transaction]) while a transaction is already open on [label], this logs an
    actionable message naming [?db_ctx] before proceeding — that combination
    almost always means an optional context wasn't forwarded. *)
val resolve
  :  ?tags:Logs.Tag.set
  -> 'x source
  -> Entity.Label.t
  -> ('x t -> 'r Lwt.t)
  -> 'r Lwt.t
