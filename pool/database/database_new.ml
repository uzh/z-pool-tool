module Label = Entity.Label
module Labels = CCSet.Make (Label)
module Log = Service.Logs

(* Requests are named through here rather than [Caqti_request] directly,
   so the caqti 3.0 move of the legacy constructors into [caqti.classic]
   (already linked) is a change to this alias. *)
module Request = struct
  type ('p, 'r, 'm) t = ('p, 'r, 'm) Caqti_request.t

  module Infix = Caqti_request.Infix
end

type no_transaction
type transaction

type _ txn =
  | Yes : transaction txn
  | No : no_transaction txn

(* Liveness cell. [parent] links a transaction scope to the connection scope it
   was derived from, so closing the connection invalidates both without anyone
   maintaining a child list. *)
type scope =
  { mutable open_ : bool
  ; parent : scope option
  }

let rec live { open_; parent } =
  open_
  &&
  match parent with
  | None -> true
  | Some p -> live p
;;

(* A connection this context holds, and the scope it is valid in. *)
type conn =
  { label : Label.t
  ; connection : Caqti_lwt.connection
  ; tags : Logs.Tag.set option
  ; scope : scope
  }

(* A connection is a pooled resource, so holding one is opt-in: [Label] carries no
   connection and borrows one per statement, which is what [Database] did for
   every statement and costs nothing to construct. [Connection] and
   [TransactionalConnection] hold one for as long as their scope is open. The
   index makes "a transaction implies a held connection" structural — there is no
   [Label] case at [transaction ctx]. *)
type 'txn ctx =
  | Label :
      { label : Label.t
      ; tags : Logs.Tag.set option
      }
      -> no_transaction ctx
  | Connection : conn -> no_transaction ctx
  | TransactionalConnection : conn -> transaction ctx

let label_ctx ?tags label = Label { label; tags }

let label_of_ctx : type x. x ctx -> Label.t = function
  | Label { label; _ } | Connection { label; _ } | TransactionalConnection { label; _ } ->
    label
;;

let tags_of_ctx : type x. x ctx -> Logs.Tag.set option = function
  | Label { tags; _ } | Connection { tags; _ } | TransactionalConnection { tags; _ } ->
    tags
;;

let txn_of_ctx : type x. x ctx -> x txn = function
  | Label _ -> No
  | Connection _ -> No
  | TransactionalConnection _ -> Yes
;;

(* A [Label] context holds nothing, so there is nothing for it to outlive. *)
let live_ctx : type x. x ctx -> bool = function
  | Label _ -> true
  | Connection { scope; _ } | TransactionalConnection { scope; _ } -> live scope
;;

exception Expired_ctx of Label.t
exception Nested_transaction of Label.t

let () =
  Printexc.register_printer (function
    | Expired_ctx l ->
      Some
        (Format.asprintf
           "Database: context for %s used after its scope closed"
           (Label.value l))
    | Nested_transaction l ->
      Some
        (Format.asprintf "Database: a transaction is already open on %s" (Label.value l))
    | _ -> None)
;;

(* Lwt sequence-associated storage propagates through binds, so this survives
   any number of intervening [let*]s and function calls within the fibre. *)
let active : Labels.t Lwt.key = Lwt.new_key ()
let active_labels () = CCOption.value (Lwt.get active) ~default:Labels.empty
let is_ambient label = Labels.mem label (active_labels ())
let with_active label = Lwt.with_value active (Some (Labels.add label (active_labels ())))

(* Soft failure: reaching for a *fresh* connection while a transaction is open on
   the same label. The statements will run outside that transaction and will not
   see its uncommitted writes — and a self-deadlock if one waits on a row the
   transaction locked. Almost always a context that was not forwarded, so say so.
   Reported where the decision is made, once per call, rather than per statement. *)
let warn_ambient ?tags label =
  if is_ambient label
  then
    Log.warn (fun m ->
      m
        ~tags:(Logger.Tags.extend label tags)
        "Using a fresh connection while a transaction is already open on this label. If \
         this call is inside that transaction, forward the context (?db_ctx) instead — \
         the optional argument was most likely not threaded through.")
;;

let on_held { label; connection; scope; _ } f =
  if not (live scope)
  then Lwt.fail (Expired_ctx label)
  else Pools_new.raise_caqti_error label (f connection)
;;

let query
  : type txn r.
    txn ctx -> (Caqti_lwt.connection -> (r, Caqti_error.t) Lwt_result.t) -> r Lwt.t
  =
  fun ctx f ->
  match ctx with
  | Label { label; _ } ->
    Pools_new.use label (fun connection ->
      Pools_new.raise_caqti_error label (f connection))
  | Connection c -> on_held c f
  | TransactionalConnection c -> on_held c f
;;

let exec ctx request input =
  query ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.exec request input)
;;

let find ctx request input =
  query ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.find request input)
;;

let find_opt ctx request input =
  query ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt request input)
;;

let collect ctx request input =
  query ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list request input)
;;

let populate ctx table columns row_type rows =
  query ctx (fun (module C : Caqti_lwt.CONNECTION) ->
    C.populate ~table ~columns row_type (Caqti_lwt.Stream.of_list rows)
    |> Lwt.map Caqti_error.uncongested)
;;

(* Private: hands out the raw [conn] so the public wrappers decide which context
   to build around it, and each reports an ambient transaction at most once. *)
let borrow ?tags label f =
  Pools_new.use label (fun connection ->
    let scope = { open_ = true; parent = None } in
    Lwt.finalize
      (fun () -> f { label; connection; tags; scope })
      (fun () ->
         scope.open_ <- false;
         Lwt.return_unit))
;;

let start_transaction ctx f =
  let unwind exn =
    Log.err (fun m ->
      m
        ~tags:(Logger.Tags.extend ctx.label ctx.tags)
        "Rolling back transaction: %s"
        (Printexc.to_string exn));
    let%lwt () = Pools_new.rollback ?tags:ctx.tags ctx.label ctx.connection in
    (* Reraise what actually aborted the transaction, not whatever the rollback
       said: the caller needs the real cause. *)
    Lwt.reraise exn
  in
  let run () =
    let scope = { open_ = true; parent = Some ctx.scope } in
    let txn = TransactionalConnection { ctx with scope } in
    Lwt.finalize
      (fun () ->
         with_active ctx.label (fun () ->
           let%lwt () = query txn (fun (module C : Caqti_lwt.CONNECTION) -> C.start ()) in
           Lwt.catch
             (fun () ->
                let%lwt result = f txn in
                let%lwt () =
                  query txn (fun (module C : Caqti_lwt.CONNECTION) -> C.commit ())
                in
                Lwt.return result)
             unwind))
      (fun () ->
         scope.open_ <- false;
         Lwt.return_unit)
  in
  if not (live ctx.scope)
  then Lwt.fail (Expired_ctx ctx.label)
  else if is_ambient ctx.label
  then Lwt.fail (Nested_transaction ctx.label)
  else run ()
;;

let connection_ctx ?tags label f =
  warn_ambient ?tags label;
  borrow ?tags label (fun ctx -> f (Connection ctx))
;;

(* No ambient warning: a second transaction on the same label is a hard error, so
   check before taking a pool slot we are only going to hand straight back. *)
let transaction_ctx ?tags label f =
  if is_ambient label
  then Lwt.fail (Nested_transaction label)
  else borrow ?tags label (fun ctx -> start_transaction ctx f)
;;

let in_transaction : type r. no_transaction ctx -> (transaction ctx -> r Lwt.t) -> r Lwt.t
  = function
  (* Nothing held yet, so this is [transaction_ctx] with the label it carries. *)
  | Label { label; tags } -> transaction_ctx ?tags label
  | Connection ctx -> start_transaction ctx
;;

let join_ctx ctx label f =
  let ctx_label = label_of_ctx ctx in
  if not (Label.equal ctx_label label)
  then
    invalid_arg
      (Format.asprintf
         "Database: context is %s but %s was requested"
         (Label.value ctx_label)
         (Label.value label));
  if live_ctx ctx then f ctx else Lwt.fail (Expired_ctx ctx_label)
;;

(* The callback, not the context, carries the polymorphism. [resolve_ctx] runs it
   against either the caller's context or one it built itself, and those have
   different indices, so an ordinary closure would be pinned to whichever branch
   was inferred first. A record with a universally quantified field keeps the
   callback usable at both without wrapping the context in an existential — the
   callback still receives a plain ['txn ctx], so every statement helper and any
   ['txn ctx]-taking function accepts it unchanged. *)
type 'r callback = { run : 'txn. 'txn ctx -> 'r Lwt.t }

let resolve_ctx ?tags ?db_ctx label (callback : _ callback) =
  match db_ctx with
  | Some ctx -> join_ctx ctx label callback.run
  | None ->
    warn_ambient ?tags label;
    callback.run (label_ctx ?tags label)
;;

let resolve_transaction_ctx ?tags ?db_ctx label f =
  match db_ctx with
  | Some ctx -> join_ctx ctx label f
  | None -> transaction_ctx ?tags label f
;;
