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

type 'txn ctx =
  { label : Label.t
  ; connection : Caqti_lwt.connection
  ; txn : 'txn txn
  ; tags : Logs.Tag.set option
  ; scope : scope
  }

let label_of_ctx (ctx : _ ctx) = ctx.label
let txn_of_ctx (ctx : _ ctx) = ctx.txn

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

(* Soft failure: borrowing a *second* connection while a transaction is open.
   Not a nested BEGIN, but two pool slots for one unit of work, split across two
   transactions that cannot see each other — and a self-deadlock if the inner one
   waits on a row the outer one locked. Almost always a context that was not
   forwarded, so say so. *)
let warn_ambient ?tags label =
  if is_ambient label
  then
    Log.warn (fun m ->
      m
        ~tags:(Logger.Tags.extend label tags)
        "Opening a new scope while a transaction is already open. If this call is inside \
         that transaction, forward the context (?db_ctx / Join ctx) instead — the \
         optional argument was most likely not threaded through.")
;;

(* Add scope guard and the failure routing *)
let query ctx f =
  if not (live ctx.scope)
  then Lwt.fail (Expired_ctx ctx.label)
  else Pools_new.raise_caqti_error ctx.label (f ctx.connection)
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

(* Private: no ambient warning, so the public wrappers can each report once. *)
let borrow ?tags label f =
  Pools_new.use label (fun connection ->
    let scope = { open_ = true; parent = None } in
    let ctx = { label; connection; txn = No; tags; scope } in
    Lwt.finalize
      (fun () -> f ctx)
      (fun () ->
         scope.open_ <- false;
         Lwt.return_unit))
;;

let in_transaction (ctx : no_transaction ctx) f =
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
    let txn = { ctx with txn = Yes; scope } in
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
  borrow ?tags label f
;;

let transaction_ctx ?tags label f =
  warn_ambient ?tags label;
  borrow ?tags label (fun ctx -> in_transaction ctx f)
;;

let join_ctx (ctx : _ ctx) label f =
  if not (Label.equal ctx.label label)
  then
    invalid_arg
      (Format.asprintf
         "Database: context is %s but %s was requested"
         (Label.value ctx.label)
         (Label.value label));
  if live ctx.scope then f ctx else Lwt.fail (Expired_ctx ctx.label)
;;

(* The callback, not the context, carries the polymorphism. [resolve_ctx] runs it
   against either the caller's context or one it borrowed itself, and those have
   different indices, so an ordinary closure would be pinned to whichever branch
   was inferred first. A record with a universally quantified field keeps the
   callback usable at both without wrapping the context in an existential — the
   callback still receives a plain ['txn ctx], so every statement helper and any
   ['txn ctx]-taking function accepts it unchanged. *)
type 'r callback = { run : 'txn. 'txn ctx -> 'r Lwt.t }

let resolve_ctx ?tags ?db_ctx label (callback : _ callback) =
  match db_ctx with
  | Some ctx -> join_ctx ctx label callback.run
  | None -> connection_ctx ?tags label callback.run
;;

let resolve_transaction_ctx ?tags ?db_ctx label f =
  match db_ctx with
  | Some ctx -> join_ctx ctx label f
  | None -> transaction_ctx ?tags label f
;;

type _ source =
  | Direct : no_transaction source
  | Transaction : transaction source
  | Join : transaction ctx -> transaction source

let source_ctx
  : type x r. ?tags:Logs.Tag.set -> x source -> Label.t -> (x ctx -> r Lwt.t) -> r Lwt.t
  =
  fun ?tags source label f ->
  match source with
  | Join ctx -> join_ctx ctx label f
  | Direct -> connection_ctx ?tags label f
  | Transaction -> transaction_ctx ?tags label f
;;
