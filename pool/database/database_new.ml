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

type not_a_transaction
type transaction

type _ txn =
  | Yes : transaction txn
  | No : not_a_transaction txn

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

type 'txn t =
  { label : Label.t
  ; connection : (module Caqti_lwt.CONNECTION)
  ; txn : 'txn txn
  ; tags : Logs.Tag.set option
  ; scope : scope
  }

type txn_ctx = transaction t
type direct_ctx = not_a_transaction t

let label (ctx : _ t) = ctx.label
let txn (ctx : _ t) = ctx.txn

exception Expired_context of Label.t
exception Nested_transaction of Label.t

let () =
  Printexc.register_printer (function
    | Expired_context l ->
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
let with_raw ctx f =
  if not (live ctx.scope)
  then Lwt.fail (Expired_context ctx.label)
  else Pools_new.or_raise ctx.label (f ctx.connection)
;;

let exec ctx request input =
  with_raw ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.exec request input)
;;

let find ctx request input =
  with_raw ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.find request input)
;;

let find_opt ctx request input =
  with_raw ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt request input)
;;

let collect ctx request input =
  with_raw ctx (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list request input)
;;

let populate ctx table columns row_type rows =
  with_raw ctx (fun (module C : Caqti_lwt.CONNECTION) ->
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

let in_transaction (ctx : direct_ctx) f =
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
           let%lwt () =
             with_raw txn (fun (module C : Caqti_lwt.CONNECTION) -> C.start ())
           in
           Lwt.catch
             (fun () ->
                let%lwt result = f txn in
                let%lwt () =
                  with_raw txn (fun (module C : Caqti_lwt.CONNECTION) -> C.commit ())
                in
                Lwt.return result)
             unwind))
      (fun () ->
         scope.open_ <- false;
         Lwt.return_unit)
  in
  if not (live ctx.scope)
  then Lwt.fail (Expired_context ctx.label)
  else if is_ambient ctx.label
  then Lwt.fail (Nested_transaction ctx.label)
  else run ()
;;

let with_connection ?tags label f =
  warn_ambient ?tags label;
  borrow ?tags label f
;;

let with_transaction ?tags label f =
  warn_ambient ?tags label;
  borrow ?tags label (fun ctx -> in_transaction ctx f)
;;

let join (ctx : _ t) label f =
  if not (Label.equal ctx.label label)
  then
    invalid_arg
      (Format.asprintf
         "Database: context is %s but %s was requested"
         (Label.value ctx.label)
         (Label.value label));
  if live ctx.scope then f ctx else Lwt.fail (Expired_context ctx.label)
;;

(* Index erased. [join_or_connect] passes the caller's context when there is one
   and a fresh [direct_ctx] when there is not, so a callback taking [_ t] directly
   would be pinned to whichever branch was inferred first. Hiding the index behind
   [Any] lets that callback be an ordinary closure — the statement helpers take
   [_ t], so they accept the unpacked context unchanged. *)
type any = Any : _ t -> any

let join_or_connect ?tags ?db_ctx label f =
  let f ctx = f (Any ctx) in
  match db_ctx with
  | Some ctx -> join ctx label f
  | None -> with_connection ?tags label f
;;

let join_or_transaction ?tags ?db_ctx label f =
  match db_ctx with
  | Some ctx -> join ctx label f
  | None -> with_transaction ?tags label f
;;

type _ source =
  | Direct : not_a_transaction source
  | Transaction : transaction source
  | Join : txn_ctx -> transaction source

let resolve
  : type x r. ?tags:Logs.Tag.set -> x source -> Label.t -> (x t -> r Lwt.t) -> r Lwt.t
  =
  fun ?tags source label f ->
  match source with
  | Join ctx -> join ctx label f
  | Direct -> with_connection ?tags label f
  | Transaction -> with_transaction ?tags label f
;;
