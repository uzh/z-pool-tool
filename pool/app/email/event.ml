open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Entity
module User = Pool_user

let get_or_failwith = Pool_common.Utils.get_or_failwith

type verification_event =
  | Created of Pool_user.EmailAddress.t * Token.t * Pool_user.Id.t
  | EmailVerified of unverified t

let verification_event_name = function
  | Created _ -> "Created"
  | EmailVerified _ -> "EmailVerified"
;;

let handle_verification_event pool : verification_event -> unit Lwt.t = function
  | Created (address, token, user_id) ->
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    let%lwt user = User.find_exn pool user_id in
    let unverified_email = create address user token in
    Repo.insert pool unverified_email
  | EmailVerified (Unverified { token; _ } as email) ->
    let%lwt () = Pool_token.deactivate pool token in
    let%lwt () = Repo.verify pool @@ verify email in
    Lwt.return_unit
;;

let[@warning "-4"] equal_verification_event
  (one : verification_event)
  (two : verification_event)
  : bool
  =
  match one, two with
  | Created (e1, t1, id1), Created (e2, t2, id2) ->
    User.EmailAddress.equal e1 e2
    && Token.equal t1 t2
    && Pool_user.Id.equal id1 id2
  | EmailVerified m, EmailVerified p -> equal m p
  | _ -> false
;;

let pp_verification_event formatter (event : verification_event) : unit =
  let pp_address = User.EmailAddress.pp formatter in
  match event with
  | Created (m, t, id) ->
    pp_address m;
    Token.pp Format.std_formatter t;
    Pool_user.Id.pp formatter id
  | EmailVerified m -> pp formatter m
;;

type dispatch =
  { job : Job.t
  ; id : Pool_queue.Id.t option [@yojson.option]
  ; message_template : string option [@yojson.option]
  ; job_ctx : Pool_queue.job_ctx option [@yojson.option]
  }
[@@deriving eq, fields, show, yojson]

let create_dispatch ?id ?message_template ?job_ctx job =
  { job; id; message_template; job_ctx }
;;

type event =
  | Sent of (dispatch * User.EmailAddress.t option * SmtpAuth.Id.t option)
  | BulkSent of dispatch list
  | SmtpCreated of SmtpAuth.Write.t
  | SmtpEdited of SmtpAuth.t
  | SmtpDeleted of SmtpAuth.Id.t
  | SmtpPasswordEdited of SmtpAuth.update_password
[@@deriving eq, show, variants]

let sent ?new_email_address ?new_smtp_auth_id job =
  Sent (job, new_email_address, new_smtp_auth_id)
;;

let create_sent
  ?id
  ?message_template
  ?job_ctx
  ?new_email_address
  ?new_smtp_auth_id
  job
  =
  create_dispatch ?id ?message_template ?job_ctx job
  |> sent ?new_email_address ?new_smtp_auth_id
;;

let handle_event pool : event -> unit Lwt.t = function
  | Sent
      ( { job; id; message_template; job_ctx }
      , new_email_address
      , new_smtp_auth_id ) ->
    Email_service.dispatch
      ?id
      ?new_email_address
      ?new_smtp_auth_id
      ?message_template
      ?job_ctx
      pool
      job
  | BulkSent jobs ->
    let jobs =
      CCList.map
        (fun { job; id; message_template; job_ctx } ->
          ( CCOption.get_or ~default:(Pool_queue.Id.create ()) id
          , job
          , message_template
          , job_ctx ))
        jobs
    in
    Email_service.dispatch_all pool jobs
  | SmtpCreated ({ SmtpAuth.Write.id; _ } as created) ->
    let open Utils.Lwt_result.Infix in
    let ctx = Database.to_ctx pool in
    let%lwt () = Repo.Smtp.insert pool created in
    let%lwt () =
      Repo.Smtp.find pool id
      >>= Entity_guard.SmtpTarget.to_authorizable ~ctx
      ||> get_or_failwith
      ||> fun (_ : Guard.Target.t) -> ()
    in
    Lwt.return_unit
  | SmtpEdited updated ->
    let%lwt () = Repo.Smtp.update pool updated in
    Lwt.return_unit
  | SmtpPasswordEdited updated_password ->
    let%lwt () = Repo.Smtp.update_password pool updated_password in
    Lwt.return_unit
  | SmtpDeleted id ->
    let%lwt () = Repo.Smtp.delete pool id in
    Lwt.return_unit
;;
