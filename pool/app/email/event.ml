open Entity
module User = Pool_user

type confirmation_email =
  { subject : I18n.Content.t
  ; text : I18n.Content.t
  ; language : Pool_common.Language.t
  ; session_text : string
  }
[@@deriving eq, show]

let sender_of_pool pool =
  let open Utils.Lwt_result.Infix in
  if Pool_database.is_root pool
  then Lwt.return (Sihl.Configuration.read_string "SMTP_SENDER")
  else (
    let%lwt sender_email =
      Settings.find_contact_email pool ||> Settings.ContactEmail.value
    in
    let%lwt tenant = Pool_tenant.find_by_label pool ||> CCResult.get_exn in
    Format.sprintf
      "%s <%s>"
      (Pool_tenant.Title.value tenant.Pool_tenant.title)
      sender_email
    |> CCOption.pure
    |> Lwt.return)
;;

let deactivate_token pool token =
  Service.Token.deactivate ~ctx:(Pool_tenant.to_ctx pool) token
;;

type verification_event =
  | Created of Pool_user.EmailAddress.t * Token.t * Pool_common.Id.t
  | EmailVerified of unverified t

let verification_event_name = function
  | Created _ -> "Created"
  | EmailVerified _ -> "EmailVerified"
;;

let handle_verification_event pool : verification_event -> unit Lwt.t = function
  | Created (address, token, user_id) ->
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    let%lwt user =
      Service.User.find
        ~ctx:(Pool_tenant.to_ctx pool)
        (Pool_common.Id.value user_id)
    in
    let unverified_email = create address user token in
    Repo.insert pool unverified_email
  | EmailVerified (Unverified { token; _ } as email) ->
    let%lwt () = deactivate_token pool token in
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
    && Pool_common.Id.equal id1 id2
  | EmailVerified m, EmailVerified p -> equal m p
  | _ -> false
;;

let pp_verification_event formatter (event : verification_event) : unit =
  let pp_address = User.EmailAddress.pp formatter in
  match event with
  | Created (m, t, id) ->
    pp_address m;
    Token.pp Format.std_formatter t;
    Pool_common.Id.pp formatter id
  | EmailVerified m -> pp formatter m
;;

type event =
  | Sent of Sihl_email.t
  | BulkSent of Sihl_email.t list
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Sent email ->
    let%lwt sender = sender_of_pool pool in
    Pool_tenant.(Service.Email.send ?sender ~ctx:(to_ctx pool) email)
  | BulkSent emails ->
    let%lwt sender = sender_of_pool pool in
    Pool_tenant.(Service.Email.bulk_send ?sender ~ctx:(to_ctx pool) emails)
;;
