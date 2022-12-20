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
  let%lwt sender_email =
    Settings.find_contact_email pool ||> Settings.ContactEmail.show
  in
  let%lwt tenant = Pool_tenant.find_by_label pool ||> CCResult.get_exn in
  Lwt.return
  @@ Format.sprintf
       "%s <%s>"
       (Pool_tenant.Title.value tenant.Pool_tenant.title)
       sender_email
;;

let send_confirmation
  pool
  layout
  ({ Sihl_user.email; _ } as user)
  { subject; text; language; session_text }
  =
  let%lwt email_template =
    let content =
      Format.asprintf "%s\n%s" (text |> I18n.Content.value) session_text
    in
    Helper.prepare_email
      pool
      language
      TemplateLabel.Boilerplate
      (subject |> I18n.Content.value)
      email
      layout
      [ ( "name"
        , CCString.concat
            " "
            [ user |> User.user_firstname |> User.Firstname.value
            ; user |> User.user_lastname |> User.Lastname.value
            ] )
      ; "content", content
      ]
  in
  let%lwt sender = sender_of_pool pool in
  email_template |> Service.Email.send ~sender ~ctx:(Pool_tenant.to_ctx pool)
;;

let create_token pool address =
  let open Utils.Lwt_result.Infix in
  Service.Token.create
    ~ctx:(Pool_tenant.to_ctx pool)
    [ "email", User.EmailAddress.value address ]
  ||> Token.create
;;

let deactivate_token pool token =
  Service.Token.deactivate ~ctx:(Pool_tenant.to_ctx pool) token
;;

let send_confirmation_email pool language tenant email firstname lastname event =
  let open Utils.Lwt_result.Infix in
  let%lwt sender = sender_of_pool pool in
  Helper.ConfirmationEmail.create
    pool
    language
    tenant
    email
    firstname
    lastname
    event
  >|> Service.Email.send ~sender ~ctx:(Pool_tenant.to_ctx pool)
;;

type verification_event =
  | Created of
      User.EmailAddress.t
      * Pool_common.Id.t
      * User.Firstname.t
      * User.Lastname.t
      * Pool_common.Language.t
      * email_layout
  | Updated of
      User.EmailAddress.t * Sihl_user.t * Pool_common.Language.t * email_layout
  | EmailVerified of unverified t

let verification_event_name = function
  | Created _ -> "Created"
  | Updated _ -> "Updated"
  | EmailVerified _ -> "EmailVerified"
;;

let handle_verification_event pool : verification_event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_tenant.to_ctx pool in
  let create_email language layout user_id address firstname lastname label
    : unit Lwt.t
    =
    let%lwt token = create_token pool address in
    user_id
    |> Pool_common.Id.value
    |> Service.User.find_opt ~ctx
    >|> function
    | None ->
      let error = PoolError.(NotFound Field.User |> show_error) in
      Logs.err (fun m -> m "Cannot create verification email: %s" error);
      failwith error
    | Some user ->
      let email = create address user token in
      let%lwt () = Repo.insert pool email in
      send_confirmation_email
        pool
        language
        layout
        email
        firstname
        lastname
        label
  in
  function
  | Created (address, user_id, firstname, lastname, language, layout) ->
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    create_email
      language
      layout
      user_id
      address
      (Some firstname)
      (Some lastname)
      TemplateLabel.SignUpVerification
  | Updated (address, user, language, layout) ->
    let open Sihl_user in
    let user_id = user.id |> Pool_common.Id.of_string in
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    create_email
      language
      layout
      user_id
      address
      (user.given_name |> CCOption.map Pool_user.Firstname.of_string)
      (user.name |> CCOption.map Pool_user.Lastname.of_string)
      TemplateLabel.EmailVerification
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
  | ( Created (a1, id1, f1, l1, lang1, layout1)
    , Created (a2, id2, f2, l2, lang2, layout2) ) ->
    User.EmailAddress.equal a1 a2
    && Pool_common.Id.equal id1 id2
    && User.Firstname.equal f1 f2
    && User.Lastname.equal l1 l2
    && Pool_common.Language.equal lang1 lang2
    && equal_email_layout layout1 layout2
  | Updated (a1, u1, lang1, layout1), Updated (a2, u2, lang2, layout2) ->
    User.EmailAddress.equal a1 a2
    && CCString.equal u1.Sihl_user.id u2.Sihl_user.id
    && Pool_common.Language.equal lang1 lang2
    && equal_email_layout layout1 layout2
  | EmailVerified m, EmailVerified p -> equal m p
  | _ -> false
;;

let pp_verification_event formatter (event : verification_event) : unit =
  let pp_address = User.EmailAddress.pp formatter in
  match event with
  | Created (m, id, f, l, language, layout) ->
    pp_address m;
    Pool_common.Id.pp formatter id;
    User.Firstname.pp formatter f;
    User.Lastname.pp formatter l;
    Pool_common.Language.pp formatter language;
    pp_email_layout formatter layout
  | Updated (m, u, language, layout) ->
    pp_address m;
    Sihl_user.pp formatter u;
    Pool_common.Language.pp formatter language;
    pp_email_layout formatter layout
  | EmailVerified m -> pp formatter m
;;

type event =
  | Sent of Sihl_email.t
  | BulkSent of Sihl_email.t list
  | ResetPassword of Sihl_user.t * Pool_common.Language.t * email_layout
  | ChangedPassword of Sihl_user.t * Pool_common.Language.t * email_layout
  | AssignmentConfirmationSent of
      Sihl_user.t * confirmation_email * email_layout
  | InvitationSent of Sihl_user.t * text_component list * CustomTemplate.t
  | InvitationBulkSent of
      (Sihl_user.t * text_component list * CustomTemplate.t) list
  | DefaultRestored of Default.default
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_tenant.to_ctx pool in
  function
  | Sent email ->
    let%lwt sender = sender_of_pool pool in
    Service.Email.send ~sender ~ctx:(Pool_tenant.to_ctx pool) email
  | BulkSent emails ->
    let%lwt sender = sender_of_pool pool in
    Service.Email.bulk_send ~sender ~ctx:(Pool_tenant.to_ctx pool) emails
  | ResetPassword (user, language, layout) ->
    Helper.PasswordReset.create pool language layout user
    >|- Pool_common.Utils.with_log_error
    >|> (function
    | Ok email ->
      let%lwt sender = sender_of_pool pool in
      Service.Email.send ~sender ~ctx email
    | Error (_ : PoolError.error) -> Lwt.return_unit)
  | ChangedPassword (({ Sihl_user.email; _ } as user), language, layout) ->
    let%lwt sender = sender_of_pool pool in
    Helper.PasswordChange.create
      pool
      language
      layout
      (Pool_user.EmailAddress.of_string email)
      (user |> User.user_firstname)
      (user |> User.user_lastname)
    >|> Service.Email.send ~sender ~ctx:(Pool_tenant.to_ctx pool)
  | AssignmentConfirmationSent (user, confirmation_email, layout) ->
    send_confirmation pool layout user confirmation_email
  | InvitationSent (user, data, template) ->
    let%lwt sender = sender_of_pool pool in
    Helper.prepare_boilerplate_email
      template
      user.Sihl_user.email
      ([ "name", User.user_fullname user ] @ data)
    |> Service.Email.send ~sender ~ctx:(Pool_tenant.to_ctx pool)
  | InvitationBulkSent multi_data ->
    let%lwt sender = sender_of_pool pool in
    multi_data
    |> CCList.map (fun (user, data, template) ->
         Helper.prepare_boilerplate_email
           template
           user.Sihl_user.email
           ([ "name", User.user_fullname user ] @ data))
    |> Service.Email.bulk_send ~sender ~ctx:(Pool_tenant.to_ctx pool)
  | DefaultRestored default_values ->
    Lwt_list.iter_s
      (fun { Default.label; language; text; html } ->
        let%lwt () = Repo.delete_email_template pool label language in
        let%lwt (_ : Sihl_email.Template.t) =
          Service.EmailTemplate.create
            ~ctx
            ~label:(TemplateLabel.show label)
            ~language:(Pool_common.Language.show language)
            ~html
            text
        in
        Lwt.return_unit)
      default_values
;;
