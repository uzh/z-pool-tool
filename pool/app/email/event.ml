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
  if Pool_database.(Label.equal pool root)
  then Lwt.return (Sihl.Configuration.read_string "SMTP_SENDER")
  else (
    let%lwt sender_email =
      Settings.find_contact_email pool ||> Settings.ContactEmail.show
    in
    let%lwt tenant = Pool_tenant.find_by_label pool ||> CCResult.get_exn in
    Format.sprintf
      "%s <%s>"
      (Pool_tenant.Title.value tenant.Pool_tenant.title)
      sender_email
    |> CCOption.pure
    |> Lwt.return)
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
  email_template |> Service.Email.send ?sender ~ctx:(Pool_tenant.to_ctx pool)
;;

let deactivate_token pool token =
  Service.Token.deactivate ~ctx:(Pool_tenant.to_ctx pool) token
;;

type verification_event =
  | Created of Pool_user.EmailAddress.t * Token.t * Pool_common.Id.t
  | Updated of
      User.EmailAddress.t * Sihl_user.t * Pool_common.Language.t * email_layout
  | EmailVerified of unverified t

let verification_event_name = function
  | Created _ -> "Created"
  | Updated _ -> "Updated"
  | EmailVerified _ -> "EmailVerified"
;;

let[@warning "-27"] handle_verification_event pool
  : verification_event -> unit Lwt.t
  = function
  | Created (address, token, user_id) ->
    let%lwt () = Repo.delete_unverified_by_user pool user_id in
    let%lwt user =
      Service.User.find_by_email
        ~ctx:(Pool_tenant.to_ctx pool)
        (User.EmailAddress.value address)
    in
    let unverified_email = create address user token in
    Repo.insert pool unverified_email
  | Updated (address, user, language, layout) ->
    Repo.delete_unverified_by_user
      pool
      (user.Sihl_user.id |> Pool_common.Id.of_string)
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
  | Created (m, t, id) ->
    Pool_user.EmailAddress.pp Format.std_formatter m;
    Token.pp Format.std_formatter t;
    Pool_common.Id.pp formatter id
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
  | InvitationSent of Sihl_user.t * text_component list * CustomTemplate.t
  | InvitationBulkSent of
      (Sihl_user.t * text_component list * CustomTemplate.t) list
  | DefaultRestored of Default.default
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let ctx = Pool_tenant.to_ctx pool in
  function
  | Sent email ->
    let%lwt sender = sender_of_pool pool in
    Service.Email.send ?sender ~ctx:(Pool_tenant.to_ctx pool) email
  | BulkSent emails ->
    let%lwt sender = sender_of_pool pool in
    Service.Email.bulk_send ?sender ~ctx:(Pool_tenant.to_ctx pool) emails
  | InvitationSent (user, data, template) ->
    let%lwt sender = sender_of_pool pool in
    Helper.prepare_boilerplate_email
      template
      user.Sihl_user.email
      ([ "name", User.user_fullname user ] @ data)
    |> Service.Email.send ?sender ~ctx:(Pool_tenant.to_ctx pool)
  | InvitationBulkSent multi_data ->
    let%lwt sender = sender_of_pool pool in
    multi_data
    |> CCList.map (fun (user, data, template) ->
         Helper.prepare_boilerplate_email
           template
           user.Sihl_user.email
           ([ "name", User.user_fullname user ] @ data))
    |> Service.Email.bulk_send ?sender ~ctx:(Pool_tenant.to_ctx pool)
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
