open Entity

type confirmation_email =
  { subject : I18n.Content.t
  ; text : I18n.Content.t
  ; language : Pool_common.Language.t
  ; session_text : string
  }
[@@deriving eq, show]

let send_conformation
    pool
    (contact : Contact.t)
    { subject; text; language; session_text }
  =
  let%lwt email_template =
    let email = contact.Contact.user.Sihl_user.email in
    let content =
      Format.asprintf "%s\n%s" (text |> I18n.Content.value) session_text
    in
    Email.Helper.prepare_email
      pool
      language
      Email.TemplateLabel.Boilerplate
      (subject |> I18n.Content.value)
      email
      [ "name", contact |> Contact.fullname; "content", content ]
  in
  email_template |> Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

type create =
  { contact : Contact.t
  ; session_id : Pool_common.Id.t
  }
[@@deriving eq, show]

type event =
  | Canceled of t
  | Created of create
  | ConfirmationSent of confirmation_email * Contact.t
  | Participated of t * Participated.t
  | ShowedUp of t * ShowUp.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Canceled assignment ->
    let%lwt () =
      { assignment with canceled_at = CanceledAt.create_now () }
      |> Repo.update pool
    in
    Lwt.return_unit
  | Created { contact; session_id } ->
    contact |> create |> Repo.insert pool session_id
  | ConfirmationSent (confirmation_email, contact) ->
    send_conformation pool contact confirmation_email
  | Participated (assignment, participated) ->
    let%lwt () = { assignment with participated } |> Repo.update pool in
    Lwt.return_unit
  | ShowedUp (assignment, show_up) ->
    let%lwt () = { assignment with show_up } |> Repo.update pool in
    Lwt.return_unit
;;
