open Entity

let send_invitation_email
    pool
    (contact : Contact.t)
    (experiment : Experiment.t)
    default_language
  =
  let open Lwt.Infix in
  let mail_subject = "Experiment Invitation" in
  let email = Contact.email_address contact in
  let name = Contact.fullname contact in
  let language =
    contact.Contact.language |> CCOption.value ~default:default_language
  in
  Email.Helper.prepare_email
    pool
    language
    Email.TemplateLabel.Invitation
    mail_subject
    (email |> Pool_user.EmailAddress.value)
    [ "name", name
    ; ( "experimentDescription"
      , experiment.Experiment.description |> Experiment.Description.value )
    ]
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

type create =
  { experiment : Experiment.t
  ; contact : Contact.t
  }
[@@deriving eq, show]

type resent =
  { invitation : t
  ; experiment : Experiment.t
  }
[@@deriving eq, show]

type event_type =
  | Created of create
  | Resent of resent
[@@deriving eq, show]

type event = event_type * Pool_common.Language.t [@@deriving eq, show]

let handle_event pool (event, language) =
  match event with
  | Created { experiment; contact } ->
    let%lwt () = create contact |> Repo.insert pool experiment.Experiment.id in
    send_invitation_email pool contact experiment language
  | Resent { invitation; experiment } ->
    let%lwt () =
      Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
    in
    send_invitation_email pool invitation.contact experiment language
;;
