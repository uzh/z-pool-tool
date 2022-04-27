open Entity

let send_invitation_email pool (subject : Subject.t) (experiment : Experiment.t)
  =
  let open Lwt.Infix in
  let mail_subject = "Experiment Invitation" in
  let email = Subject.email_address subject in
  let name = Subject.fullname subject in
  (* TODO:[timhub] pass context or language to event *)
  let language =
    subject.Subject.language |> CCOption.value ~default:Pool_common.Language.En
  in
  Email.Helper.prepare_email
    pool
    language
    Email.TemplateLabel.Invitation
    mail_subject
    (email |> Pool_user.EmailAddress.value)
    [ "name", name
    ; ( "experiment_description"
      , experiment.Experiment.description |> Experiment.Description.value )
    ]
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

type create =
  { experiment : Experiment.t
  ; subject : Subject.t
  }
[@@deriving eq, show]

type resent =
  { invitation : t
  ; experiment : Experiment.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Resent of resent
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment; subject } ->
    let%lwt () = create subject |> Repo.insert pool experiment.Experiment.id in
    send_invitation_email pool subject experiment
  | Resent { invitation; experiment } ->
    let%lwt () =
      Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
    in
    send_invitation_email pool invitation.subject experiment
;;
