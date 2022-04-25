open Entity

let send_invitation_email pool (subject : Subject.t) (experiment : Experiment.t)
  =
  let open Lwt.Infix in
  let mail_subject = "Experiment Invitation" in
  let email = Subject.email_address subject in
  let name = Subject.fullname subject in
  Email.Helper.prepare_email
    pool
    "experiment_invitation"
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

type event =
  | Created of create
  | Resent of t
[@@deriving eq, show]

let send_invitation pool subject =
  let email = Subject.email_address subject in
  let name = Subject.fullname subject in
  let open Lwt.Infix in
  Email.Helper.Invitation.create pool email name
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment; subject } ->
    let%lwt () = create subject |> Repo.insert pool experiment.Experiment.id in
    send_invitation_email pool subject experiment
  | Resent invitation ->
    let%lwt () =
      Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
    in
    send_invitation pool invitation.subject
;;
