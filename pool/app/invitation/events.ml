open Entity

let send_invitation_email pool participant (experiment : Experiment.t) =
  let open Lwt.Infix in
  let subject = "Experiment Invitation" in
  let email = Participant.email_address participant in
  let name = Participant.fullname participant in
  Email.Helper.prepare_email
    pool
    "experiment_invitation"
    subject
    (email |> Pool_user.EmailAddress.value)
    [ "name", name
    ; ( "experiment_description"
      , experiment.Experiment.description |> Experiment.Description.value )
    ]
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

type create =
  { experiment : Experiment.t
  ; participant : Participant.t
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
  | Created { experiment; participant } ->
    let%lwt () =
      create participant |> Repo.insert pool experiment.Experiment.id
    in
    send_invitation_email pool participant experiment
  | Resent { invitation; experiment } ->
    let%lwt () =
      Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
    in
    send_invitation_email pool invitation.participant experiment
;;
