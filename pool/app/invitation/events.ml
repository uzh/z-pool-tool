open Entity

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

let send_invitation pool participant (experiment : Experiment.t) =
  let email = Participant.email_address participant in
  let name = Participant.fullname participant in
  let open Lwt.Infix in
  Email.Helper.Invitation.create
    pool
    email
    name
    (experiment.Experiment.description |> Experiment.Description.value)
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment; participant } ->
    let%lwt () =
      create participant |> Repo.insert pool experiment.Experiment.id
    in
    send_invitation pool participant experiment
  | Resent { invitation; experiment } ->
    let%lwt () =
      Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
    in
    send_invitation pool invitation.participant experiment
;;
