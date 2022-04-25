open Entity

type create =
  { experiment_id : Pool_common.Id.t
  ; participant : Participant.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Resent of t
[@@deriving eq, show]

let send_invitation pool participant =
  let email = Participant.email_address participant in
  let name = Participant.fullname participant in
  let open Lwt.Infix in
  Email.Helper.Invitation.create pool email name
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment_id; participant } ->
    let%lwt () = create participant |> Repo.insert pool experiment_id in
    send_invitation pool participant
  | Resent invitation ->
    let%lwt () =
      Repo.update pool { invitation with resent_at = Some (ResentAt.create ()) }
    in
    send_invitation pool invitation.participant
;;
