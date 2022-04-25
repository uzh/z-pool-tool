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

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment_id; participant } ->
    let open Lwt.Infix in
    let%lwt () = create participant |> Repo.insert pool experiment_id in
    let email = Participant.email_address participant in
    let name = Participant.fullname participant in
    Email.Helper.Invitation.create pool email name
    >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
  | Resent _ ->
    (* TODO send invitation email *)
    Lwt.return_unit
;;
