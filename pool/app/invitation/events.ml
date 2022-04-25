open Entity

type create =
  { experiment_id : Pool_common.Id.t
  ; subject : Subject.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Resent of t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment_id; subject } ->
    let%lwt () = create subject |> Repo.insert pool experiment_id in
    let email = Subject.email_address participant in
    let name = Subject.fullname participant in
    Email.Helper.Invitation.create pool email name
    >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
  | Resent _ ->
    (* TODO send invitation email *)
    Lwt.return_unit
;;
