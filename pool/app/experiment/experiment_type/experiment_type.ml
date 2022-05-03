type public =
  { id : Pool_common.Id.t
  ; description : Experiment.Description.t
  ; sessions : Session.t list
  }
[@@deriving eq, show]

type sessions =
  { experiment : Experiment.t
  ; sessions : Session.t list
  }
[@@deriving eq, show]

type invitations =
  { experiment : Experiment.t
  ; invitations : Invitation.t list
  }
[@@deriving eq, show]

let find_invitations pool id =
  let open Lwt_result.Syntax in
  let* experiment = Experiment.find pool id in
  let* invitations =
    Invitation.find_by_experiment pool experiment.Experiment.id
  in
  Lwt.return_ok { experiment; invitations }
;;
