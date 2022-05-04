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
