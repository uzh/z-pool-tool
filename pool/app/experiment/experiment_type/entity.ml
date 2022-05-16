type public =
  { id : Pool_common.Id.t
  ; description : Experiment.Description.t
  }
[@@deriving eq, show]

type public_sessions =
  { experiment : public
  ; sessions : Session.Public.t list
  }
[@@deriving eq, show]

let create_public_sessions experiment sessions = { experiment; sessions }

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
