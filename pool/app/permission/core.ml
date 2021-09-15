type thing =
  | System
  | Tenant
  | Tenant_recruiting
  | Location
  | Experiment
  | Experiment_session
  | Participant
[@@deriving eq, show]

type permission =
  | Create of thing
  | Read of (thing * Common.Id.t option)
  | Update of (thing * Common.Id.t option)
  | Destroy of (thing * Common.Id.t option)
  | Manage of (thing * Common.Id.t option)
[@@deriving eq, show]

let explode_permission = function
  | Manage (thing, id) ->
    [ Create thing
    ; Read (thing, id)
    ; Update (thing, id)
    ; Destroy (thing, id)
    ; Manage (thing, id)
    ]
  | other -> [ other ]
;;
