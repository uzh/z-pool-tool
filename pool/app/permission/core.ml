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
  | Read of (thing * string option)
  | Update of (thing * string option)
  | Destroy of (thing * string option)
  | Manage of (thing * string option)
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
