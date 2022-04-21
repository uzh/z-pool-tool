module Id = Pool_common.Id

type thing =
  | System
  | Tenant
  | Tenant_recruiting
  | Location
  | Experiment
  | Experiment_session
  | Participant
  | Participation
[@@deriving eq, show]

type permission =
  | Create of thing
  | Read of (thing * Id.t option)
  | Update of (thing * Id.t option)
  | Destroy of (thing * Id.t option)
  | Manage of (thing * Id.t option)
[@@deriving eq, show]

let[@warning "-4"] explode_permission = function
  | Manage (thing, id) ->
    [ Create thing
    ; Read (thing, id)
    ; Update (thing, id)
    ; Destroy (thing, id)
    ; Manage (thing, id)
    ]
  | other -> [ other ]
;;
