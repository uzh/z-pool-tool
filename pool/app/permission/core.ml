module Id = Pool_common.Id

type thing =
  | Experiment
  | Experiment_session
  | Invitation
  | Location
  | Participant
  | Participation
  | System
  | Subject
  | Tenant
  | Tenant_recruiting
  | Waiting_list
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
