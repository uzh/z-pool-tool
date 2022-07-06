type role =
  { name : string
  ; permissions : Permission.permission list
  }

let root =
  Permission.
    [ Manage (Participant, None)
    ; Manage (Contact, None)
    ; Manage (Tenant, None)
    ; Manage (Tenant_recruiting, None)
    ; Manage (Location, None)
    ; Manage (Experiment, None)
    ; Manage (Experiment_session, None)
    ; Manage (System, None)
    ]
;;

let operator tenant_id = Permission.[ Manage (Tenant, Some tenant_id) ]

let recruiter tenant_id =
  Permission.[ Manage (Tenant_recruiting, Some tenant_id) ]
;;

let location_manager location_id =
  Permission.[ Read (Location, Some location_id) ]
;;

let experimenter experiment_id =
  Permission.[ Manage (Experiment, Some experiment_id) ]
;;

let assistant experiment_id =
  Permission.
    [ Read (Experiment, Some experiment_id)
    ; Update (Experiment, Some experiment_id)
    ]
;;

let contact user_id =
  Permission.
    [ Create Contact
    ; Read (Contact, Some user_id)
    ; Update (Contact, Some user_id)
    ]
;;
