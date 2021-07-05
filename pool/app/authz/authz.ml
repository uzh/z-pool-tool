type thing =
  | User
  | Tenant
  | Tenant_infrastructure
  | Location
  | Experiment
  | Experiment_session

type context = thing * string option

type permission =
  | Create of context
  | Read of context
  | Update of context
  | Destroy of context
  | Manage of context

(* Default roles *)

let root = [ Manage (User, None) ]

let operator tenant_id =
  [ Manage (Tenant_infrastructure, Some tenant_id)
  ; Manage (Tenant, Some tenant_id)
  ]
;;

let recruiter tenant_id = [ Manage (Tenant, Some tenant_id) ]
let location_manager location_id = [ Read (Location, Some location_id) ]
let experimenter experiment_id = [ Manage (Experiment, Some experiment_id) ]

let assistant experiment_id =
  [ Read (Experiment, Some experiment_id)
  ; Update (Experiment, Some experiment_id)
  ]
;;

let participant user_id =
  [ Create (User, Some user_id)
  ; Read (User, Some user_id)
  ; Update (User, Some user_id)
  ]
;;

let recruiter = [ Manage (User, None) ]

type assign = Sihl.User.t -> permission list -> unit Lwt.t

let assign : assign = fun user permissions -> Repo.insert user.id permissions

type divest = Sihl.User.t -> permission list -> unit Lwt.t

let divest : divest = fun user permissions -> Repo.delete user.id permissions

type can = Sihl.User.t -> any_of:permission list -> bool Lwt.t

let can : can = fun user ~any_of:permissions -> Repo.has_any user.id permissions
