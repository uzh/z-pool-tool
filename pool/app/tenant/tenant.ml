include Entity
include Event
module Filter = Utils.Filter

let find_by_id = Utils.todo
let find_by_participant = Utils.todo
let find_by_user = Utils.todo

type list_recruiters = Filter.Ql.t
type handle_list_recruiters = list_recruiters -> Sihl_user.t list Lwt.t
type list_tenants = Filter.Ql.t
type handle_list_tenants = list_tenants -> t list Lwt.t
type add = t -> t Lwt.t
type update = t -> t Lwt.t
type destroy = t -> t Lwt.t
type disable = t Lwt.t
type enable = t Lwt.t

(* MONITORING AND MANAGEMENT *)

(* The system should proactively report degraded health to operators *)
type generate_status_report = StatusReport.t Lwt.t
