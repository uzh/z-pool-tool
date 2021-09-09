include Entity
include Event
module Filter = Utils.Filter

let find_by_id (id : string) : (t, string) result Lwt.t = Utils.todo id
let find_by_participant = Utils.todo
let find_by_user = Utils.todo

type list_recruiters = Filter.Ql.t
type handle_list_recruiters = list_recruiters -> Sihl_user.t list Lwt.t
type list_tenants = Filter.Ql.t
type handle_list_tenants = list_tenants -> t list Lwt.t

(* MONITORING AND MANAGEMENT *)

(* The system should proactively report degraded health to operators *)
type generate_status_report = StatusReport.t Lwt.t
