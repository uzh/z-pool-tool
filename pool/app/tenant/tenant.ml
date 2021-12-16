include Entity
include Event

(* MONITORING AND MANAGEMENT *)

(* The system should proactively report degraded health to operators *)
type generate_status_report = StatusReport.t Lwt.t
