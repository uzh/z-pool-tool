module StatusReport : sig
  type t

  val equal : t -> t -> bool
end

type event =
  | OperatorAssigned of Pool_common.Id.t * Admin.operator Admin.t
  | OperatorDivested of Pool_common.Id.t * Admin.operator Admin.t
  | StatusReportGenerated of unit

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

type generate_status_report = StatusReport.t Lwt.t
