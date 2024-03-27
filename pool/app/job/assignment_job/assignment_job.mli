val update_matches_filter
  :  ?admin:Admin.t
  -> Pool_database.Label.t
  -> [< `Experiment of Experiment.t * Filter.t option | `Session of Session.t ]
  -> ( Assignment.event list * Email.job list
       , Pool_common.Message.error )
       Lwt_result.t

val dispatch_update_upcomming : Pool_database.Label.t -> unit Lwt.t
val register : unit -> Sihl.Container.Service.t
val job : Pool_database.Label.t Sihl_queue.job

type event = Dispatched

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
