val update_matches_filter
  :  ?current_user:Admin.t
  -> Database.Label.t
  -> [< `Experiment of Experiment.t * Filter.t option | `Session of Session.t ]
  -> ( Assignment.event list * Email.dispatch list
       , Pool_message.Error.t )
       Lwt_result.t

val dispatch_update_upcomming : Database.Label.t -> unit Lwt.t
val register : unit -> Sihl.Container.Service.t
val job : Database.Label.t Pool_queue.Job.t

type event = Dispatched

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
