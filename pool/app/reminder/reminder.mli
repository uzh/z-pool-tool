val create_reminder_events
  :  Pool_tenant.t
  -> Session.t list
  -> Session.t list
  -> (Pool_event.t list, Pool_common.Message.error) result Lwt.t

val send_tenant_reminder : Pool_tenant.t -> unit Lwt.t
val run : unit -> unit Lwt.t
val register : unit -> Sihl.Container.Service.t
