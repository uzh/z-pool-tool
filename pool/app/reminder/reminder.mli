module Service : sig
  val create_reminder_events
    :  Pool_tenant.t
    -> Session.t list
    -> Session.t list
    -> (Pool_event.t list, Pool_message.Error.t) result Lwt.t

  val send_tenant_reminder : Pool_tenant.t -> unit Lwt.t
  val run : unit -> unit Lwt.t
  val register : unit -> Sihl.Container.Service.t
end

val prepare_messages
  :  Pool_database.Label.t
  -> Pool_tenant.t
  -> Pool_common.Language.t list
  -> Experiment.t
  -> Session.t
  -> ((Assignment.t -> (Email.job, Pool_message.Error.t) result)
     * (Assignment.t
        -> Pool_user.CellPhone.t
        -> (Text_message.job, Pool_message.Error.t) result))
       Lwt.t
