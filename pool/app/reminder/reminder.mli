module Service : sig
  val create_reminder_events
    :  Pool_tenant.t
    -> Session.t list
    -> Session.t list
    -> (Pool_event.t list, Pool_common.Message.error) result Lwt.t

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
  -> ((Assignment.t -> (Sihl_email.t, Pool_common.Message.error) result)
     * (Assignment.t
        -> Pool_user.CellPhone.t
        -> (Text_message.t, Pool_common.Message.error) result))
       Lwt.t
