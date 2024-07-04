module Conformist = Pool_conformist
module Message = Pool_message
open CCResult.Infix

let src = Logs.Src.create "queue.cqrs"

module Resend : sig
  include Common.CommandSig with type t = Pool_queue.Instance.t

  val handle
    :  ?contact:Contact.t
    -> ?experiment:Experiment.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Pool_queue.Instance.t

  let handle ?contact ?experiment queue_instance =
    let open Pool_queue.Instance in
    let open Pool_queue.JobName in
    let* instance = Pool_queue.Instance.resendable queue_instance in
    match name queue_instance with
    | SendTextMessage ->
      let* message = Text_message.Service.Job.decode (input instance) in
      message
      |> Text_message.create_job
           ?message_template:(message_template instance)
           ~mappings:(Pool_queue.Clone (id instance))
      |> Text_message.sent
           ?new_recipient:(CCOption.bind contact Contact.cell_phone)
      |> Pool_event.text_message
      |> CCList.return
      |> CCResult.return
    | SendEmail ->
      let open Email.Service.Job in
      let* message = decode (input instance) in
      message
      |> Email.create_sent
           ~mappings:(Pool_queue.Clone (id instance))
           ?message_template:(message_template instance)
           ?new_email_address:(CCOption.map Contact.email_address contact)
           ?new_smtp_auth_id:(CCOption.bind experiment Experiment.smtp_auth_id)
      |> Pool_event.email
      |> CCList.return
      |> CCResult.return
    | CheckMatchesFilter -> Error Pool_message.Error.JobCannotBeRetriggered
  ;;

  let effects = Queue.Guard.Access.resend
end
