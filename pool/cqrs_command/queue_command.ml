module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_message
open CCResult.Infix

let src = Logs.Src.create "queue.cqrs"

let update_email_job
  ?contact
  ?experiment
  instance_id
  (Email.{ email; _ } as job)
  =
  let open Email in
  let email_address =
    CCOption.map
      CCFun.(Contact.email_address %> Pool_user.EmailAddress.value)
      contact
  in
  let email =
    Sihl.Contract.Email.
      { email with
        recipient = CCOption.value ~default:job.email.recipient email_address
      }
  in
  { job with
    email
  ; smtp_auth_id = CCOption.bind experiment Experiment.smtp_auth_id
  ; resent = Some instance_id
  }
;;

let update_text_message_job
  ?contact
  instance_id
  (Text_message.{ message; _ } as job)
  =
  let open Text_message in
  let recipient =
    CCOption.bind contact (fun contact -> contact.Contact.cell_phone)
    |> CCOption.value ~default:message.recipient
  in
  let message = { message with recipient } in
  { job with message; resent = Some instance_id }
;;

let parse_instance_job { Sihl_queue.name; input; _ } =
  let open Queue.JobName in
  (try Ok (name |> read) with
   | _ -> Error Message.(Error.Invalid Field.Input))
  >>= function
  | SendEmail -> Email.parse_job_json input >|= fun job -> `EmailJob job
  | SendTextMessage ->
    Text_message.parse_job_json input >|= fun job -> `TextMessageJob job
;;

let update_job ?contact ?experiment ({ Sihl_queue.id; _ } as instance) =
  let id = Pool_common.Id.of_string id in
  instance
  |> parse_instance_job
  >|= function
  | `EmailJob job -> `EmailJob (job |> update_email_job ?contact ?experiment id)
  | `TextMessageJob job ->
    `TextMessageJob (job |> update_text_message_job ?contact id)
;;

module Resend : sig
  include Common.CommandSig with type t = Sihl_queue.instance

  val handle
    :  ?contact:Contact.t
    -> ?experiment:Experiment.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Sihl_queue.instance

  let handle ?contact ?experiment queue_instance =
    let* queue_instance = Queue.resendable queue_instance in
    queue_instance
    |> update_job ?contact ?experiment
    >|= function
    | `EmailJob job -> [ Email.Sent job |> Pool_event.email ]
    | `TextMessageJob job ->
      [ Text_message.Sent job |> Pool_event.text_message ]
  ;;

  let effects = Queue.Guard.Access.resend
end
