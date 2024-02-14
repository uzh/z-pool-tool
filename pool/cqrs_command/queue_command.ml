module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message
open CCResult.Infix

let src = Logs.Src.create "queue.cqrs"

let update_email_job ?contact ?experiment (Email.{ email; _ } as job) =
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
  }
;;

let update_text_message_job ?contact (Text_message.{ message; _ } as job) =
  let open Text_message in
  let recipient =
    CCOption.bind contact (fun contact -> contact.Contact.cell_phone)
    |> CCOption.value ~default:message.recipient
  in
  let message = { message with recipient } in
  { job with message }
;;

let update_job ?contact ?experiment { Sihl_queue.name; input; _ } =
  let open Queue.JobName in
  (try Ok (name |> read) with
   | _ -> Error Message.(Invalid Field.Input))
  >>= function
  | SendEmail ->
    Email.parse_job_json input
    >|= update_email_job ?contact ?experiment
    >|= fun job -> `email job
  | SendTextMessage ->
    Text_message.parse_job_json input
    >|= update_text_message_job ?contact
    >|= fun job -> `textmessage job
;;

module Resend : sig
  include Common.CommandSig with type t = Sihl_queue.instance

  val handle
    :  ?contact:Contact.t
    -> ?experiment:Experiment.t
    -> t
    -> (Pool_event.t list, Message.error) result
end = struct
  type t = Sihl_queue.instance

  let handle ?contact ?experiment job =
    let* job = Queue.resendable job in
    job
    |> update_job ?contact ?experiment
    >|= function
    | `email job -> [ Email.Sent job |> Pool_event.email ]
    | `textmessage job -> [ Text_message.Sent job |> Pool_event.text_message ]
  ;;

  let effects = Queue.Guard.Access.resend
end
