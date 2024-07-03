module Conformist = Pool_conformist
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

let parse_instance_job instance =
  let open Queue in
  let open JobName in
  let input = Instance.input instance in
  instance
  |> Instance.name
  |> function
  | CheckMatchesFilter ->
    (try
       Ok (input |> Yojson.Safe.from_string |> Database.Label.t_of_yojson)
     with
     | _ -> Error Pool_message.(Error.Invalid Field.DatabaseLabel))
    >|= fun job -> `MatcherJob job
  | SendEmail -> Email.parse_job_json input >|= fun job -> `EmailJob job
  | SendTextMessage ->
    Text_message.parse_job_json input >|= fun job -> `TextMessageJob job
;;

let update_job ?contact ?experiment instance =
  let id = Queue.Instance.id instance in
  instance
  |> parse_instance_job
  >>= function
  | `EmailJob job ->
    `EmailJob (job |> update_email_job ?contact ?experiment id)
    |> CCResult.return
  | `TextMessageJob job ->
    `TextMessageJob (job |> update_text_message_job ?contact id)
    |> CCResult.return
  | `MatcherJob _ -> Error Pool_message.Error.JobCannotBeRetriggered
;;

module Resend : sig
  include Common.CommandSig with type t = Queue.Instance.t

  val handle
    :  ?contact:Contact.t
    -> ?experiment:Experiment.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Queue.Instance.t

  let handle ?contact ?experiment queue_instance =
    let* queue_instance = Queue.Instance.resendable queue_instance in
    queue_instance
    |> update_job ?contact ?experiment
    >|= function
    | `EmailJob job -> [ Email.Sent job |> Pool_event.email ]
    | `TextMessageJob job ->
      [ Text_message.Sent job |> Pool_event.text_message ]
  ;;

  let effects = Queue.Guard.Access.resend
end

module CreateTextMessageDeliveryReport : sig
  type t = Text_message.delivery_report

  val decode
    :  (string * string list) list
    -> Queue.Id.t
    -> string
    -> (t, Pool_message.Error.t) result

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Text_message.delivery_report

  let command
    from
    to_
    message_id
    dlr_mask
    error_code
    error_message
    submit_date
    done_date
    plmn
    country
    sms_cost
    job_id
    raw
    =
    Text_message.
      { job_id
      ; raw
      ; from
      ; to_
      ; message_id
      ; dlr_mask
      ; error_code
      ; error_message
      ; submit_date
      ; done_date
      ; plmn
      ; country
      ; sms_cost
      }
  ;;

  let schema =
    let ptime name =
      let open CCResult.Infix in
      Conformist.custom
        (fun values ->
          values
          |> CCList.head_opt
          |> CCOption.to_result Pool_message.Error.NoValue
          >>= Pool_model.Time.parse_time)
        (fun date -> date |> Ptime.to_rfc3339 |> CCList.return)
        name
    in
    Conformist.(
      make
        Field.
          [ string "from"
          ; string "to"
          ; string "message-id"
          ; int "dlr-mask"
          ; int "error-code"
          ; string "error-message"
          ; ptime "submit-date"
          ; ptime "done-date"
          ; string "plmn"
          ; string "country"
          ; float "sms-cost"
          ]
        command)
  ;;

  let decode data job_id raw =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
    |> CCResult.map (fun cmd -> cmd job_id raw)
  ;;

  let handle ?(tags = Logs.Tag.empty) report =
    let open Text_message in
    Logs.info ~src (fun m -> m "Handle command TextMessageDeliveryReport" ~tags);
    Ok [ ReportCreated report |> Pool_event.text_message ]
  ;;
end
