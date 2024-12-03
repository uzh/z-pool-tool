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
  open Pool_queue

  type t = Instance.t

  let handle ?contact ?experiment queue_instance =
    let open Instance in
    let open JobName in
    let* instance = Instance.resendable queue_instance in
    match name queue_instance with
    | SendTextMessage ->
      let* message = Text_message.Service.Job.decode (input instance) in
      message
      |> Text_message.create_job
           ?message_template:(message_template instance)
           ~job_ctx:(job_ctx_clone (id instance))
      |> Text_message.sent ?new_recipient:(CCOption.bind contact Contact.cell_phone)
      |> Pool_event.text_message
      |> CCList.return
      |> CCResult.return
    | SendEmail ->
      let open Email.Service.Job in
      let* message = decode (input instance) in
      message
      |> Email.create_sent
           ~job_ctx:(job_ctx_clone (id instance))
           ?message_template:(message_template instance)
           ?new_email_address:(CCOption.map Contact.email_address contact)
           ?new_smtp_auth_id:(CCOption.bind experiment Experiment.smtp_auth_id)
      |> Pool_event.email
      |> CCList.return
      |> CCResult.return
    | CheckMatchesFilter -> Error Pool_message.Error.JobCannotBeRetriggered
  ;;

  let effects = Guard.Access.resend
end

module CreateTextMessageDeliveryReport : sig
  type t = Text_message.delivery_report

  val decode
    :  (string * string list) list
    -> Pool_queue.Id.t
    -> string
    -> (t, Pool_message.Error.t) result

  val handle : ?tags:Logs.Tag.set -> t -> (Pool_event.t list, Pool_message.Error.t) result
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
