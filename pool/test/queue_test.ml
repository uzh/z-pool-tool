module QueueCommand = Cqrs_command.Queue_command
module Field = Pool_message.Field

module DlrData = struct
  let from = "UAST"
  let to_ = "+41791234567"
  let message_id = "0f32bc8f-7271-4228-bf05-ba32403af065"
  let dlr_mask_str = "1"
  let dlr_mask = 1
  let error_code_str = "0000"
  let error_code = 0
  let error_message = "Success"
  let submit_date_str = "2024-06-24T10:27:56Z"

  let submit_date =
    Pool_model.Time.parse_time submit_date_str |> CCResult.get_exn
  ;;

  let done_date_str = "2024-06-24T10:27:56Z"
  let done_date = Pool_model.Time.parse_time done_date_str |> CCResult.get_exn
  let plmn = "22801"
  let country = "ch"
  let sms_cost = 0.1
  let sms_cost_str = "0.1"

  let urlencoded =
    [ "from", [ from ]
    ; "to", [ to_ ]
    ; "message-id", [ message_id ]
    ; "dlr-mask", [ dlr_mask_str ]
    ; "error-code", [ error_code_str ]
    ; "error-message", [ error_message ]
    ; "submit-date", [ submit_date_str ]
    ; "done-date", [ done_date_str ]
    ; "plmn", [ plmn ]
    ; "country", [ country ]
    ; "sms-cost", [ sms_cost_str ]
    ]
  ;;
end

let create_text_message_dlr () =
  let open QueueCommand.CreateTextMessageDeliveryReport in
  let open CCResult.Infix in
  let open DlrData in
  let raw = "raw request" in
  let job_id = Pool_queue.Id.create () in
  let events = decode urlencoded job_id raw >>= handle in
  let expected =
    let open Text_message in
    Ok
      [ ReportCreated
          { job_id
          ; raw
          ; from
          ; to_
          ; message_id
          ; error_code
          ; error_message
          ; dlr_mask
          ; submit_date
          ; done_date
          ; plmn
          ; country
          ; sms_cost
          }
        |> Pool_event.text_message
      ]
  in
  Test_utils.check_result expected events
;;
