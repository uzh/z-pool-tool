open Containers
open CCFun
open Tyxml.Html
module Field = Pool_message.Field

let formatted_date_time = Pool_model.Time.formatted_date_time

let path =
  Contact.id %> Contact.Id.value %> Format.asprintf "/admin/contacts/%s"
;;

let data_table_head language =
  let open Pool_queue in
  let field_to_string field =
    Pool_common.Utils.field_to_string_capitalized language field |> txt
  in
  let name = `column column_job_name in
  let message_template = `custom (field_to_string Field.MessageTemplate) in
  let status = `column column_job_status in
  let recipient = `custom (field_to_string Field.Recipient) in
  let last_error_at = `column column_error_at in
  let run_at = `column column_run_at in
  [ name; status; message_template; recipient; last_error_at; run_at; `empty ]
;;

let list Pool_context.{ language; _ } queue_table url (queued_jobs, query) =
  let open Pool_queue in
  let filterable_by =
    match queue_table with
    | `History -> history_filterable_by
    | `Current -> current_filterable_by
  in
  let data_table =
    Component.DataTable.create_meta
      ?filter:filterable_by
      ~search:searchable_by
      url
      query
      language
  in
  let cols = data_table_head language in
  let th_class = [ "w-2"; "w-2"; "w-3"; "w-3"; "w-2"; "w-2" ] in
  let row instance =
    let recipient =
      match instance |> Instance.name with
      | JobName.SendEmail -> Email.Service.Job.show_recipient instance
      | JobName.SendTextMessage ->
        Text_message.Service.Job.show_recipient instance
      | JobName.CheckMatchesFilter -> ""
    in
    let formatted_date_time date =
      span ~a:[ a_class [ "nobr" ] ] [ txt (formatted_date_time date) ]
    in
    let name =
      Instance.name
      %> JobName.show
      %> CCString.capitalize_ascii
      %> CCString.replace ~which:`All ~sub:"_" ~by:" "
    in
    [ txt (instance |> name)
    ; txt
        (instance |> Instance.status |> Status.show |> CCString.capitalize_ascii)
    ; txt (instance |> Instance.message_template |> CCOption.value ~default:"")
    ; txt recipient
    ; instance
      |> Instance.last_error_at
      |> CCOption.map_or ~default:(txt "") formatted_date_time
    ; instance |> Instance.run_at |> formatted_date_time
    ; Http_utils.Url.Admin.Settings.queue_path ~id:(Instance.id instance) ()
      |> Component.Input.link_as_button ~icon:Component.Icon.Eye
    ]
    |> CCList.map CCFun.(CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~target_id:"queue-ob-list"
    ~th_class
    ~cols
    ~row
    data_table
    queued_jobs
;;
