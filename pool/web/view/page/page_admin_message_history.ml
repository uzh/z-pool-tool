open Containers
open CCFun
open Tyxml.Html
open Pool_common
module Field = Message.Field

let path = Contact.id %> Id.value %> Format.asprintf "/admin/contacts/%s"

let list Pool_context.{ language; _ } url (messages, query) =
  let open Message_history in
  let data_table = Component.DataTable.create_meta url query language in
  let field_to_txt = Utils.field_to_string_capitalized language %> txt in
  let cols =
    [ `custom (field_to_txt Field.Status)
    ; `custom (field_to_txt Field.Template)
    ; `column column_created_at
    ; `empty
    ]
  in
  let th_class = [ "w-5"; "w-5"; "w-2" ] in
  let row m =
    let open Sihl_queue in
    let job = job m in
    let formatted_date_time date =
      span
        ~a:[ a_class [ "nobr" ] ]
        [ txt (Utils.Time.formatted_date_time date) ]
    in
    [ job.status |> Queue.Status.sihl_queue_to_human |> txt
    ; m |> message_template |> CCOption.value ~default:"" |> txt
    ; job.last_error_at
      |> CCOption.map_or ~default:(txt "-") formatted_date_time
    ; txt "btn"
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~target_id:"contacts-list"
    ~th_class
    ~cols
    ~row
    data_table
    messages
;;
