open Containers
open CCFun
open Tyxml.Html
open Pool_common
module Field = Pool_message.Field

let path = Contact.id %> Id.value %> Format.asprintf "/admin/contacts/%s"

let list Pool_context.{ language; _ } url (messages, query) =
  let open Queue in
  let data_table =
    let open History in
    Component.DataTable.create_meta
      ?filter:filterable_by
      ~search:searchable_by
      url
      query
      language
  in
  let cols = Page_admin_settings_queue.data_table_head language `history in
  let th_class = [ "w-2"; "w-2"; "w-2"; "w-2"; "w-2" ] in
  let row m =
    let { Sihl_queue.id
        ; name
        ; status
        ; last_error
        ; last_error_at
        ; next_run_at
        ; _
        }
      =
      History.job m
    in
    let formatted_date_time date =
      span
        ~a:[ a_class [ "nobr" ] ]
        [ txt (Pool_model.Time.formatted_date_time date) ]
    in
    [ txt name
    ; m |> History.message_template |> CCOption.value ~default:"" |> txt
    ; status |> Status.sihl_queue_to_human |> txt
    ; txt (CCOption.value ~default:"-" last_error)
    ; last_error_at |> CCOption.map_or ~default:(txt "-") formatted_date_time
    ; next_run_at |> formatted_date_time
    ; Format.asprintf "/admin/settings/queue/%s" id
      |> Component.Input.link_as_button ~icon:Component.Icon.Eye
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
