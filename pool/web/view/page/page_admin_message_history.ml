open Containers
open CCFun
open Tyxml.Html
module Field = Pool_message.Field

let path =
  Contact.id %> Pool_user.Id.value %> Format.asprintf "/admin/contacts/%s"
;;

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
    let instance = History.job m in
    let formatted_date_time date =
      span
        ~a:[ a_class [ "nobr" ] ]
        [ txt (Pool_model.Time.formatted_date_time date) ]
    in
    [ txt (instance |> Instance.name |> Queue.JobName.show)
    ; m |> History.message_template |> CCOption.value ~default:"" |> txt
    ; instance
      |> Instance.status
      |> Status.show
      |> CCString.capitalize_ascii
      |> txt
    ; txt (instance |> Instance.last_error |> CCOption.value ~default:"-")
    ; instance
      |> Instance.last_error_at
      |> CCOption.map_or ~default:(txt "-") formatted_date_time
    ; instance |> Instance.next_run_at |> formatted_date_time
    ; [%string "/admin/settings/queue/%{instance |> Instance.id |> Id.value}"]
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
