open Tyxml.Html
open Component
open Pool_message
module HttpUtils = Http_utils

let formatted_date_time = Pool_model.Time.formatted_date_time
let base_path = "/admin/settings/queue"

let data_table_head language =
  let open Pool_common in
  let open Pool_queue in
  let field_to_string field =
    Utils.field_to_string_capitalized language field |> txt
  in
  let name = `column Mapping.column_job_name in
  let status = `column Mapping.column_job_status in
  let input = `custom (field_to_string Field.Input) in
  let last_error = `custom (field_to_string Field.LastError) in
  let last_error_at = `column Mapping.column_error_at in
  let next_run = `column Mapping.column_run_at in
  function
  | `settings ->
    [ name; status; input; last_error; last_error_at; next_run; `empty ]
  | `history ->
    [ name; `empty; status; last_error; last_error_at; next_run; `empty ]
;;

let data_table Pool_context.{ language; _ } (queued_jobs, query) =
  let open Pool_queue in
  let url = Uri.of_string base_path in
  let data_table =
    Component.DataTable.create_meta
      ?filter:Job.filterable_by
      ~search:Job.searchable_by
      url
      query
      language
  in
  let cols = data_table_head language `settings in
  let th_class = [ "w-1"; "w-1"; "w-4"; "w-2"; "w-1"; "w-1" ] in
  let row instance =
    let formatted_date_time date =
      span ~a:[ a_class [ "nobr" ] ] [ txt (formatted_date_time date) ]
    in
    [ txt (instance |> Instance.name |> JobName.show)
    ; txt
        (instance |> Instance.status |> Status.show |> CCString.capitalize_ascii)
    ; span
        ~a:[ a_class [ "word-break-all" ] ]
        [ txt (instance |> Instance.input |> CCString.take 80) ]
    ; txt (instance |> Instance.last_error |> CCOption.value ~default:"-")
    ; instance
      |> Instance.last_error_at
      |> CCOption.map_or ~default:(txt "-") formatted_date_time
    ; instance |> Instance.run_at |> formatted_date_time
    ; [%string "/admin/settings/queue/%{instance |> Instance.id |> Id.value}"]
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

let render_email_html html =
  let style = "<style>section { word-break: break-all; } </style>" in
  let html =
    Format.asprintf
      "<template shadowrootmode=\"closed\">%s %s</template>"
      html
      style
  in
  div ~a:[ a_class [ "border" ] ] Unsafe.[ data html ]
;;

let email_job_instance_detail instance =
  let open Email.Service.Job in
  decode (Pool_queue.Instance.input instance)
  |> CCResult.map_or ~default:[] (fun job ->
    let { Sihl_email.sender; recipient; subject; text; html; _ } = email job in
    [ Field.Sender, txt sender
    ; Field.Recipient, txt recipient
    ; Field.EmailSubject, txt subject
    ; ( Field.EmailText
      , html |> CCOption.map_or ~default:(txt "") render_email_html )
    ; ( Field.PlainText
      , div
          ~a:[ a_class [ "word-wrap-break" ] ]
          [ HttpUtils.add_line_breaks text ] )
    ])
;;

let text_message_job_instance_detail instance =
  let open Text_message in
  Service.Job.decode (Pool_queue.Instance.input instance)
  |> CCResult.map_or ~default:[] (fun { recipient; sender; text } ->
    [ Field.Recipient, txt (Pool_user.CellPhone.value recipient)
    ; Field.Sender, txt (Pool_tenant.Title.value sender)
    ; Field.SmsText, Content.value text |> HttpUtils.add_line_breaks
    ])
;;

let matcher_job_instance_detail label =
  [ Field.Label, txt (Database.Label.value label) ]
;;

let queue_instance_detail language instance =
  let open Pool_queue.JobName in
  let default = "-" in
  let vertical_table =
    Component.Table.vertical_table
      ~align_top:true
      ~th_class:[ "w-2" ]
      `Striped
      language
  in
  let clone_link, job_detail =
    let link id =
      let id = Pool_queue.Id.value id in
      div
        [ txt Pool_common.(Utils.text_to_string language I18n.JobCloneOf)
        ; txt " "
        ; a
            ~a:
              [ a_href
                  (Format.asprintf "%s/%s" base_path id
                   |> Sihl.Web.externalize_path)
              ]
            [ txt id ]
        ]
    in
    let link =
      let open CCOption in
      instance |> Pool_queue.Instance.clone_of |> map_or ~default:(txt "") link
    in
    match Pool_queue.Instance.name instance with
    | CheckMatchesFilter ->
      ( txt ""
      , matcher_job_instance_detail
          (Pool_queue.Instance.database_label instance) )
    | SendEmail -> link, email_job_instance_detail instance
    | SendTextMessage -> link, text_message_job_instance_detail instance
  in
  let queue_instance_detail =
    let open Pool_queue in
    (( Field.Status
     , strong
         [ instance
           |> Instance.status
           |> Pool_queue.Status.show
           |> CCString.capitalize_ascii
           |> txt
         ] )
     :: job_detail)
    @ [ Field.Tries, instance |> Instance.tries |> CCInt.to_string |> txt
      ; Field.MaxTries, instance |> Instance.max_tries |> CCInt.to_string |> txt
      ; ( Field.LastErrorAt
        , instance
          |> Instance.last_error_at
          |> CCOption.map_or ~default formatted_date_time
          |> txt )
      ; ( Field.LastError
        , instance |> Instance.last_error |> CCOption.value ~default |> txt )
      ; ( Field.NextRunAt
        , instance |> Instance.run_at |> formatted_date_time |> txt )
      ]
    |> vertical_table
  in
  div ~a:[ a_class [ "stack" ] ] [ clone_link; queue_instance_detail ]
;;

let index (Pool_context.{ language; _ } as context) job =
  let title =
    h1
      ~a:[ a_class [ "heading-1" ] ]
      [ txt Pool_common.(Utils.nav_link_to_string language I18n.Queue) ]
  in
  let html = data_table context job in
  div ~a:[ a_class [ "gap-lg"; "trim"; "safety-margin" ] ] [ title; html ]
;;

let resend_form Pool_context.{ csrf; language; _ } job =
  form
    ~a:
      [ a_action
          (Format.asprintf
             "%s/%s/resend"
             base_path
             (job |> Pool_queue.Instance.id |> Pool_queue.Id.value)
           |> Sihl.Web.externalize_path)
      ; a_method `Post
      ]
    [ Input.csrf_element csrf ()
    ; Input.submit_element
        language
        (Control.Resend (Some Field.Message))
        ~classnames:[ "small" ]
        ~has_icon:Icon.RefreshOutline
        ()
    ]
;;

let detail Pool_context.({ language; _ } as context) instance =
  let open Pool_queue in
  let buttons_html =
    if Instance.resendable instance |> CCResult.is_ok
    then
      div
        ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
        [ resend_form context instance ]
    else txt ""
  in
  let html =
    div ~a:[ a_class [ "gap-lg" ] ] [ queue_instance_detail language instance ]
  in
  let title =
    div
      ~a:
        [ a_class
            [ "flexrow"
            ; "justify-between"
            ; "align-center"
            ; "flex-gap"
            ; "flexcolumn-mobile"
            ]
        ]
      [ div
          [ h1
              ~a:[ a_class [ "heading-1" ] ]
              [ Format.asprintf
                  "%s%s"
                  (instance |> Instance.name |> JobName.show)
                  (instance
                   |> Instance.last_error_at
                   |> CCOption.map_or
                        ~default:""
                        CCFun.(formatted_date_time %> Format.asprintf " (%s)"))
                |> txt
              ]
          ]
      ; buttons_html
      ]
  in
  div ~a:[ a_class [ "gap-lg"; "trim"; "safety-margin" ] ] [ title; html ]
;;
