open Tyxml.Html
open Component
module HttpUtils = Http_utils
module Message = Pool_common.Message

let formatted_date_time = Pool_common.Utils.Time.formatted_date_time
let base_path = "/admin/settings/queue"

let data_table_head language =
  let open Pool_common in
  let open Queue in
  let field_to_string field =
    Utils.field_to_string_capitalized language field |> txt
  in
  let name = `column column_job_name in
  let status = `column column_job_status in
  let input = `custom (field_to_string Message.Field.Input) in
  let last_error = `custom (field_to_string Message.Field.LastError) in
  let last_error_at = `column column_last_error_at in
  let next_run = `column column_next_run in
  function
  | `settings ->
    [ name; status; input; last_error; last_error_at; next_run; `empty ]
  | `history ->
    [ name; `empty; status; last_error; last_error_at; next_run; `empty ]
;;

let data_table Pool_context.{ language; _ } (queued_jobs, query) =
  let open Queue in
  let url = Uri.of_string base_path in
  let data_table =
    Component.DataTable.create_meta
      ?filter:filterable_by
      ~search:searchable_by
      url
      query
      language
  in
  let cols = data_table_head language `settings in
  let th_class = [ "w-1"; "w-1"; "w-4"; "w-2"; "w-1"; "w-1" ] in
  let row
    { Sihl_queue.id
    ; name
    ; status
    ; input
    ; last_error
    ; last_error_at
    ; next_run_at
    ; _
    }
    =
    let formatted_date_time date =
      span ~a:[ a_class [ "nobr" ] ] [ txt (formatted_date_time date) ]
    in
    [ txt name
    ; txt (Status.sihl_queue_to_human status)
    ; span ~a:[ a_class [ "word-break-all" ] ] [ txt (CCString.take 80 input) ]
    ; txt (CCOption.value ~default:"-" last_error)
    ; last_error_at |> CCOption.map_or ~default:(txt "-") formatted_date_time
    ; next_run_at |> formatted_date_time
    ; Format.asprintf "/admin/settings/queue/%s" id
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

let email_job_instance_detail { Email.email; _ } =
  let { Sihl_email.sender; recipient; subject; text; html; _ } = email in
  let open Message in
  [ Field.Sender, txt sender
  ; Field.Recipient, txt recipient
  ; Field.EmailSubject, txt subject
  ; Field.EmailText, html |> CCOption.map_or ~default:(txt "") render_email_html
  ; ( Field.PlainText
    , div
        ~a:[ a_class [ "word-wrap-break" ] ]
        [ HttpUtils.add_line_breaks text ] )
  ]
;;

let text_message_job_instance_detail { Text_message.message; _ } =
  let open Text_message in
  let { recipient; sender; text } = message in
  let open Message in
  [ Field.Recipient, txt (Pool_user.CellPhone.value recipient)
  ; Field.Sender, txt (Pool_tenant.Title.value sender)
  ; Field.SmsText, Content.value text |> HttpUtils.add_line_breaks
  ]
;;

let queue_instance_detail
  language
  { Sihl_queue.tries
  ; next_run_at
  ; max_tries
  ; status
  ; last_error
  ; last_error_at
  ; tag
  ; _
  }
  job
  =
  let default = "-" in
  let vertical_table =
    Component.Table.vertical_table
      ~align_top:true
      ~th_class:[ "w-2" ]
      `Striped
      language
  in
  let clone_link =
    let link id =
      let open Pool_common in
      let id = Id.value id in
      div
        [ txt (Utils.text_to_string language I18n.JobCloneOf)
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
    CCOption.map_or ~default:(txt "") link
    @@
    match job with
    | `EmailJob { Email.resent; _ } -> resent
    | `TextMessageJob { Text_message.resent; _ } -> resent
  in
  let job_detail =
    match job with
    | `EmailJob email -> email_job_instance_detail email
    | `TextMessageJob msg -> text_message_job_instance_detail msg
  in
  let queue_instance_detail =
    let open Message in
    ((Field.Status, strong [ status |> Queue.Status.sihl_queue_to_human |> txt ])
     :: job_detail)
    @ [ Field.Tag, tag |> CCOption.value ~default |> txt
      ; Field.Tries, tries |> CCInt.to_string |> txt
      ; Field.MaxTries, max_tries |> CCInt.to_string |> txt
      ; ( Field.LastErrorAt
        , last_error_at |> CCOption.map_or ~default formatted_date_time |> txt )
      ; Field.LastError, last_error |> CCOption.value ~default |> txt
      ; Field.NextRunAt, next_run_at |> formatted_date_time |> txt
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
          (Format.asprintf "%s/%s/resend" base_path job.Sihl_queue.id
           |> Sihl.Web.externalize_path)
      ; a_method `Post
      ]
    [ Input.csrf_element csrf ()
    ; Input.submit_element
        language
        Message.(Resend (Some Field.Message))
        ~classnames:[ "small" ]
        ~has_icon:Icon.RefreshOutline
        ()
    ]
;;

let detail Pool_context.({ language; _ } as context) instance job =
  let buttons_html =
    if Queue.resendable instance |> CCResult.is_ok
    then
      div
        ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
        [ resend_form context instance ]
    else txt ""
  in
  let html =
    div
      ~a:[ a_class [ "gap-lg" ] ]
      [ queue_instance_detail language instance job ]
  in
  let title =
    let { Sihl_queue.name; last_error_at; _ } = instance in
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
                  name
                  (last_error_at
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
