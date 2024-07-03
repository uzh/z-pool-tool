open Tyxml.Html
open Component
open Pool_message
module HttpUtils = Http_utils

let formatted_date_time = Pool_model.Time.formatted_date_time
let base_path = "/admin/settings/queue"

let data_table_head language =
  let open Pool_common in
  let open Queue in
  let field_to_string field =
    Utils.field_to_string_capitalized language field |> txt
  in
  let name = `column column_job_name in
  let status = `column column_job_status in
  let input = `custom (field_to_string Field.Input) in
  let last_error = `custom (field_to_string Field.LastError) in
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
    ; instance |> Instance.next_run_at |> formatted_date_time
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

let email_job_instance_detail { Email.email; _ } =
  let { Sihl_email.sender; recipient; subject; text; html; _ } = email in
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
  [ Field.Recipient, txt (Pool_user.CellPhone.value recipient)
  ; Field.Sender, txt (Pool_tenant.GtxSender.value sender)
  ; Field.SmsText, Content.value text |> HttpUtils.add_line_breaks
  ]
;;

let matcher_job_instance_detail label =
  [ Field.Label, txt (Database.Label.value label) ]
;;

let text_message_dlr_detail dlr =
  let open Text_message in
  [ "Dlr status", dlr.dlr_mask |> DlrMask.of_int |> DlrMask.to_human
  ; "Error code", dlr.error_code |> CCInt.to_string
  ; "Error message", dlr.error_message
  ; "Submit date", dlr.submit_date |> Pool_model.Time.formatted_date_time
  ; "Done date", dlr.done_date |> Pool_model.Time.formatted_date_time
  ; "Plmn", dlr.plmn
  ; "Country", dlr.country
  ]
  |> CCList.map (fun (k, v) ->
    tr [ th ~a:[ a_class [ "w-2" ] ] [ txt k ]; td [ txt v ] ])
  |> table ~a:[ a_class [ "table"; "striped"; "align-top" ] ]
;;

let queue_instance_detail language ?text_message_dlr instance job =
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
      let id = Queue.Id.value id in
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
    CCOption.map_or ~default:(txt "") link
    @@
    match job with
    | `MatcherJob _ -> None
    | `EmailJob { Email.resent; _ } -> resent
    | `TextMessageJob { Text_message.resent; _ } -> resent
  in
  let job_detail =
    match job with
    | `MatcherJob database_label -> matcher_job_instance_detail database_label
    | `EmailJob email -> email_job_instance_detail email
    | `TextMessageJob msg -> text_message_job_instance_detail msg
  in
  let queue_instance_detail =
    let open Queue in
    (( Field.Status
     , strong
         [ instance
           |> Instance.status
           |> Queue.Status.show
           |> CCString.capitalize_ascii
           |> txt
         ] )
     :: job_detail)
    @ [ Field.Tag, instance |> Instance.tag |> CCOption.value ~default |> txt
      ; Field.Tries, instance |> Instance.tries |> CCInt.to_string |> txt
      ; Field.MaxTries, instance |> Instance.max_tries |> CCInt.to_string |> txt
      ; ( Field.LastErrorAt
        , instance
          |> Instance.last_error_at
          |> CCOption.map_or ~default formatted_date_time
          |> txt )
      ; ( Field.LastError
        , instance |> Instance.last_error |> CCOption.value ~default |> txt )
      ; ( Field.NextRunAt
        , instance |> Instance.next_run_at |> formatted_date_time |> txt )
      ]
    |> vertical_table
  in
  let dlr_html =
    match text_message_dlr with
    | None -> txt ""
    | Some dlr ->
      div
        [ h2
            [ txt
                Pool_common.(
                  Utils.field_to_string language Field.TextMessageDlrStatus
                  |> CCString.capitalize_ascii)
            ]
        ; text_message_dlr_detail dlr
        ]
  in
  div ~a:[ a_class [ "stack" ] ] [ clone_link; queue_instance_detail; dlr_html ]
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
             (job |> Queue.Instance.id |> Queue.Id.value)
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

let detail
  Pool_context.({ language; _ } as context)
  ?text_message_dlr
  instance
  job
  =
  let buttons_html =
    if Queue.Instance.resendable instance |> CCResult.is_ok
    then
      div
        ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
        [ resend_form context instance ]
    else txt ""
  in
  let html =
    div
      ~a:[ a_class [ "gap-lg" ] ]
      [ queue_instance_detail language ?text_message_dlr instance job ]
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
                  (instance |> Queue.Instance.name |> Queue.JobName.show)
                  (instance
                   |> Queue.Instance.last_error_at
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
