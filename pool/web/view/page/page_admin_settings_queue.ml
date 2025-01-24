open Tyxml.Html
open Component
open Pool_message
module HttpUtils = Http_utils

let formatted_date_time = Pool_model.Time.formatted_date_time
let base_path = "/admin/settings/queue"

let list queue_table context =
  Page_admin_queue.list
    context
    queue_table
    (HttpUtils.Url.Admin.Settings.queue_list_path queue_table |> Uri.of_string)
;;

let render_email_html html =
  let style = "<style>section { word-break: break-all; } </style>" in
  let html =
    Format.asprintf "<template shadowrootmode=\"closed\">%s %s</template>" html style
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
    ; Field.EmailText, html |> CCOption.map_or ~default:(txt "") render_email_html
    ; ( Field.PlainText
      , div ~a:[ a_class [ "word-wrap-break" ] ] [ HttpUtils.add_line_breaks text ] )
    ])
;;

let text_message_job_instance_detail instance =
  let open Text_message in
  Service.Job.decode (Pool_queue.Instance.input instance)
  |> CCResult.map_or ~default:[] (fun { recipient; sender; text } ->
    [ Field.Recipient, txt (Pool_user.CellPhone.value recipient)
    ; Field.Sender, txt (Pool_tenant.GtxSender.value sender)
    ; Field.SmsText, Content.value text |> HttpUtils.add_line_breaks
    ])
;;

let matcher_job_instance_detail label = [ Field.Label, txt (Database.Label.value label) ]

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
  |> CCList.map (fun (k, v) -> tr [ th ~a:[ a_class [ "w-2" ] ] [ txt k ]; td [ txt v ] ])
  |> table ~a:[ a_class [ "table"; "striped"; "align-top" ] ]
;;

let queue_instance_detail language ?text_message_dlr instance =
  let open Pool_queue.JobName in
  let default = "-" in
  let vertical_table =
    Component.Table.vertical_table ~align_top:true ~th_class:[ "w-2" ] `Striped language
  in
  let clone_link, job_detail =
    let link id =
      let id = Pool_queue.Id.value id in
      div
        [ txt Pool_common.(Utils.text_to_string language I18n.JobCloneOf)
        ; txt " "
        ; a
            ~a:
              [ a_href (Format.asprintf "%s/%s" base_path id |> Sihl.Web.externalize_path)
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
      txt "", matcher_job_instance_detail (Pool_queue.Instance.database_label instance)
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
      ; Field.LastError, instance |> Instance.last_error |> CCOption.value ~default |> txt
      ; Field.NextRunAt, instance |> Instance.run_at |> formatted_date_time |> txt
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

let index queue_table (Pool_context.{ language; _ } as context) job =
  let open Pool_common in
  let title =
    let i18n =
      match queue_table with
      | `History -> I18n.QueueHistory
      | `Current -> I18n.Queue
    in
    h1
      ~a:[ a_class [ "heading-1"; "has-gap" ] ]
      [ txt (Utils.nav_link_to_string language i18n) ]
  in
  let switch_table =
    (fun (path_table, i18n) ->
      div
        [ a
            ~a:
              [ a_href
                  (HttpUtils.Url.Admin.Settings.queue_list_path path_table
                   |> Sihl.Web.externalize_path)
              ]
            [ txt
                (i18n
                 |> Utils.nav_link_to_string language
                 |> CCString.lowercase_ascii
                 |> Format.asprintf "Go to %s")
            ]
        ])
    @@
    match queue_table with
    | `Current -> `History, I18n.QueueHistory
    | `History -> `Current, I18n.Queue
  in
  div
    ~a:[ a_class [ "gap-lg"; "trim"; "safety-margin" ] ]
    [ title
    ; div ~a:[ a_class [ "stack" ] ] [ switch_table; list queue_table context job ]
    ]
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

let detail Pool_context.({ language; _ } as context) ?text_message_dlr instance =
  let open Pool_queue in
  let buttons_html =
    if Instance.resendable instance |> CCResult.is_ok
    then div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] [ resend_form context instance ]
    else txt ""
  in
  let html =
    div
      ~a:[ a_class [ "gap-lg" ] ]
      [ queue_instance_detail language ?text_message_dlr instance ]
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
              ~a:[ a_class [ "heading-1"; "has-gap" ] ]
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
