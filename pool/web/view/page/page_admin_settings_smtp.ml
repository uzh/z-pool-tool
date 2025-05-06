open Tyxml.Html
open Component.Input
open Pool_message
module Icon = Component.Icon
module Field = Pool_message.Field
module SmtpAuth = Email.SmtpAuth

let base_path = function
  | `Tenant -> "/admin/settings/smtp"
  | `Root -> "/root/settings/smtp"
;;

let list Pool_context.{ language; csrf; _ } location smtp_auth_list query =
  let url = Uri.of_string (base_path location) in
  let data_table = Component.DataTable.create_meta url query language in
  let cols =
    let create_smtp : [ | Html_types.flow5 ] elt =
      Component.Input.link_as_button
        ~style:`Success
        ~icon:Icon.Add
        ~control:(language, Control.Add (Some Field.Smtp))
        (Format.asprintf "%s/new" (base_path location))
    in
    [ `column SmtpAuth.column_label
    ; `column SmtpAuth.column_smtp_server
    ; `column SmtpAuth.column_smtp_username
    ; `column SmtpAuth.column_smtp_mechanism
    ; `column SmtpAuth.column_smtp_protocol
    ; `column SmtpAuth.column_smtp_default_account
    ; `custom create_smtp
    ]
  in
  let delete_button (auth : SmtpAuth.t) =
    let open SmtpAuth in
    let action_path =
      Format.asprintf "%s/%s/delete" (base_path location) (auth.id |> Id.value)
      |> Sihl.Web.externalize_path
    in
    form
      ~a:
        [ a_method `Post
        ; a_action action_path
        ; a_user_data
            "confirmable"
            Pool_common.(Utils.confirmable_to_string language I18n.DeleteSmtpServer)
        ]
      [ csrf_element csrf (); submit_icon ~classnames:[ "error" ] Icon.TrashOutline ]
  in
  let row (auth : SmtpAuth.t) =
    let open SmtpAuth in
    let open Pool_message in
    [ auth.label |> Label.value |> txt, Some Field.Label
    ; auth.server |> Server.value |> txt, Some Field.SmtpServer
    ; ( auth.username |> CCOption.map_or ~default:"" Username.value |> txt
      , Some Field.SmtpUsername )
    ; auth.mechanism |> Mechanism.show |> txt, Some Field.SmtpMechanism
    ; auth.protocol |> Protocol.show |> txt, Some Field.SmtpProtocol
    ; ( auth.default |> Default.value |> Utils.Bool.to_string |> txt
      , Some Field.DefaultSmtpServer )
    ; ( button_group
          [ edit_link (Format.asprintf "%s/%s" (base_path location) (auth.id |> Id.value))
          ; delete_button auth
          ]
      , None )
    ]
    |> CCList.map (fun (html, field) ->
      td ~a:(Component.Table.data_label_opt language field) [ html ])
    |> tr
  in
  Component.DataTable.make
    ~break_mobile:true
    ~target_id:"smtp-table"
    ~cols
    ~row
    data_table
    smtp_auth_list
;;

let index (Pool_context.{ language; _ } as context) location smtp_auth_list query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Smtp) ]
    ; list context location smtp_auth_list query
    ]
;;

let smtp_form_inputs ?flash_fetcher language (smtp_auth : SmtpAuth.t option) =
  let open SmtpAuth in
  let input_element_root
        ?(break = false)
        ?(required = false)
        ?(field_type = `Text)
        field
        decode_fcn
    =
    input_element
      ~classnames:(if break then [ "break-grid" ] else [])
      ~required
      ~value:(smtp_auth |> CCOption.map_or ~default:"" decode_fcn)
      ?flash_fetcher
      language
      field_type
      field
  in
  let password =
    match smtp_auth with
    | Some _ -> txt ""
    | None -> input_element_root ~field_type:`Password Field.SmtpPassword (CCFun.const "")
  in
  div
    ~a:[ a_class [ "grid-col-2"; "flex-gap" ] ]
    [ input_element_root ~required:true Field.SmtpLabel (fun smtp ->
        smtp.label |> Label.value)
    ; input_element_root ~required:true Field.SmtpServer (fun smtp ->
        smtp.server |> Server.value)
    ; input_element_root ~required:true ~field_type:`Number Field.SmtpPort (fun smtp ->
        smtp.port |> Port.value |> CCInt.to_string)
    ; selector
        ~required:true
        language
        Field.SmtpMechanism
        Mechanism.show
        Mechanism.all
        (smtp_auth |> CCOption.map (fun smtp -> smtp.mechanism))
        ()
    ; selector
        ~required:true
        language
        Field.SmtpProtocol
        Protocol.show
        Protocol.all
        (smtp_auth |> CCOption.map (fun smtp -> smtp.protocol))
        ()
    ; input_element_root ~break:true Field.SmtpUsername (fun smtp ->
        smtp.username |> CCOption.map_or ~default:"" Username.value)
    ; password
    ; checkbox_element
        ~classnames:[ "break-grid" ]
        ?value:(smtp_auth |> CCOption.map (fun smtp -> smtp.default |> Default.value))
        ~hints:[ Pool_common.I18n.SmtpSettingsDefaultFlag ]
        language
        Field.DefaultSmtpServer
    ]
;;

let show Pool_context.{ language; csrf; flash_fetcher; _ } location smtp_auth =
  let open SmtpAuth in
  let action_path sub =
    Sihl.Web.externalize_path
      (Format.asprintf "%s/%s%s" (base_path location) (smtp_auth |> id |> Id.value) sub)
  in
  let submit ?submit_type ?(has_icon = Icon.Save) ?(control = Control.Update None) () =
    div
      ~a:[ a_class [ "flexrow" ] ]
      [ submit_element ?submit_type ~has_icon ~classnames:[ "push" ] language control () ]
  in
  let form_attrs action_path =
    [ a_method `Post
    ; a_action action_path
    ; a_class [ "stack" ]
    ; a_user_data "detect-unsaved-changes" ""
    ]
  in
  let smtp_details =
    div
      [ form
          ~a:(action_path "" |> form_attrs)
          [ csrf_element csrf ()
          ; smtp_form_inputs ?flash_fetcher language (Some smtp_auth)
          ; submit ()
          ]
      ]
  in
  let smtp_password =
    div
      ~a:[ a_class [ "stack" ] ]
      [ h2 ~a:[ a_class [ "heading-2"; "has-gap" ] ] [ txt "Update or Delete Password" ]
      ; form
          ~a:(action_path "/password" |> form_attrs)
          [ csrf_element csrf ()
          ; input_element ~value:"" ~required:true language `Password Field.SmtpPassword
          ; submit ()
          ]
      ; form
          ~a:(action_path "/password" |> form_attrs)
          [ csrf_element csrf ()
          ; input_element ~value:"" language `Hidden Field.SmtpPassword
          ; submit
              ~submit_type:`Error
              ~has_icon:Icon.Trash
              ~control:(Control.Delete (Some Field.Password))
              ()
          ]
      ]
  in
  let validate_smtp_settings =
    div
      [ h2 [ txt Pool_common.(Utils.text_to_string language I18n.Validation) ]
      ; p [ txt Pool_common.(Utils.hint_to_string language I18n.SmtpValidation) ]
      ; form
          ~a:(action_path "/validate" |> form_attrs)
          [ csrf_element csrf ()
          ; input_element ~required:true ?flash_fetcher language `Email Field.EmailAddress
          ; submit ~control:Control.Validate ()
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt "Email Server Settings (SMTP)" ]
    ; p
        [ Pool_common.(Utils.hint_to_string language I18n.SmtpSettingsIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ smtp_details; smtp_password; validate_smtp_settings ]
    ]
;;

(* TODO: Add option to force default *)
let smtp_create_form Pool_context.{ language; csrf; flash_fetcher; _ } location =
  let action_path =
    location |> base_path |> Format.asprintf "%s/create" |> Sihl.Web.externalize_path
  in
  let submit () =
    div
      ~a:[ a_class [ "flexrow" ] ]
      [ submit_element
          ~has_icon:Icon.Save
          ~classnames:[ "push" ]
          language
          (Control.Create None)
          ()
      ]
  in
  let create_form =
    form
      ~a:
        [ a_method `Post
        ; a_action action_path
        ; a_class [ "stack" ]
        ; a_user_data "detect-unsaved-changes" ""
        ]
      ([ csrf_element csrf (); smtp_form_inputs ?flash_fetcher language None ]
       @ [ h3
             ~a:[ a_class [ "has-gap" ] ]
             [ txt Pool_common.(Utils.text_to_string language I18n.Validation) ]
         ; p [ txt Pool_common.(Utils.hint_to_string language I18n.SmtpValidation) ]
         ; input_element ?flash_fetcher ~required:true language `Email Field.EmailAddress
         ; submit ()
         ])
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt "Configure Email Server (SMTP)" ]
    ; p
        [ Pool_common.(Utils.hint_to_string language I18n.SmtpSettingsIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; div ~a:[ a_class [ "stack" ] ] [ div [ create_form ] ]
    ]
;;
