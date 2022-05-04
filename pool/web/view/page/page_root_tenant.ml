open Tyxml.Html
module File = Pool_common.File
module Id = Pool_common.Id
module Message = Pool_common.Message

let submit_element = Component.submit_element

let list csrf tenant_list root_list message Pool_context.{ language; _ } =
  let input_element = Component.input_element language `Text |> CCFun.flip in
  let input_element_file ?(allow_multiple = false) field =
    let field_label =
      Pool_common.Utils.field_to_string language field
      |> CCString.capitalize_ascii
    in
    div
      [ label [ txt field_label ]
      ; input
          ~a:
            [ a_input_type `File
            ; a_name Message.Field.(field |> show)
            ; (if allow_multiple then a_multiple () else a_value "")
            ]
          ()
      ]
  in
  let build_tenant_rows tenant_list =
    let open Pool_tenant in
    CCList.map
      (fun (tenant : Pool_tenant.t) ->
        div
          [ h2 [ txt (tenant.title |> Pool_tenant.Title.value) ]
          ; a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf "/root/tenants/%s" (Id.value tenant.id)))
                ]
              [ txt "detail" ]
          ; hr ()
          ])
      tenant_list
  in
  let build_root_rows root_list =
    let open Sihl.Contract.User in
    let status_toggle (status : Sihl.Contract.User.status) id =
      let text, style =
        match status with
        | Active -> Message.Disable, "button--warning"
        | Inactive -> Message.Enable, "button--primary"
      in
      form
        ~a:
          [ a_action
              (Sihl.Web.externalize_path
                 (Format.asprintf "/root/root/%s/toggle-status" id))
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ submit_element language text ~classnames:[ style ] () ]
    in
    CCList.map
      (fun root ->
        let user = root |> Root.user in
        let status = status_toggle user.status user.id in
        div [ h2 [ txt user.email ]; status; hr () ])
      root_list
  in
  let tenant_list = build_tenant_rows tenant_list in
  let root_list = build_root_rows root_list in
  let text_fields =
    Message.Field.
      [ Title
      ; Description
      ; Url
      ; DatabaseUrl
      ; DatabaseLabel
      ; SmtpAuthServer
      ; SmtpPort
      ; SmtpUsername
      ; SmtpPassword
      ; SmtpAuthMethod
      ; SmtpProtocol
      ]
  in
  let input_fields =
    let open Message in
    CCList.map (input_element "") text_fields
    @ [ Component.language_select (Pool_common.Language.all ()) None () ]
    @ CCList.map input_element_file [ Field.Styles; Field.Icon ]
    @ CCList.map
        (input_element_file ~allow_multiple:true)
        [ Field.TenantLogos; Field.PartnerLogos ]
  in
  let html =
    div
      [ h1 [ txt "Tenants" ]
      ; div tenant_list
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/tenants/create")
            ; a_method `Post
            ; a_enctype "multipart/form-data"
            ; a_class [ "stack" ]
            ]
          ((Component.csrf_element csrf () :: input_fields)
          @ [ submit_element
                language
                Message.(Create None)
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; hr ()
      ; h1 [ txt "Root users" ]
      ; div root_list
      ; form
          ~a:
            [ a_action (Sihl.Web.externalize_path "/root/root/create")
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          (CCList.map
             (input_element "")
             Message.Field.[ Email; Password; Firstname; Lastname ]
          @ [ submit_element
                language
                Message.(Create (Some Field.root))
                ~classnames:[ "button--primary" ]
                ()
            ])
      ]
  in
  Page_layout.create_root_layout
    html
    message
    language
    ~active_navigation:"/root/tenants"
    ()
;;

let detail (tenant : Pool_tenant.t) Pool_context.{ language; csrf; message; _ } =
  let open Pool_tenant in
  let open Pool_tenant.SmtpAuth in
  let input_element = Component.input_element language `Text in
  let input_element_file ?(allow_multiple = false) ?file_href field =
    let field_label =
      Pool_common.Utils.field_to_string language field
      |> CCString.capitalize_ascii
    in
    div
      [ (match allow_multiple, file_href with
        | false, Some file_href -> a ~a:[ a_href file_href ] [ txt field_label ]
        | _, _ -> label [ txt field_label ])
      ; input
          ~a:
            ([ a_input_type `File; a_name Message.Field.(field |> show) ]
            @ if allow_multiple then [ a_multiple () ] else [])
          ()
      ]
  in
  let detail_fields =
    Message.
      [ Field.Title, Title.value tenant.title
      ; Field.Description, Description.value tenant.description
      ; Field.Url, Pool_tenant.Url.value tenant.url
      ; Field.SmtpAuthServer, Server.value tenant.smtp_auth.server
      ; Field.SmtpPort, Port.value tenant.smtp_auth.port
      ; Field.SmtpUsername, Username.value tenant.smtp_auth.username
      ; ( Field.SmtpAuthMethod
        , AuthenticationMethod.value tenant.smtp_auth.authentication_method )
      ; Field.SmtpProtocol, Protocol.value tenant.smtp_auth.protocol
      ]
  in
  let database_fields =
    Message.
      [ Field.DatabaseUrl, ""
      ; Field.DatabaseLabel, Pool_database.Label.value tenant.database_label
      ]
  in
  let detail_input_fields =
    (CCList.map (CCFun.uncurry input_element) detail_fields
    @ [ Component.language_select
          (Pool_common.Language.all ())
          (Some tenant.default_language)
          ()
      ])
    @ [ input_element_file
          ~file_href:(File.path (tenant.styles |> Pool_tenant.Styles.value))
          Message.Field.Styles
      ; input_element_file
          ~file_href:(File.path (tenant.icon |> Pool_tenant.Icon.value))
          Message.Field.Icon
      ; input_element_file ~allow_multiple:true Message.Field.TenantLogos
      ; input_element_file ~allow_multiple:true Message.Field.PartnerLogos
      ]
  in
  let database_input_fields =
    CCList.map (CCFun.uncurry input_element) database_fields
  in
  let disabled =
    let label_text =
      Pool_common.Utils.field_to_string language Message.Field.Disabled
      |> CCString.capitalize_ascii
    in
    let attributes =
      match tenant.disabled |> Pool_tenant.Disabled.value with
      | true ->
        [ a_input_type `Checkbox
        ; a_name Message.Field.(Disabled |> show)
        ; a_checked ()
        ]
      | false ->
        [ a_input_type `Checkbox; a_name Message.Field.(Disabled |> show) ]
    in
    div [ label [ txt label_text ]; input ~a:attributes () ]
  in
  let delete_img_form files =
    div
      ~a:[ a_style "display: flex;" ]
      (CCList.map
         (fun (file : File.t) ->
           div
             [ img
                 ~src:(File.path file)
                 ~alt:""
                 ~a:[ a_style "width: 200px" ]
                 ()
             ; form
                 ~a:
                   [ a_action
                       (Sihl.Web.externalize_path
                          (Format.asprintf
                             "/root/tenants/%s/assets/%s/delete"
                             (tenant.id |> Id.value)
                             (File.id file |> Id.value)))
                   ; a_method `Post
                   ; a_class [ "stack" ]
                   ]
                 [ Component.csrf_element csrf ()
                 ; submit_element
                     language
                     Message.(Delete (Some Field.file))
                     ~classnames:[ "button--failure" ]
                     ()
                 ]
             ])
         files)
  in
  let delete_file_forms =
    let label_text m =
      m
      |> Pool_common.Utils.field_to_string language
      |> CCString.capitalize_ascii
    in
    div
      [ h3 [ Message.Field.TenantLogos |> label_text |> txt ]
      ; delete_img_form (tenant.logos |> Pool_tenant.Logos.value)
      ; h3 [ Message.Field.PartnerLogos |> label_text |> txt ]
      ; delete_img_form (tenant.partner_logo |> Pool_tenant.PartnerLogos.value)
      ]
  in
  let html =
    div
      [ h1 [ txt (tenant.Pool_tenant.title |> Pool_tenant.Title.value) ]
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenants/%s/update-detail"
                      (Id.value tenant.id)))
            ; a_method `Post
            ; a_enctype "multipart/form-data"
            ; a_class [ "stack" ]
            ]
          ((Component.csrf_element csrf () :: detail_input_fields)
          @ [ disabled
            ; submit_element
                language
                Message.(Update None)
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; hr ()
      ; delete_file_forms
      ; hr ()
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenants/%s/update-database"
                      (Id.value tenant.id)))
            ; a_method `Post
            ; a_enctype "multipart/form-data"
            ; a_class [ "stack" ]
            ]
          ((Component.csrf_element csrf () :: database_input_fields)
          @ [ submit_element
                language
                Message.(Update None)
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; hr ()
      ; form
          ~a:
            [ a_action
                (Sihl.Web.externalize_path
                   (Format.asprintf
                      "/root/tenants/%s/create-operator"
                      (Id.value tenant.id)))
            ; a_method `Post
            ; a_class [ "stack" ]
            ]
          ((Component.csrf_element csrf ()
           :: CCList.map
                (CCFun.flip input_element "")
                Message.Field.[ Email; Password; Firstname; Lastname ])
          @ [ submit_element
                language
                Message.(Create (Some Field.operator))
                ~classnames:[ "button--primary" ]
                ()
            ])
      ; a
          ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
          [ txt "back" ]
      ]
  in
  Page_layout.create_root_layout html message language ()
;;
