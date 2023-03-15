open Tyxml.Html
open Component.Input
module Table = Component.Table
module File = Pool_common.File
module Id = Pool_tenant.Id
module Message = Pool_common.Message

let list tenant_list Pool_context.{ language; csrf; _ } =
  let build_tenant_rows tenant_list =
    let thead =
      Pool_common.Message.
        [ Field.Tenant |> Table.field_to_txt language; txt "" ]
    in
    let open Pool_tenant in
    let body =
      CCList.map
        (fun (tenant : t) ->
          [ txt (tenant.title |> Title.value)
          ; a
              ~a:
                [ a_href
                    (Sihl.Web.externalize_path
                       (Format.asprintf "/root/tenants/%s" (Id.value tenant.id)))
                ]
              [ txt Pool_common.(Utils.control_to_string language Message.More)
              ]
          ])
        tenant_list
    in
    Table.horizontal_table `Striped ~thead ~align_last_end:true body
  in
  let tenant_list = build_tenant_rows tenant_list in
  let text_fields =
    Message.Field.[ Title; Description; Url; DatabaseUrl; DatabaseLabel ]
  in
  let input_fields =
    let language_select =
      let open Pool_common.Language in
      selector language Message.Field.Language show all None ()
    in
    let open Message in
    CCList.map (input_element language `Text) text_fields
    @ [ language_select ]
    @ CCList.map (input_element_file language) [ Field.Styles; Field.Icon ]
    @ CCList.map
        (input_element_file language ~allow_multiple:true)
        [ Field.TenantLogos; Field.PartnerLogos ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Tenants) ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ tenant_list
        ; div
            [ h2
                ~a:[ a_class [ "heading-2" ] ]
                [ Pool_common.(
                    Utils.control_to_string
                      language
                      Message.(Create (Some Field.Tenant)))
                  |> txt
                ]
            ; form
                ~a:
                  [ a_action (Sihl.Web.externalize_path "/root/tenants/create")
                  ; a_method `Post
                  ; a_enctype "multipart/form-data"
                  ; a_class [ "stack" ]
                  ]
                ((csrf_element csrf () :: input_fields)
                 @ [ div
                       ~a:[ a_class [ "flexrow" ] ]
                       [ submit_element
                           ~classnames:[ "push" ]
                           language
                           Message.(Create None)
                           ()
                       ]
                   ])
            ]
        ]
    ]
;;

let manage_operators { Pool_tenant.id; _ } Pool_context.{ language; csrf; _ } =
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(
              Utils.field_to_string language Message.Field.Operators
              |> CCString.capitalize_ascii)
        ]
    ; div
        [ form
            ~a:
              [ a_action
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/root/tenants/%s/create-operator"
                        (Pool_tenant.Id.value id)))
              ; a_method `Post
              ; a_class [ "stack" ]
              ]
            ((csrf_element csrf ()
              :: CCList.map
                   (fun (field, input) ->
                     input_element ~required:true language input field)
                   Message.Field.
                     [ Email, `Email
                     ; Password, `Password
                     ; Firstname, `Text
                     ; Lastname, `Text
                     ])
             @ [ div
                   ~a:[ a_class [ "flexrow"; "align-center"; "flex-gap" ] ]
                   [ div
                       [ a
                           ~a:
                             [ a_href
                                 (Sihl.Web.externalize_path
                                    (Format.asprintf
                                       "/root/tenants/%s"
                                       (Id.value id)))
                             ]
                           [ txt
                               Pool_common.(
                                 Utils.control_to_string language Message.back)
                           ]
                       ]
                   ; submit_element
                       ~classnames:[ "push" ]
                       language
                       Message.(Create (Some Field.operator))
                       ()
                   ]
               ])
        ]
    ]
;;

let detail (tenant : Pool_tenant.t) Pool_context.{ language; csrf; _ } =
  let open Pool_tenant in
  let control_to_string = Pool_common.Utils.control_to_string language in
  let detail_fields =
    Message.
      [ Field.Title, Title.value tenant.title
      ; ( Field.Description
        , tenant.description |> CCOption.map_or ~default:"" Description.value )
      ; Field.Url, Url.value tenant.url
      ]
  in
  let database_fields =
    Message.
      [ Field.DatabaseUrl, ""
      ; Field.DatabaseLabel, Pool_database.Label.value tenant.database_label
      ]
  in
  let to_input_element (field, value) =
    input_element language `Text field ~value
  in
  let language_select =
    let open Pool_common.Language in
    selector
      language
      Message.Field.Language
      show
      all
      (Some tenant.default_language)
      ()
  in
  let detail_input_fields =
    (CCList.map to_input_element detail_fields @ [ language_select ])
    @ [ div
          [ input_element_file language Message.Field.Styles
          ; div
              ~a:[ a_class [ "gap-xs" ] ]
              [ a
                  ~a:
                    [ a_href
                        (File.path (tenant.styles |> Pool_tenant.Styles.value))
                    ]
                  [ txt "Download" ]
              ]
          ]
      ; div
          [ input_element_file language Message.Field.Icon
          ; div
              ~a:[ a_class [ "gap-xs" ] ]
              [ a
                  ~a:
                    [ a_href (File.path (tenant.icon |> Pool_tenant.Icon.value))
                    ]
                  [ txt "Download" ]
              ]
          ]
      ; input_element_file
          ~allow_multiple:true
          language
          Message.Field.TenantLogos
      ; input_element_file
          ~allow_multiple:true
          language
          Message.Field.PartnerLogos
      ]
  in
  let database_input_fields = CCList.map to_input_element database_fields in
  let disabled =
    checkbox_element
      language
      Message.Field.TenantDisabledFlag
      ~value:(tenant.disabled |> Disabled.value)
  in
  let delete_img_form files =
    div
      ~a:[ a_class [ "flexrow" ] ]
      (CCList.map
         (fun (file : File.t) ->
           div
             [ div
                 ~a:
                   [ a_class [ "aspect-ratio"; "contain" ]
                   ; a_style "width: 200px"
                   ]
                 [ img ~src:(File.path file) ~alt:"" () ]
             ; form
                 ~a:
                   [ a_action
                       (Sihl.Web.externalize_path
                          (Format.asprintf
                             "/root/tenants/%s/assets/%s/delete"
                             (tenant.id |> Id.value)
                             (File.id file |> Pool_common.Id.value)))
                   ; a_method `Post
                   ; a_class [ "stack" ]
                   ]
                 [ csrf_element csrf ()
                 ; submit_element
                     language
                     Message.(Delete (Some Field.file))
                     ~submit_type:`Error
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
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ Message.Field.TenantLogos |> label_text |> txt ]
      ; delete_img_form (tenant.logos |> Pool_tenant.Logos.value)
      ; h3
          ~a:[ a_class [ "heading-3" ] ]
          [ Message.Field.PartnerLogos |> label_text |> txt ]
      ; delete_img_form (tenant.partner_logo |> Pool_tenant.PartnerLogos.value)
      ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (tenant.Pool_tenant.title |> Pool_tenant.Title.value) ]
    ; p
        [ a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "/root/tenants/%s/operator"
                        (tenant.id |> Id.value)))
              ]
            [ txt
                (control_to_string Pool_common.Message.(Manage Field.Operators))
            ]
        ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ form
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
            ((csrf_element csrf () :: detail_input_fields)
             @ [ disabled
               ; div
                   ~a:[ a_class [ "flexrow" ] ]
                   [ submit_element
                       ~classnames:[ "push" ]
                       language
                       Message.(Update None)
                       ()
                   ]
               ])
        ; delete_file_forms
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
            ((csrf_element csrf () :: database_input_fields)
             @ [ div
                   ~a:[ a_class [ "flexrow" ] ]
                   [ submit_element
                       ~classnames:[ "push" ]
                       language
                       Message.(Update None)
                       ()
                   ]
               ])
        ; p
            [ a
                ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
                [ txt (control_to_string Pool_common.Message.back) ]
            ]
        ]
    ]
;;
