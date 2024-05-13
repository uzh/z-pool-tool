open Tyxml.Html
open Component.Input
open Pool_message
module Table = Component.Table
module File = Pool_common.File
module Id = Pool_tenant.Id

let database_fields tenant language flash_fetcher =
  let open Pool_common.I18n in
  let fields =
    [ Field.DatabaseUrl, "", TenantDatabaseUrl
    ; ( Field.DatabaseLabel
      , tenant
        |> CCOption.map_or ~default:"" (fun t ->
          t.Pool_tenant.database_label |> Database.Label.value)
      , TenantDatabaseLabel )
    ]
  in
  fields
  |> CCList.map (fun (field, value, help) ->
    input_element
      language
      `Text
      field
      ~hints:[ help ]
      ~value
      ~flash_fetcher
      ~required:true)
  |> div ~a:[ a_class [ "stack" ] ]
;;

let map_or = CCOption.map_or ~default:""

let tenant_form
  ?(tenant : Pool_tenant.t option)
  Pool_context.{ language; csrf; _ }
  flash_fetcher
  =
  let open Pool_tenant in
  let action, control =
    match tenant with
    | Some tenant ->
      ( Format.asprintf "/root/tenants/%s/update-detail" (Id.value tenant.id)
      , Control.(Update (Some Field.Tenant)) )
    | None -> "/root/tenants/create", Control.(Create (Some Field.Tenant))
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") tenant in
  let language_select =
    let open Pool_common.Language in
    selector language Field.Language show all None ()
  in
  let file_uploads =
    [ ( Field.Styles
      , CCOption.bind tenant (fun t -> t.styles |> CCOption.map Styles.value)
      , false
      , false )
    ; ( Field.Icon
      , CCOption.bind tenant (fun t -> t.icon |> CCOption.map Icon.value)
      , false
      , false )
    ; Field.TenantLogos, None, true, true
    ; Field.PartnerLogos, None, false, true
    ]
    |> CCList.map (fun (field, file, required, allow_multiple) ->
      let required = if CCOption.is_some tenant then false else required in
      let download =
        match file with
        | None -> txt ""
        | Some file ->
          div
            ~a:[ a_class [ "gap-xs" ] ]
            [ a ~a:[ a_href (File.externalized_path file) ] [ txt "Download" ] ]
      in
      div
        [ input_element_file language field ~allow_multiple ~required
        ; download
        ])
  in
  form
    ~a:
      [ a_method `Post
      ; a_action (Sihl.Web.externalize_path action)
      ; a_class [ "stack" ]
      ; a_enctype "multipart/form-data"
      ; a_user_data "detect-unsaved-changes" ""
      ]
    [ csrf_element csrf ()
    ; input_element
        language
        `Text
        Field.Title
        ~value:(value (fun t -> t.title |> Title.value))
        ~required:true
        ~flash_fetcher
    ; input_element
        language
        `Text
        Field.Description
        ~value:(value (fun t -> t.description |> map_or Description.value))
        ~flash_fetcher
    ; input_element
        language
        `Text
        Field.Url
        ~hints:Pool_common.I18n.[ TenantUrl ]
        ~value:(value (fun t -> t.url |> Url.value))
        ~flash_fetcher
        ~required:true
    ; language_select
    ; (if CCOption.is_some tenant
       then txt ""
       else database_fields tenant language flash_fetcher)
    ; div ~a:[ a_class [ "stack" ] ] file_uploads
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element ~classnames:[ "push" ] language control () ]
    ]
;;

let list tenant_list (Pool_context.{ language; _ } as context) flash_fetcher =
  let build_tenant_rows tenant_list =
    let thead = [ Field.Tenant |> Table.field_to_txt language; txt "" ] in
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
              [ txt Pool_common.(Utils.control_to_string language Control.More)
              ]
          ])
        tenant_list
    in
    Table.horizontal_table `Striped ~thead ~align_last_end:true body
  in
  let tenant_list = build_tenant_rows tenant_list in
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
                      Control.(Create (Some Field.Tenant)))
                  |> txt
                ]
            ; tenant_form context flash_fetcher
            ]
        ]
    ]
;;

let manage_operators
  { Pool_tenant.id; _ }
  operators
  Pool_context.{ language; csrf; _ }
  =
  let operator_list =
    Page_admin_admins.static_overview ~disable_edit:true language operators
  in
  let create_operator_form =
    div
      [ h2
          ~a:[ a_class [ "heading-2" ] ]
          [ txt
              Pool_common.(
                Utils.control_to_string
                  language
                  Control.(Create (Some Field.Operator)))
          ]
      ; form
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
                 Field.
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
                         [ Pool_common.Utils.control_to_string
                             language
                             Control.Back
                           |> txt
                         ]
                     ]
                 ; submit_element
                     ~classnames:[ "push" ]
                     language
                     Control.(Create (Some Field.operator))
                     ()
                 ]
             ])
      ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ Pool_common.Utils.field_to_string language Field.Operators
          |> CCString.capitalize_ascii
          |> txt
        ]
    ; div ~a:[ a_class [ "stack-lg" ] ] [ operator_list; create_operator_form ]
    ]
;;

let tenant_detail_sub_form language field form_html =
  div
    [ h3
        ~a:[ a_class [ "heading-3" ] ]
        [ Pool_common.(Utils.field_to_string language field)
          |> CCString.capitalize_ascii
          |> txt
        ]
    ; form_html
    ]
;;

let detail
  (tenant : Pool_tenant.t)
  (Pool_context.{ language; csrf; _ } as context)
  flash_fetcher
  =
  let open Pool_tenant in
  let control_to_string = Pool_common.Utils.control_to_string language in
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
                 [ img ~src:(File.externalized_path file) ~alt:"" () ]
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
                     Control.(Delete (Some Field.file))
                     ~submit_type:`Error
                     ()
                 ]
             ])
         files)
  in
  let delete_file_forms =
    div
      [ delete_img_form (tenant.logos |> Pool_tenant.Logos.value)
        |> tenant_detail_sub_form language Field.TenantLogos
      ; delete_img_form (tenant.partner_logo |> Pool_tenant.PartnerLogos.value)
        |> tenant_detail_sub_form language Field.PartnerLogos
      ]
  in
  let database_form =
    form
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
      ([ csrf_element csrf ()
       ; database_fields (Some tenant) language flash_fetcher
       ]
       @ [ div
             ~a:[ a_class [ "flexrow" ] ]
             [ submit_element
                 ~classnames:[ "push" ]
                 language
                 Control.(Update None)
                 ()
             ]
         ])
    |> tenant_detail_sub_form language Field.Database
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
            [ txt (control_to_string (Control.Manage Field.Operators)) ]
        ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ tenant_form ~tenant context flash_fetcher
        ; delete_file_forms
        ; database_form
        ; p
            [ a
                ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
                [ txt (control_to_string Control.Back) ]
            ]
        ]
    ]
;;
