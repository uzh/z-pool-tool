open Tyxml.Html
open Component.Input
module Table = Component.Table
module File = Pool_common.File
module Id = Pool_tenant.Id
module Message = Pool_common.Message

let database_fields tenant language flash_fetcher =
  let fields =
    Message.
      [ Field.DatabaseUrl, ""
      ; ( Field.DatabaseLabel
        , tenant
          |> CCOption.map_or ~default:"" (fun t ->
               t.Pool_tenant.database_label |> Pool_database.Label.value) )
      ]
  in
  fields
  |> CCList.map (fun (field, value) ->
       input_element language `Text field ~value ~flash_fetcher ~required:true)
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
      , Message.(Update (Some Field.Tenant)) )
    | None -> "/root/tenants/create", Message.(Create (Some Field.Tenant))
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") tenant in
  let language_select =
    let open Pool_common.Language in
    selector language Message.Field.Language show all None ()
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
               [ a ~a:[ a_href (File.path file) ] [ txt "Download" ] ]
         in
         div
           [ input_element_file language field ~allow_multiple ~required
           ; download
           ])
  in
  let gtx_api_key_input =
    if CCOption.is_none tenant
    then
      input_element language `Text Field.GtxApiKey ~flash_fetcher ~required:true
    else txt ""
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
        ~value:(value (fun t -> t.url |> Url.value))
        ~flash_fetcher
        ~required:true
    ; gtx_api_key_input
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
            ; tenant_form context flash_fetcher
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

let update_gtx_api_key_form
  (tenant : Pool_tenant.t)
  Pool_context.{ language; csrf; _ }
  flash_fetcher
  =
  let action =
    Format.asprintf
      "/root/tenants/%s/update-gtx-api-key"
      (Id.value tenant.Pool_tenant.id)
  in
  form
    ~a:
      [ a_action action
      ; a_method `Post
      ; a_enctype "multipart/form-data"
      ; a_class [ "stack" ]
      ]
    [ csrf_element csrf ()
    ; input_element language `Text Field.GtxApiKey ~flash_fetcher ~required:true
    ; submit_element language Message.(Update (Some Field.GtxApiKey)) ()
    ]
  |> tenant_detail_sub_form language Message.Field.GtxApiKey
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
    div
      [ delete_img_form (tenant.logos |> Pool_tenant.Logos.value)
        |> tenant_detail_sub_form language Message.Field.TenantLogos
      ; delete_img_form (tenant.partner_logo |> Pool_tenant.PartnerLogos.value)
        |> tenant_detail_sub_form language Message.Field.PartnerLogos
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
                 Message.(Update None)
                 ()
             ]
         ])
    |> tenant_detail_sub_form language Message.Field.Database
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
        [ tenant_form ~tenant context flash_fetcher
        ; delete_file_forms
        ; database_form
        ; update_gtx_api_key_form tenant context flash_fetcher
        ; p
            [ a
                ~a:[ a_href (Sihl.Web.externalize_path "/root/tenants") ]
                [ txt (control_to_string Pool_common.Message.back) ]
            ]
        ]
    ]
;;
