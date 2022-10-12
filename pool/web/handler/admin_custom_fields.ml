module HttpUtils = Http_utils
module Message = Pool_common.Message

let create_layout req = General.create_tenant_layout `Admin req

let boolean_fields =
  Custom_field.boolean_fields |> CCList.map Message.Field.show
;;

let model_from_router req =
  let open Custom_field in
  let open CCResult in
  HttpUtils.get_field_router_param_opt req Message.Field.Model
  |> CCOption.to_result Message.(NotFound Field.Model)
  >>= fun s -> s |> Model.create
;;

let with_model req fnc =
  let model = model_from_router req in
  match model with
  | Ok model -> model |> fnc
  | Error err ->
    Http_utils.redirect_to_with_actions
      "/admin/custom-fields"
      [ HttpUtils.Message.set ~error:[ err ] ]
;;

let find_assocs_in_urlencoded urlencoded field encoder =
  let field = Message.Field.show field in
  CCList.filter_map
    (fun (key, values) ->
      let group, id = CCString.take_drop (CCString.length field) key in
      let value = CCList.head_opt values in
      let key =
        let open CCOption in
        id
        |> CCString.chop_prefix ~pre:"["
        >>= CCString.chop_suffix ~suf:"]"
        >>= encoder
      in
      match key, value with
      | Some l, Some v when CCString.equal field group -> Some (l, v)
      | _ -> None)
    urlencoded
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let open Custom_field in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let* model = model_from_router req |> Lwt_result.lift in
       let%lwt field_list = find_by_model tenant_db model in
       Page.Admin.CustomFields.index field_list model context
       |> create_layout ~active_navigation:"/admin/custom-fields" req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let redirect _ =
  let open Custom_field in
  Model.all
  |> CCList.head_opt
  |> CCOption.map_or ~default:"/admin/dashboard" (fun model ->
       model |> Model.show |> Format.asprintf "/admin/custom-fields/%s")
  |> Http_utils.redirect_to
;;

let form ?id req model =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err ->
      ( err
      , model
        |> Custom_field.Model.show
        |> Format.asprintf "/admin/custom-fields/%s" ))
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* custom_field =
      id
      |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
           Custom_field.find tenant_db id >|= CCOption.pure)
    in
    let%lwt sys_languages = Settings.find_languages tenant_db in
    Page.Admin.CustomFields.detail
      ?custom_field
      model
      context
      sys_languages
      flash_fetcher
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req = with_model req (form req)

let edit req =
  let id =
    HttpUtils.get_field_router_param req Message.Field.CustomField
    |> Custom_field.Id.of_string
  in
  with_model req (form ~id req)
;;

let write ?id req model =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values boolean_fields
    ||> HttpUtils.remove_empty_values
  in
  let field_names, field_hints, validations =
    let open Pool_common in
    let encode_lang t = t |> Language.create |> CCResult.to_opt in
    let go field = find_assocs_in_urlencoded urlencoded field in
    ( go Message.Field.Name encode_lang
    , go Message.Field.Hint encode_lang
    , go Message.Field.Validation CCOption.pure )
  in
  let redirect_path =
    Format.asprintf "/admin/custom-fields/%s" (Custom_field.Model.show model)
  in
  let error_path =
    match id with
    | None -> Format.asprintf "%s/field/new" redirect_path
    | Some id ->
      Format.asprintf
        "%s/field/%s/edit"
        redirect_path
        (Custom_field.Id.value id)
  in
  let result { Pool_context.tenant_db; _ } =
    Lwt_result.map_error (fun err ->
      err, error_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let events =
      let open Lwt_result.Syntax in
      let%lwt sys_languages = Settings.find_languages tenant_db in
      let* decoded =
        urlencoded
        |> Cqrs_command.Custom_field_command.base_decode
        |> Lwt_result.lift
      in
      match id with
      | None ->
        Cqrs_command.Custom_field_command.Create.handle
          sys_languages
          model
          field_names
          field_hints
          validations
          decoded
        |> Lwt_result.lift
      | Some id ->
        let* custom_field = Custom_field.find tenant_db id in
        Cqrs_command.Custom_field_command.Update.handle
          sys_languages
          custom_field
          field_names
          field_hints
          validations
          decoded
        |> Lwt_result.lift
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set ~success:[ Message.(Created Field.CustomField) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let create req = with_model req (write req)

let update req =
  let id =
    HttpUtils.get_field_router_param req Message.Field.CustomField
    |> Custom_field.Id.of_string
  in
  with_model req (write ~id req)
;;

let sort_options req =
  let handler req model =
    let open Utils.Lwt_result.Infix in
    let open Lwt_result.Syntax in
    let custom_field_id =
      HttpUtils.get_field_router_param req Message.Field.CustomField
      |> Custom_field.Id.of_string
    in
    let redirect_path =
      Format.asprintf
        "/admin/custom-fields/%s/field/%s/edit"
        (Custom_field.Model.show model)
        (Custom_field.Id.value custom_field_id)
    in
    let result { Pool_context.tenant_db; _ } =
      Lwt_result.map_error (fun err -> err, redirect_path, [])
      @@ let* custom_field = custom_field_id |> Custom_field.find tenant_db in
         let%lwt ids =
           Sihl.Web.Request.urlencoded_list
             Message.Field.(CustomFieldOption |> array_key)
             req
         in
         let%lwt options =
           let open Lwt.Infix in
           let open Custom_field in
           find_options_by_field tenant_db (id custom_field)
           >|= fun options ->
           CCList.filter_map
             (fun id ->
               CCList.find_opt
                 SelectOption.(
                   fun (option : t) -> Id.equal (Id.of_string id) option.id)
                 options)
             ids
         in
         let events =
           options
           |> Cqrs_command.Custom_field_command.SortOptions.handle
           |> Lwt_result.lift
         in
         let handle events =
           let%lwt (_ : unit list) =
             Lwt_list.map_s (Pool_event.handle_event tenant_db) events
           in
           Http_utils.redirect_to_with_actions
             redirect_path
             [ HttpUtils.Message.set
                 ~success:[ Message.(Updated Field.CustomField) ]
             ]
         in
         events |>> handle
    in
    result |> HttpUtils.extract_happy_path_with_actions req
  in
  with_model req (handler req)
;;
