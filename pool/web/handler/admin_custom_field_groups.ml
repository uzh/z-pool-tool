module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let get_group_id req =
  HttpUtils.get_field_router_param
    req
    Pool_common.Message.Field.CustomFieldGroup
  |> Custom_field.Group.Id.of_string
;;

let model_from_query = Admin_custom_fields.model_from_query

let form ?id req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/custom-fields")
    @@ let* custom_field_group =
         id
         |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
              Custom_field.find_group tenant_db id >|= CCOption.pure)
       in
       let query_model = model_from_query req in
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let%lwt sys_languages = Settings.find_languages tenant_db in
       Page.Admin.CustomFieldGroups.detail
         ?custom_field_group
         ?query_model
         context
         sys_languages
         flash_fetcher
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req = form req

let edit req =
  let id = req |> get_group_id in
  form ~id req
;;

let write ?id req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let error_path = Format.asprintf "/admin/custom-fields" in
  let field_names =
    let open Pool_common in
    let encode_lang t = t |> Language.create |> CCResult.to_opt in
    let go field =
      Admin_custom_fields.find_assocs_in_urlencoded urlencoded field
    in
    go Message.Field.Name encode_lang
  in
  let result { Pool_context.tenant_db; _ } =
    Lwt_result.map_error (fun err ->
      err, error_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@ let%lwt urlencoded =
         Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
       in
       let events =
         let%lwt sys_languages = Settings.find_languages tenant_db in
         match id with
         | None ->
           let open CCResult in
           Cqrs_command.Custom_field_group_command.(
             urlencoded
             |> default_decode
             >>= Create.handle sys_languages field_names
             |> Lwt_result.lift)
         | Some id ->
           let* custom_field_group = id |> Custom_field.find_group tenant_db in
           let open CCResult in
           Cqrs_command.Custom_field_group_command.(
             urlencoded
             |> default_decode
             >>= Update.handle sys_languages custom_field_group field_names
             |> Lwt_result.lift)
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           error_path
           [ Message.set
               ~success:[ Pool_common.Message.(Created Field.CustomFieldGroup) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let create req = write req

let update req =
  let id = req |> get_group_id in
  write ~id req
;;

let delete req =
  let id = req |> get_group_id in
  let result { Pool_context.tenant_db; _ } =
    let redirect_path =
      id
      |> Custom_field.Group.Id.value
      |> Format.asprintf "/admin/custom-fields/custom_field_groups/%s/edit"
    in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let open Utils.Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let* events =
      id
      |> Custom_field.find_group tenant_db
      >>= fun g ->
      g
      |> Cqrs_command.Custom_field_group_command.Destroy.handle
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set
          ~success:[ Pool_common.Message.(Deleted Field.CustomFieldGroup) ]
      ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;
