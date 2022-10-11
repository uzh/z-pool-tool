module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let get_option_id req =
  HttpUtils.get_field_router_param
    req
    Pool_common.Message.Field.CustomFieldOption
  |> Custom_field.Id.of_string
;;

let get_field_id req =
  HttpUtils.get_field_router_param req Pool_common.Message.Field.CustomField
  |> Custom_field.Id.of_string
;;

let form ?id req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/custom-fields")
    @@ let* custom_field_option =
         id
         |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
              Custom_field.find_option tenant_db id >|= CCOption.pure)
       in
       let* custom_field = req |> get_field_id |> Custom_field.find tenant_db in
       let%lwt sys_languages = Settings.find_languages tenant_db in
       Page.Admin.CustomFieldOptions.detail
         ?custom_field_option
         custom_field
         context
         sys_languages
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req = form req

let edit req =
  let id = req |> get_option_id in
  form ~id req
;;

let write ?id req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let custom_field_id = req |> get_field_id in
  let base_path =
    Format.asprintf
      "/admin/custom-fields/%s"
      (Custom_field.Id.value custom_field_id)
  in
  let redirect_path = Format.asprintf "%s/%s" base_path "edit" in
  let error_path =
    match id with
    | None -> Format.asprintf "%s/options/new" base_path
    | Some id ->
      Format.asprintf "%s/options/%s/edit" base_path (Custom_field.Id.value id)
  in
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
    @@ let* custom_field = custom_field_id |> Custom_field.find tenant_db in
       let events =
         let open Lwt_result.Syntax in
         let%lwt sys_languages = Settings.find_languages tenant_db in
         match id with
         | None ->
           Cqrs_command.Custom_field_option_command.Create.handle
             sys_languages
             custom_field
             field_names
           |> Lwt_result.lift
         | Some id ->
           let* custom_field_option = Custom_field.find_option tenant_db id in
           Cqrs_command.Custom_field_option_command.Update.handle
             sys_languages
             custom_field_option
             field_names
           |> Lwt_result.lift
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:
                 [ Pool_common.Message.(Created Field.CustomFieldOption) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let create req = write req

let update req =
  let id = req |> get_option_id in
  write ~id req
;;

let delete req =
  let id = req |> get_option_id in
  let custom_field_id = req |> get_field_id in
  let result { Pool_context.tenant_db; _ } =
    let redirect_path =
      custom_field_id
      |> Custom_field.Id.value
      |> Format.asprintf "/admin/custom-fields/%s/edit"
    in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let open Utils.Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let* events =
      id
      |> Custom_field.find_option tenant_db
      >>= fun o ->
      o
      |> Cqrs_command.Custom_field_option_command.Destroy.handle
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set
          ~success:[ Pool_common.Message.(Deleted Field.CustomFieldOption) ]
      ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;
