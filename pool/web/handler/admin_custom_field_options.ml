module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Url = Page.Admin.CustomFields.Url

let create_layout req = General.create_tenant_layout req

let get_option_id req =
  HttpUtils.get_field_router_param
    req
    Pool_common.Message.Field.CustomFieldOption
  |> Custom_field.SelectOption.Id.of_string
;;

let get_field_id req =
  HttpUtils.get_field_router_param req Pool_common.Message.Field.CustomField
  |> Custom_field.Id.of_string
;;

let get_custom_field fnc req =
  let%lwt custom_field =
    let open Lwt_result.Infix in
    Pool_context.find req
    |> Lwt_result.lift
    >>= fun { Pool_context.tenant_db; _ } ->
    Custom_field.find tenant_db (req |> get_field_id)
  in
  match custom_field with
  | Ok field -> field |> fnc req
  | Error err ->
    Http_utils.redirect_to_with_actions
      Url.fallback_path
      [ HttpUtils.Message.set ~error:[ err ] ]
;;

let form ?id req custom_field =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err ->
      ( err
      , Url.Field.edit_path Custom_field.(model custom_field, id custom_field) ))
    @@ let* custom_field_option =
         id
         |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
              Custom_field.find_option tenant_db id >|= CCOption.pure)
       in
       let* custom_field = req |> get_field_id |> Custom_field.find tenant_db in
       let* sys_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       Page.Admin.CustomFieldOptions.detail
         ?custom_field_option
         custom_field
         context
         sys_languages
         flash_fetcher
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req = get_custom_field form req

let edit req =
  let id = req |> get_option_id in
  get_custom_field (form ~id) req
;;

let write ?id req custom_field =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let url_data = Custom_field.(model custom_field, id custom_field) in
  let redirect_path = Url.Field.edit_path url_data in
  let error_path =
    match id with
    | None -> Url.Option.new_path url_data
    | Some id -> Url.Option.edit_path url_data id
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
    @@
    let events =
      let* sys_languages =
        Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
      in
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
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      let success =
        let open Pool_common.Message in
        if CCOption.is_some id
        then Updated Field.CustomFieldOption
        else Created Field.CustomFieldOption
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let create req = get_custom_field write req

let update req =
  let id = req |> get_option_id in
  get_custom_field (write ~id) req
;;

let delete req =
  let handler req custom_field =
    let id = req |> get_option_id in
    let result { Pool_context.tenant_db; _ } =
      let redirect_path =
        Url.Field.edit_path Custom_field.(model custom_field, id custom_field)
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
  in
  get_custom_field handler req
;;
