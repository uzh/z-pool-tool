module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Url = Page.Admin.CustomFields.Url
module Field = Pool_message.Field
module Response = Http_response

let src = Logs.Src.create "handler.admin.custom_field_options"
let create_layout req = General.create_tenant_layout req

let get_option_id req =
  HttpUtils.get_field_router_param req Pool_message.Field.CustomFieldOption
  |> Custom_field.SelectOption.Id.of_string
;;

let get_field_id req =
  HttpUtils.get_field_router_param req Pool_message.Field.CustomField
  |> Custom_field.Id.of_string
;;

let custom_field_option_opt ?id database_label =
  let open Utils.Lwt_result.Infix in
  match id with
  | None -> Lwt_result.return None
  | Some id ->
    Custom_field.find_option database_label id >|+ CCOption.pure >|- Response.not_found
;;

(* let get_custom_field fnc req =
  let%lwt custom_field =
    let open Utils.Lwt_result.Infix in
    Pool_context.find req
    |> Lwt_result.lift
    >>= fun { Pool_context.database_label; _ } ->
    Custom_field.find database_label (req |> get_field_id)
  in
  match custom_field with
  | Ok field -> field |> fnc req
  | Error err ->
    Http_utils.redirect_to_with_actions
      Url.fallback_path
      [ HttpUtils.Message.set ~error:[ err ] ]
;; *)

let form ?id req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    let* custom_field =
      req |> get_field_id |> Custom_field.find database_label >|- Response.not_found
    in
    let* custom_field_option = custom_field_option_opt ?id database_label in
    Response.bad_request_render_error context
    @@
    let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    Page.Admin.CustomFieldOptions.detail
      ?custom_field_option
      custom_field
      context
      sys_languages
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let new_form req = form req

let edit req =
  let id = req |> get_option_id in
  form ~id req
;;

let write ?id req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let field_names =
    let open Pool_common in
    let encode_lang t = t |> Language.create |> CCResult.to_opt in
    let go field = Admin_custom_fields.find_assocs_in_urlencoded urlencoded field in
    go Pool_message.Field.Name encode_lang
  in
  let result { Pool_context.database_label; user; _ } =
    let* custom_field =
      req |> get_field_id |> Custom_field.find database_label >|- Response.not_found
    in
    let* custom_field_option = custom_field_option_opt ?id database_label in
    let url_data = Custom_field.(model custom_field, id custom_field) in
    let redirect_path = Url.Field.edit_path url_data in
    Response.bad_request_on_error ~urlencoded (form ?id)
    @@
    let events =
      let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
      match custom_field_option with
      | None ->
        Cqrs_command.Custom_field_option_command.Create.handle
          ~tags
          sys_languages
          custom_field
          field_names
        |> Lwt_result.lift
      | Some custom_field_option ->
        Cqrs_command.Custom_field_option_command.Update.handle
          ~tags
          sys_languages
          custom_field_option
          field_names
        |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      let success =
        let open Pool_message.Success in
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
  Response.handle ~src req result
;;

let create req = write req

let update req =
  let id = req |> get_option_id in
  write ~id req
;;

let toggle_action action req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let* custom_field =
      req |> get_field_id |> Custom_field.find database_label >|- Response.not_found
    in
    let* option =
      req
      |> get_option_id
      |> Custom_field.find_option database_label
      >|- Response.not_found
    in
    let redirect_path =
      Url.Field.edit_path Custom_field.(model custom_field, id custom_field)
    in
    (* TODO: Rerender form *)
    Response.bad_request_render_error context
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Custom_field_option_command in
      Lwt_result.lift
      @@
      match action with
      | `Delete -> Destroy.handle ~tags option
      | `Publish -> Publish.handle ~tags option
    in
    let success =
      let open Pool_message.Success in
      match action with
      | `Delete -> Deleted Field.CustomFieldOption
      | `Publish -> Published Field.CustomFieldOption
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let delete = toggle_action `Delete
let publish = toggle_action `Publish

let changelog req =
  let result (_ : Pool_context.t) =
    let open Utils.Lwt_result.Infix in
    let* model = Admin_custom_fields.model_from_router req |> Lwt_result.lift in
    let field_id = get_field_id req in
    let option_id = get_option_id req in
    let url =
      HttpUtils.Url.Admin.custom_field_option_path
        model
        field_id
        ~suffix:"changelog"
        ~id:option_id
        ()
    in
    let open Custom_field in
    Lwt_result.ok
    @@ Helpers.Changelog.htmx_handler ~url (SelectOption.Id.to_common option_id) req
  in
  HttpUtils.Htmx.handle_error_message ~error_as_notification:true req result
;;

module Access : sig
  include module type of Helpers.Access

  val publish : Rock.Middleware.t
end = struct
  include Helpers.Access
  module CustomFieldCommand = Cqrs_command.Custom_field_option_command

  let custom_field_effects =
    Middleware.Guardian.id_effects Custom_field.Id.validate Field.CustomField
  ;;

  let create =
    CustomFieldCommand.Create.effects |> Middleware.Guardian.validate_admin_entity
  ;;

  let update = custom_field_effects CustomFieldCommand.Update.effects
  let publish = custom_field_effects CustomFieldCommand.Publish.effects
  let delete = custom_field_effects CustomFieldCommand.Destroy.effects
end
