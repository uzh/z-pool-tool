open Pool_message
open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Url = Page.Admin.CustomFields.Url
module Response = Http_response

let src = Logs.Src.create "handler.admin.custom_field_groups"
let create_layout req = General.create_tenant_layout req

let get_group_id req =
  HttpUtils.get_field_router_param req Field.CustomFieldGroup
  |> Custom_field.Group.Id.of_string
;;

let get_model = Admin_custom_fields.get_model

let form ?id req model =
  let result ({ Pool_context.database_label; _ } as context) =
    let* custom_field_group =
      id
      |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
        let* group =
          Custom_field.find_group database_label id |> Response.not_found_on_error
        in
        let%lwt fields = Custom_field.find_by_group database_label id in
        (group, fields) |> CCOption.pure |> Lwt_result.return)
    in
    Response.bad_request_render_error context
    @@
    let%lwt sys_languages = Settings.find_languages database_label in
    Page.Admin.CustomFieldGroups.detail ?custom_field_group model context sys_languages
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let new_form req = get_model form req

let edit req =
  let id = get_group_id req in
  get_model (form ~id) req
;;

let write ?id req model =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let field_names =
    let open Pool_common in
    let encode_lang t = t |> Language.create |> CCResult.to_opt in
    let go = Admin_custom_fields.find_assocs_in_urlencoded urlencoded in
    go Field.Name encode_lang
  in
  let result { Pool_context.database_label; user; _ } =
    let%lwt sys_languages = Settings.find_languages database_label in
    let events =
      match id with
      | None ->
        Response.bad_request_on_error
          ~urlencoded
          new_form
          Cqrs_command.Custom_field_group_command.(
            Create.handle ~tags sys_languages field_names model |> Lwt_result.lift)
      | Some id ->
        let* custom_field_group =
          id |> Custom_field.find_group database_label |> Response.not_found_on_error
        in
        Response.bad_request_on_error ~urlencoded edit
        @@ Cqrs_command.Custom_field_group_command.(
             Update.handle ~tags sys_languages custom_field_group field_names model
             |> Lwt_result.lift)
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      let success =
        let open Success in
        if CCOption.is_some id
        then Updated Field.CustomFieldGroup
        else Created Field.CustomFieldGroup
      in
      Http_utils.redirect_to_with_actions
        (Url.index_path model)
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let create req = get_model write req

let update req =
  let id = req |> get_group_id in
  get_model (write ~id) req
;;

let delete req =
  let handler req model =
    let tags = Pool_context.Logger.Tags.req req in
    let id = get_group_id req in
    let result { Pool_context.database_label; user; _ } =
      let* group =
        Custom_field.find_group database_label id |> Response.not_found_on_error
      in
      Response.bad_request_on_error edit
      @@
      let open Utils.Lwt_result.Infix in
      let* events =
        Cqrs_command.Custom_field_group_command.Destroy.handle ~tags group
        |> Lwt_result.lift
      in
      let%lwt () = Pool_event.handle_events database_label user events in
      Http_utils.redirect_to_with_actions
        (Url.Group.edit_path (model, id))
        [ HttpUtils.Message.set ~success:[ Success.Deleted Field.CustomFieldGroup ] ]
      |> Lwt_result.ok
    in
    Response.handle ~src req result
  in
  get_model handler req
;;

let sort req =
  let handler req model =
    let open Utils.Lwt_result.Infix in
    let result { Pool_context.database_label; user; _ } =
      Response.bad_request_on_error Admin_custom_fields.index
      @@
      let tags = Pool_context.Logger.Tags.req req in
      let%lwt ids =
        Sihl.Web.Request.urlencoded_list Field.(CustomFieldGroup |> array_key) req
      in
      let%lwt groups =
        let open Utils.Lwt_result.Infix in
        Custom_field.find_groups_by_model database_label model
        ||> fun options ->
        CCList.filter_map
          (fun id ->
             CCList.find_opt
               Custom_field.Group.(
                 fun (option : t) -> Id.equal (Id.of_string id) option.id)
               options)
          ids
      in
      let events =
        let open Cqrs_command.Custom_field_group_command.Sort in
        groups |> handle ~tags |> Lwt_result.lift
      in
      let handle events =
        let%lwt () = Pool_event.handle_events ~tags database_label user events in
        Http_utils.redirect_to_with_actions
          (Url.index_path model)
          [ HttpUtils.Message.set ~success:[ Success.Updated Field.CustomFieldGroup ] ]
      in
      events |>> handle
    in
    Response.handle ~src req result
  in
  get_model handler req
;;

let sort_fields req =
  let group = req |> get_group_id in
  Admin_custom_fields.sort_fields req ~group ()
;;

let changelog req =
  let result (_ : Pool_context.t) =
    let open Utils.Lwt_result.Infix in
    let* model = Admin_custom_fields.model_from_router req |> Lwt_result.lift in
    let group_id = get_group_id req in
    let url =
      HttpUtils.Url.Admin.custom_field_groups_path
        model
        ~suffix:"changelog"
        ~id:group_id
        ()
    in
    Lwt_result.ok
    @@ Helpers.Changelog.htmx_handler ~url (Custom_field.Group.Id.to_common group_id) req
  in
  Response.Htmx.handle ~src req result
;;

module Access : sig
  include module type of Helpers.Access

  val sort : Rock.Middleware.t
  val sort_fields : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Command = Cqrs_command.Custom_field_group_command
  module Guardian = Middleware.Guardian

  let custom_field_group_effects =
    Guardian.id_effects Custom_field.Group.Id.validate Field.CustomFieldGroup
  ;;

  let create = Command.Create.effects |> Guardian.validate_admin_entity
  let update = custom_field_group_effects Command.Update.effects
  let sort = Command.Sort.effects |> Guardian.validate_admin_entity

  let sort_fields =
    custom_field_group_effects (fun goup_id ->
      Guard.ValidationSet.And
        [ Cqrs_command.Custom_field_command.Sort.effects
        ; Custom_field.Guard.Access.Group.update goup_id
        ])
  ;;

  let delete = custom_field_group_effects Command.Destroy.effects
end
