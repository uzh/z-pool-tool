open Pool_message
module HttpUtils = Http_utils
module Message = Pool_message
module Response = Http_response
module Url = Page.Admin.CustomFields.Url

let src = Logs.Src.create "handler.admin.custom_fields"
let create_layout req = General.create_tenant_layout req
let boolean_fields = Custom_field.boolean_fields |> CCList.map Field.show

let model_from_router req =
  let open Custom_field in
  let open CCResult in
  HttpUtils.find_field_router_param_opt req Field.Model
  |> CCOption.to_result Message.(Error.NotFound Field.Model)
  >>= Model.create
;;

let get_model fnc req =
  let model = model_from_router req in
  match model with
  | Ok model -> model |> fnc req
  | Error err ->
    Http_utils.redirect_to_with_actions
      Url.fallback_path
      [ HttpUtils.Message.set ~error:[ err ] ]
;;

let find_assocs_in_urlencoded urlencoded field encoder =
  let field = Field.show field in
  CCList.filter_map
    (fun (key, values) ->
       let group, id = CCString.take_drop (CCString.length field) key in
       let value = CCList.head_opt values in
       let key =
         let open CCOption in
         id |> CCString.chop_prefix ~pre:"[" >>= CCString.chop_suffix ~suf:"]" >>= encoder
       in
       match key, value with
       | Some l, Some v when CCString.equal field group -> Some (l, v)
       | _ -> None)
    urlencoded
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let open Custom_field in
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@ let* model = model_from_router req |> Lwt_result.lift in
       let%lwt group_list = Custom_field.find_groups_by_model database_label model in
       let%lwt field_list = find_by_model database_label model in
       Page.Admin.CustomFields.index field_list group_list model context
       >|> create_layout ~active_navigation:Url.fallback_path req context
       >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

(* TODO: Required? *)
let redirect _ =
  let open Custom_field in
  Model.all
  |> CCList.head_opt
  |> CCOption.map_or ~default:"/admin/dashboard" Url.index_path
  |> Http_utils.redirect_to
;;

let form ?id req model =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    let* custom_field =
      id
      |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
        Custom_field.find database_label id >|- Response.not_found >|+ CCOption.pure)
    in
    Response.bad_request_render_error context
    @@
    let%lwt groups = Custom_field.find_groups_by_model database_label model in
    let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    Page.Admin.CustomFields.detail ?custom_field model context groups sys_languages
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let new_form req = get_model form req

let edit req =
  let id =
    HttpUtils.get_field_router_param req Field.CustomField |> Custom_field.Id.of_string
  in
  get_model (form ~id) req
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
    ( go Field.Name encode_lang
    , go Field.Hint encode_lang
    , go Field.Validation CCOption.pure )
  in
  let result { Pool_context.database_label; user; _ } =
    let* custom_field, error_handler, success =
      match id with
      | None -> Lwt_result.return (None, new_form, Success.Created Field.CustomField)
      | Some id ->
        let* field =
          Custom_field.find database_label id
          |> Response.not_found_on_error
          >|+ CCOption.return
        in
        Lwt_result.return (field, edit, Success.Updated Field.CustomField)
    in
    Response.bad_request_on_error ~urlencoded error_handler
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Utils.Lwt_result.Infix in
      let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
      let* decoded =
        urlencoded |> Cqrs_command.Custom_field_command.base_decode |> Lwt_result.lift
      in
      match custom_field with
      | None ->
        Cqrs_command.Custom_field_command.Create.handle
          ~tags
          sys_languages
          model
          field_names
          field_hints
          validations
          decoded
        |> Lwt_result.lift
      | Some custom_field ->
        Cqrs_command.Custom_field_command.Update.handle
          ~tags
          sys_languages
          custom_field
          field_names
          field_hints
          validations
          decoded
        |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
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
  let id =
    HttpUtils.get_field_router_param req Field.CustomField |> Custom_field.Id.of_string
  in
  get_model (write ~id) req
;;

let toggle_action action req =
  let open Utils.Lwt_result.Infix in
  let id =
    HttpUtils.get_field_router_param req Field.CustomField |> Custom_field.Id.of_string
  in
  let result { Pool_context.database_label; user; _ } =
    let* custom_field = Custom_field.find database_label id >|- Response.not_found in
    Response.bad_request_on_error edit
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* model = model_from_router req |> Lwt_result.lift in
    let events =
      let open Cqrs_command.Custom_field_command in
      Lwt_result.lift
      @@
      match action with
      | `Publish -> Publish.handle ~tags custom_field
      | `Delete -> Delete.handle ~tags custom_field
    in
    let success =
      match action with
      | `Publish -> Success.Published Field.CustomField
      | `Delete -> Success.Deleted Field.CustomField
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        (Url.index_path model)
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let publish = toggle_action `Publish
let delete = toggle_action `Delete

let sort_options req =
  let handler req model =
    let open Utils.Lwt_result.Infix in
    let custom_field_id =
      HttpUtils.get_field_router_param req Field.CustomField |> Custom_field.Id.of_string
    in
    let result { Pool_context.database_label; user; _ } =
      let* custom_field =
        custom_field_id |> Custom_field.find database_label >|- Response.not_found
      in
      Response.bad_request_on_error edit
      @@
      let tags = Pool_context.Logger.Tags.req req in
      let%lwt ids =
        Sihl.Web.Request.urlencoded_list Field.(CustomFieldOption |> array_key) req
      in
      let%lwt options =
        let open Utils.Lwt_result.Infix in
        let open Custom_field in
        find_options_by_field database_label (id custom_field)
        ||> fun options ->
        CCList.filter_map
          (fun id ->
             CCList.find_opt
               SelectOption.(fun (option : t) -> Id.equal (Id.of_string id) option.id)
               options)
          ids
      in
      let events =
        let open Cqrs_command.Custom_field_option_command.Sort in
        options |> handle ~tags |> Lwt_result.lift
      in
      let handle events =
        let%lwt () = Pool_event.handle_events ~tags database_label user events in
        Http_utils.redirect_to_with_actions
          (Url.Field.edit_path (model, custom_field_id))
          [ HttpUtils.Message.set ~success:[ Success.Updated Field.CustomField ] ]
      in
      events |>> handle
    in
    Response.handle ~src req result
  in
  get_model handler req
;;

let sort_fields req ?group () =
  let handler req current_model =
    let open Utils.Lwt_result.Infix in
    let redirect_path =
      match group with
      | None -> Url.index_path current_model
      | Some group -> Url.Group.edit_path (current_model, group)
    in
    let result { Pool_context.database_label; user; _ } =
      Response.bad_request_on_error index
      @@
      let open Custom_field in
      let tags = Pool_context.Logger.Tags.req req in
      let%lwt ids =
        Sihl.Web.Request.urlencoded_list Field.(CustomField |> array_key) req
      in
      let%lwt fields =
        match group with
        | None -> find_ungrouped_by_model database_label current_model
        | Some group -> find_by_group database_label group
      in
      let fields =
        CCList.filter_map
          (fun idx ->
             CCList.find_opt
               (fun (field : t) -> Id.equal (Id.of_string idx) (id field))
               fields)
          ids
      in
      let events =
        let open Cqrs_command.Custom_field_command.Sort in
        fields |> handle ~tags |> Lwt_result.lift
      in
      let handle events =
        let%lwt () = Pool_event.handle_events ~tags database_label user events in
        Http_utils.redirect_to_with_actions
          redirect_path
          [ HttpUtils.Message.set ~success:[ Success.Updated Field.CustomFieldGroup ] ]
      in
      events |>> handle
    in
    Response.handle ~src req result
  in
  get_model handler req
;;

let sort_ungrouped_fields req = sort_fields req ()

let changelog req =
  let response req model =
    let custom_field_id =
      HttpUtils.get_field_router_param req Field.CustomField |> Custom_field.Id.of_string
    in
    let url =
      HttpUtils.Url.Admin.custom_fields_path
        model
        ~suffix:"changelog"
        ~id:custom_field_id
        ()
    in
    let to_human { Pool_context.database_label; language; _ } =
      Custom_field.changelog_to_human database_label language
    in
    let open Custom_field in
    Helpers.Changelog.htmx_handler ~to_human ~url (custom_field_id |> Id.to_common) req
  in
  get_model response req
;;

module Access : sig
  include module type of Helpers.Access

  val publish : Rock.Middleware.t
  val sort : Rock.Middleware.t
end = struct
  include Helpers.Access
  module CustomFieldCommand = Cqrs_command.Custom_field_command
  module Guardian = Middleware.Guardian

  let custom_field_effects =
    Guardian.id_effects Custom_field.Id.validate Field.CustomField
  ;;

  let index =
    Custom_field.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create = CustomFieldCommand.Create.effects |> Guardian.validate_admin_entity
  let update = custom_field_effects CustomFieldCommand.Update.effects
  let delete = custom_field_effects CustomFieldCommand.Delete.effects
  let publish = custom_field_effects CustomFieldCommand.Publish.effects
  let sort = CustomFieldCommand.Sort.effects |> Guardian.validate_admin_entity
end
