module HttpUtils = Http_utils
module Message = Pool_common.Message
module Url = Page.Admin.CustomFields.Url

let create_layout req = General.create_tenant_layout req

let boolean_fields =
  Custom_field.boolean_fields |> CCList.map Message.Field.show
;;

let model_from_router req =
  let open Custom_field in
  let open CCResult in
  HttpUtils.find_field_router_param_opt req Message.Field.Model
  |> CCOption.to_result Message.(NotFound Field.Model)
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
  let open Custom_field in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let* model = model_from_router req |> Lwt_result.lift in
       let%lwt group_list =
         Custom_field.find_groups_by_model database_label model
       in
       let%lwt field_list = find_by_model database_label model in
       Page.Admin.CustomFields.index field_list group_list model context
       |> create_layout ~active_navigation:Url.fallback_path req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

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
    Utils.Lwt_result.map_error (fun err -> err, Url.index_path model)
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* custom_field =
      id
      |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
           Custom_field.find database_label id >|+ CCOption.pure)
    in
    let%lwt groups = Custom_field.find_groups_by_model database_label model in
    let* sys_languages =
      Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
    in
    Page.Admin.CustomFields.detail
      ?custom_field
      model
      context
      groups
      sys_languages
      flash_fetcher
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req = get_model form req

let edit req =
  let id =
    HttpUtils.get_field_router_param req Message.Field.CustomField
    |> Custom_field.Id.of_string
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
    ( go Message.Field.Name encode_lang
    , go Message.Field.Hint encode_lang
    , go Message.Field.Validation CCOption.pure )
  in
  let error_path, success =
    let open Pool_common.Message in
    match id with
    | None -> Url.Field.new_path model, Updated Field.CustomField
    | Some id -> Url.Field.edit_path (model, id), Created Field.CustomField
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, error_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Utils.Lwt_result.Infix in
      let* sys_languages =
        Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
      in
      let* decoded =
        urlencoded
        |> Cqrs_command.Custom_field_command.base_decode
        |> Lwt_result.lift
      in
      match id with
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
      | Some id ->
        let* custom_field = Custom_field.find database_label id in
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
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        (Url.index_path model)
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let create req = get_model write req

let update req =
  let id =
    HttpUtils.get_field_router_param req Message.Field.CustomField
    |> Custom_field.Id.of_string
  in
  get_model (write ~id) req
;;

let toggle_action action req =
  let open Utils.Lwt_result.Infix in
  let id =
    HttpUtils.get_field_router_param req Message.Field.CustomField
    |> Custom_field.Id.of_string
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, Url.fallback_path, [])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* custom_field = Custom_field.find database_label id in
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
      | `Publish -> Message.(Published Field.CustomField)
      | `Delete -> Message.(Deleted Field.CustomField)
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        (Url.index_path model)
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let publish = toggle_action `Publish
let delete = toggle_action `Delete

let sort_options req =
  let handler req model =
    let open Utils.Lwt_result.Infix in
    let custom_field_id =
      HttpUtils.get_field_router_param req Message.Field.CustomField
      |> Custom_field.Id.of_string
    in
    let redirect_path = Url.Field.edit_path (model, custom_field_id) in
    let result { Pool_context.database_label; _ } =
      Utils.Lwt_result.map_error (fun err -> err, redirect_path, [])
      @@
      let tags = Pool_context.Logger.Tags.req req in
      let* custom_field = custom_field_id |> Custom_field.find database_label in
      let%lwt ids =
        Sihl.Web.Request.urlencoded_list
          Message.Field.(CustomFieldOption |> array_key)
          req
      in
      let%lwt options =
        let open Utils.Lwt_result.Infix in
        let open Custom_field in
        find_options_by_field database_label (id custom_field)
        ||> fun options ->
        CCList.filter_map
          (fun id ->
            CCList.find_opt
              SelectOption.(
                fun (option : t) -> Id.equal (Id.of_string id) option.id)
              options)
          ids
      in
      let events =
        let open Cqrs_command.Custom_field_option_command.Sort in
        options |> handle ~tags |> Lwt_result.lift
      in
      let handle events =
        let%lwt () =
          Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
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
    let result { Pool_context.database_label; _ } =
      Utils.Lwt_result.map_error (fun err -> err, redirect_path, [])
      @@
      let open Custom_field in
      let tags = Pool_context.Logger.Tags.req req in
      let%lwt ids =
        Sihl.Web.Request.urlencoded_list
          Message.Field.(CustomField |> array_key)
          req
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
        let%lwt () =
          Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
        in
        Http_utils.redirect_to_with_actions
          redirect_path
          [ HttpUtils.Message.set
              ~success:[ Message.(Updated Field.CustomFieldGroup) ]
          ]
      in
      events |>> handle
    in
    result |> HttpUtils.extract_happy_path_with_actions req
  in
  get_model handler req
;;

let sort_ungrouped_fields req = sort_fields req ()

module Access : sig
  include module type of Helpers.Access

  val publish : Rock.Middleware.t
  val sort : Rock.Middleware.t
end = struct
  include Helpers.Access
  module CustomFieldCommand = Cqrs_command.Custom_field_command
  module Guardian = Middleware.Guardian
  module Field = Pool_common.Message.Field

  let custom_field_effects =
    Guardian.id_effects Custom_field.Id.of_string Field.CustomField
  ;;

  let index =
    Custom_field.Guard.Access.index
    |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create =
    CustomFieldCommand.Create.effects |> Guardian.validate_admin_entity
  ;;

  let update =
    CustomFieldCommand.Update.effects
    |> custom_field_effects
    |> Guardian.validate_generic
  ;;

  let delete =
    CustomFieldCommand.Delete.effects
    |> custom_field_effects
    |> Guardian.validate_generic
  ;;

  let publish =
    CustomFieldCommand.Publish.effects
    |> custom_field_effects
    |> Guardian.validate_generic
  ;;

  let sort =
    CustomFieldCommand.Sort.effects
    |> custom_field_effects
    |> Guardian.validate_generic
  ;;
end
