module HttpUtils = Http_utils
module Message = Pool_common.Message
module Url = Page.Admin.CustomFields.Url

let create_layout req = General.create_tenant_layout req

let get_group_id req =
  HttpUtils.get_field_router_param req Message.Field.CustomFieldGroup
  |> Custom_field.Group.Id.of_string
;;

let get_model = Admin_custom_fields.get_model

let form ?id req model =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, Url.index_path model)
    @@ let* custom_field_group =
         id
         |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
              let* group = Custom_field.find_group database_label id in
              let%lwt fields = Custom_field.find_by_group database_label id in
              (group, fields) |> CCOption.pure |> Lwt_result.return)
       in
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let%lwt sys_languages = Settings.find_languages database_label in
       Page.Admin.CustomFieldGroups.detail
         ?custom_field_group
         model
         context
         sys_languages
         flash_fetcher
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req = get_model form req

let edit req =
  let id = get_group_id req in
  get_model (form ~id) req
;;

let write ?id req model =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let redirect_path = Url.index_path model in
  let error_path =
    id
    |> function
    | None -> Url.Group.new_path model
    | Some id -> Url.Group.edit_path (model, id)
  in
  let field_names =
    let open Pool_common in
    let encode_lang t = t |> Language.create |> CCResult.to_opt in
    let go = Admin_custom_fields.find_assocs_in_urlencoded urlencoded in
    go Message.Field.Name encode_lang
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, error_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Logger.req req in
    let events =
      let%lwt sys_languages = Settings.find_languages database_label in
      match id with
      | None ->
        Cqrs_command.Custom_field_group_command.(
          Create.handle ~tags sys_languages field_names model |> Lwt_result.lift)
      | Some id ->
        let* custom_field_group =
          id |> Custom_field.find_group database_label
        in
        Cqrs_command.Custom_field_group_command.(
          Update.handle ~tags sys_languages custom_field_group field_names model
          |> Lwt_result.lift)
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      let success =
        let open Message in
        if CCOption.is_some id
        then Updated Field.CustomFieldGroup
        else Created Field.CustomFieldGroup
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let create req = get_model write req

let update req =
  let id = req |> get_group_id in
  get_model (write ~id) req
;;

let delete req =
  let handler req model =
    let id = req |> get_group_id in
    let result { Pool_context.database_label; _ } =
      let redirect_path = Url.Group.edit_path (model, id) in
      Utils.Lwt_result.map_error (fun err -> err, redirect_path)
      @@
      let open Utils.Lwt_result.Infix in
      let* events =
        let open CCFun.Infix in
        id
        |> Custom_field.find_group database_label
        >>= Cqrs_command.Custom_field_group_command.Destroy.handle
            %> Lwt_result.lift
      in
      let%lwt () = Pool_event.handle_events database_label events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set
            ~success:[ Message.(Deleted Field.CustomFieldGroup) ]
        ]
      |> Lwt_result.ok
    in
    result |> HttpUtils.extract_happy_path req
  in
  get_model handler req
;;

let sort req =
  let handler req model =
    let open Utils.Lwt_result.Infix in
    let redirect_path = Url.index_path model in
    let result { Pool_context.database_label; _ } =
      Utils.Lwt_result.map_error (fun err -> err, redirect_path, [])
      @@ let%lwt ids =
           Sihl.Web.Request.urlencoded_list
             Message.Field.(CustomFieldGroup |> array_key)
             req
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
         let tags = Logger.req req in
         let events =
           groups
           |> Cqrs_command.Custom_field_group_command.Sort.handle ~tags
           |> Lwt_result.lift
         in
         let handle events =
           let%lwt () =
             Lwt_list.iter_s
               (Pool_event.handle_event ~tags database_label)
               events
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

let sort_fields req =
  let group = req |> get_group_id in
  Admin_custom_fields.sort_fields req ~group ()
;;

module Access : sig
  include Helpers.AccessSig

  val sort : Rock.Middleware.t
end = struct
  module CustomFieldCommand = Cqrs_command.Custom_field_group_command
  module Field = Pool_common.Message.Field

  let custom_field_effects =
    Middleware.Guardian.id_effects
      Custom_field.Group.Id.of_string
      Field.CustomFieldGroup
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity
      [ `Read, `TargetEntity `CustomField ]
  ;;

  let create =
    CustomFieldCommand.Create.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Custom_field.Group.Id.value)
        ; `Read, `TargetEntity `CustomField
        ])
    ]
    |> custom_field_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    [ CustomFieldCommand.Update.effects ]
    |> custom_field_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let sort =
    CustomFieldCommand.Sort.effects |> Middleware.Guardian.validate_admin_entity
  ;;

  let delete =
    [ CustomFieldCommand.Destroy.effects ]
    |> custom_field_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
