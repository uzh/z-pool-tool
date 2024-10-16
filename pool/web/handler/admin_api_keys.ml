open Utils.Lwt_result.Infix
open Pool_message

let src = Logs.Src.create "handler.admin.api_keys"
let api_key_path = Http_utils.Url.Admin.api_key_path
let create_layout = General.create_tenant_layout

let api_key_id req =
  Http_utils.get_field_router_param req Field.ApiKey |> Api_key.Id.of_string
;;

let extract_happy_path = Http_utils.extract_happy_path ~src

let find_authorizable_target database_label api_key =
  let open Utils.Lwt_result.Infix in
  api_key.Api_key.id
  |> Guard.Uuid.target_of Api_key.Id.value
  |> Guard.Persistence.Target.find ~ctx:(Database.to_ctx database_label)
  >|- CCFun.const (Pool_message.Error.NotFound Field.Target)
;;

let find_granted_roles database_label api_key =
  api_key.Api_key.id
  |> Guard.Uuid.actor_of Api_key.Id.value
  |> Guard.Persistence.Actor.find database_label
  ||> CCOption.of_result
  >|> Helpers.Guard.find_roles database_label
;;

let index req =
  Http_utils.Htmx.handler
    ~active_navigation:(api_key_path ())
    ~error_path:"root"
    ~query:(module Api_key)
    ~create_layout
    req
  @@ fun (Pool_context.{ database_label; _ } as context) query ->
  let%lwt api_keys = Api_key.all ~query database_label in
  let open Page.Admin.ApiKey in
  (if Http_utils.Htmx.is_hx_request req then list else index) context api_keys
  |> Lwt_result.return
;;

let show req =
  let open Api_key in
  let result ({ Pool_context.database_label; _ } as context) =
    Lwt_result.map_error (fun err -> err, api_key_path ())
    @@
    let* api_key = api_key_id req |> find database_label in
    let target_id = api_key.id |> Guard.Uuid.target_of Id.value in
    let%lwt granted_roles = find_granted_roles database_label api_key in
    Page.Admin.ApiKey.show context api_key target_id granted_roles
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let new_form req =
  let result context =
    Lwt_result.map_error (fun err -> err, api_key_path ())
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Page.Admin.ApiKey.create context ~flash_fetcher ()
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let edit req =
  let open Api_key in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Lwt_result.map_error (fun err -> err, api_key_path ())
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* api_key = api_key_id req |> find database_label in
    let%lwt actor =
      Pool_context.Utils.find_authorizable_opt
        ~admin_only:true
        database_label
        user
    in
    let target_id = api_key.id |> Guard.Uuid.target_of Id.value in
    let%lwt available_roles =
      CCOption.map_or
        ~default:(Lwt.return [])
        (Guard.Persistence.Actor.can_assign_roles database_label)
        actor
      ||> CCList.map fst
    in
    let%lwt granted_roles = find_granted_roles database_label api_key in
    Page.Admin.ApiKey.edit
      context
      ~flash_fetcher
      api_key
      target_id
      available_roles
      granted_roles
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let create req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> Http_utils.remove_empty_values
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , api_key_path ~suffix:"new" ()
      , [ Http_utils.urlencoded_to_flash urlencoded ] ))
    @@
    let id = Api_key.Id.create () in
    let events =
      let open CCResult in
      let open Cqrs_command.Api_key_command.Create in
      decode urlencoded >>= handle ~id ~tags:Logs.Tag.empty |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        (api_key_path ~id ())
        [ Http_utils.Message.set ~success:[ Success.Created Field.ApiKey ] ]
    in
    events |>> handle
  in
  result |> Http_utils.extract_happy_path_with_actions ~src req
;;

let update req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> Http_utils.remove_empty_values
  in
  let id = api_key_id req in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , api_key_path ~id ~suffix:"edit" ()
      , [ Http_utils.urlencoded_to_flash urlencoded ] ))
    @@
    let* api_key = Api_key.find database_label id in
    let events =
      let open CCResult in
      let open Cqrs_command.Api_key_command.Update in
      decode urlencoded
      >>= handle ~tags:Logs.Tag.empty api_key
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        (api_key_path ~id ())
        [ Http_utils.Message.set ~success:[ Success.Updated Field.ApiKey ] ]
    in
    events |>> handle
  in
  result |> Http_utils.extract_happy_path_with_actions ~src req
;;

let handle_toggle_role req =
  let open Api_key in
  let result { Pool_context.database_label; _ } =
    let* api_key = api_key_id req |> find database_label in
    let target_id = Guard.Uuid.target_of Id.value api_key.id in
    Helpers.Guard.handle_toggle_role target_id req |> Lwt_result.ok
  in
  result |> Http_utils.Htmx.handle_error_message ~src req
;;

let search_role_entities req =
  let result { Pool_context.database_label; _ } =
    let* api_key = api_key_id req |> Api_key.find database_label in
    let* target = find_authorizable_target database_label api_key in
    Helpers.Guard.search_role_entities target req |> Lwt_result.ok
  in
  result |> Http_utils.Htmx.handle_error_message ~src req
;;

let grant_role req =
  let open Api_key in
  let open Utils.Lwt_result.Infix in
  let key_id = api_key_id req in
  let redirect_path =
    Http_utils.Url.Admin.api_key_path ~suffix:"edit" ~id:key_id ()
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let* actor =
      Pool_context.Utils.find_authorizable ~admin_only:true database_label user
    in
    let* api_key = find database_label key_id in
    let target_id = Guard.Uuid.actor_of Id.value api_key.id in
    Helpers.Guard.grant_role ~redirect_path ~actor ~target_id database_label req
  in
  result |> extract_happy_path req
;;

let revoke_role ({ Rock.Request.target; _ } as req) =
  let open Api_key in
  let open Utils.Lwt_result.Infix in
  let redirect_path =
    CCString.replace ~which:`Right ~sub:"/revoke-role" ~by:"/edit" target
  in
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let* api_key = api_key_id req |> find database_label in
    let target_id = Guard.Uuid.actor_of Id.value api_key.id in
    Helpers.Guard.revoke_role ~redirect_path ~target_id database_label req
  in
  result |> extract_happy_path req
;;

module Access : sig
  include module type of Helpers.Access

  val grant_role : Rock.Middleware.t
  val revoke_role : Rock.Middleware.t
end = struct
  module Command = Cqrs_command.Api_key_command
  module Guardian = Middleware.Guardian
  module GuardianCommand = Cqrs_command.Guardian_command

  let announcement_effects =
    Guardian.id_effects Api_key.Id.validate Field.ApiKey
  ;;

  let index =
    Api_key.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create = Command.Create.effects |> Guardian.validate_admin_entity
  let read = announcement_effects Api_key.Access.read
  let update = announcement_effects Command.Update.effects
  let delete = Guardian.denied

  let grant_role =
    GuardianCommand.GrantRoles.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let revoke_role =
    GuardianCommand.RevokeRole.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
