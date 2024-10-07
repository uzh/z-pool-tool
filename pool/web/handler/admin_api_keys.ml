open Utils.Lwt_result.Infix
open Pool_message

let src = Logs.Src.create "handler.admin.api_keys"
let api_key_path = Http_utils.Url.Admin.api_key_path
let create_layout = General.create_tenant_layout

let api_key_id req =
  Http_utils.get_field_router_param req Field.ApiKey |> Api_key.Id.of_string
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
  let result ({ Pool_context.database_label; _ } as context) =
    Lwt_result.map_error (fun err -> err, api_key_path ())
    @@
    let* api_key = api_key_id req |> Api_key.find database_label in
    Page.Admin.ApiKey.show context api_key
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path ~src req
;;

let form case req =
  let result ({ Pool_context.database_label; _ } as context) =
    Lwt_result.map_error (fun err -> err, api_key_path ())
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* api_key =
      match case with
      | `New -> Lwt_result.return None
      | `Edit ->
        api_key_id req |> Api_key.find database_label >|+ CCOption.return
    in
    Page.Admin.ApiKey.form context ~flash_fetcher ?api_key ()
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path ~src req
;;

let new_form = form `New
let edit = form `Edit

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

module Access : sig
  include module type of Helpers.Access
end = struct
  module Command = Cqrs_command.Api_key_command
  module Guardian = Middleware.Guardian

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
end
