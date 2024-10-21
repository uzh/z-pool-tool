open Utils.Lwt_result.Infix
open Pool_message

let src = Logs.Src.create "handler.root.version"
let active_navigation = Http_utils.Url.Root.version_path ()
let create_layout = General.create_root_layout
let version_path = Http_utils.Url.Root.version_path

let version_id req =
  Http_utils.get_field_router_param req Field.Version
  |> Pool_version.Id.of_string
;;

let index req =
  let create_layout (_ : Rock.Request.t) ?active_navigation context children =
    General.create_root_layout ?active_navigation context children
    |> Lwt_result.ok
  in
  Http_utils.Htmx.handler
    ~active_navigation
    ~error_path:"root"
    ~query:(module Pool_version)
    ~create_layout
    req
  @@ fun context query ->
  let%lwt versions = Pool_version.all ~query () in
  let open Page.Root.Version in
  (if Http_utils.Htmx.is_hx_request req then list else index) context versions
  |> Lwt_result.return
;;

let form case req =
  let result context =
    Lwt_result.map_error (fun err -> err, active_navigation)
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* version =
      match case with
      | `New -> Lwt_result.return None
      | `Edit -> version_id req |> Pool_version.find >|+ CCOption.return
    in
    Page.Root.Version.form context ~flash_fetcher ?version ()
    |> create_layout context
    ||> Sihl.Web.Response.of_html
    ||> CCResult.return
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
      , version_path ~suffix:"new" ()
      , [ Http_utils.urlencoded_to_flash urlencoded ] ))
    @@
    let events =
      let open CCResult in
      let open Cqrs_command.Pool_version_command.Create in
      urlencoded |> decode >>= handle ~tags |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        (version_path ())
        [ Http_utils.Message.set ~success:[ Success.Created Field.Version ] ]
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
  let id = version_id req in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , version_path ~id ~suffix:"edit" ()
      , [ Http_utils.urlencoded_to_flash urlencoded ] ))
    @@
    let* version = Pool_version.find id in
    let events =
      let open CCResult in
      let open Cqrs_command.Pool_version_command.Update in
      urlencoded
      |> decode
      >>= handle ~tags:Logs.Tag.empty version
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        (version_path ())
        [ Http_utils.Message.set ~success:[ Success.Updated Field.Version ] ]
    in
    events |>> handle
  in
  result |> Http_utils.extract_happy_path_with_actions ~src req
;;

let publish req =
  let tags = Pool_context.Logger.Tags.req req in
  let id = version_id req in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, version_path ~id ~suffix:"edit" ())
    @@
    let* version = Pool_version.find id in
    let%lwt tenant_ids =
      Pool_tenant.find_all () ||> CCList.map (fun { Pool_tenant.id; _ } -> id)
    in
    let events =
      let open Cqrs_command.Pool_version_command.Publish in
      handle ~tags:Logs.Tag.empty tenant_ids version |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        (version_path ())
        [ Http_utils.Message.set ~success:[ Success.Published Field.Version ] ]
    in
    events |>> handle
  in
  result |> Http_utils.extract_happy_path ~src req
;;

module Access : sig
  include module type of Helpers.Access
end = struct
  module Command = Cqrs_command.Pool_version_command
  module Guardian = Middleware.Guardian

  let announcement_effects =
    Guardian.id_effects Pool_version.Id.validate Field.Version
  ;;

  let index =
    Pool_version.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create = Command.Create.effects |> Guardian.validate_admin_entity
  let read = announcement_effects Pool_version.Access.read
  let update = announcement_effects Command.Update.effects
  let delete = Guardian.denied
end
