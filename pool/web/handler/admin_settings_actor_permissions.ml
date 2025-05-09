open CCFun.Infix
open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.settings_actor_permissions"
let active_navigation = "/admin/settings/actor-permission"

let show req =
  Response.Htmx.index_handler
    ~active_navigation
    ~query:(module Guard.ActorPermission)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun ({ Pool_context.database_label; language; _ } as context) query ->
  let%lwt permissions, query =
    Guard.Persistence.ActorPermission.find_by query database_label
  in
  let%lwt hint = I18n.(find_by_key database_label Key.ActorPermissionHint) language in
  let open Page.Admin.Settings.ActorPermission in
  (if HttpUtils.Htmx.is_hx_request req then list else index ~hint)
    context
    permissions
    query
  |> Lwt_result.return
;;

let delete req =
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    Response.bad_request_on_error show
    @@
    let permission =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.find_in_urlencoded Field.Permission
      >== fun permission ->
      let read = Yojson.Safe.from_string %> Guard.ActorPermission.of_yojson in
      CCResult.map_err
        Error.authorization
        (try read permission with
         | _ -> Error "Undefined Yojson for actor permission.")
    in
    let events = Cqrs_command.Guardian_command.DeleteActorPermission.handle ~tags in
    let handle = function
      | Ok events ->
        let%lwt () = Pool_event.handle_events ~tags database_label user events in
        Http_utils.redirect_to_with_actions
          active_navigation
          [ Message.set ~success:[ Success.Deleted Field.Permission ] ]
      | Error _ ->
        Http_utils.redirect_to_with_actions
          active_navigation
          [ Message.set ~error:[ Error.NotFound Field.Permission ] ]
    in
    permission >== events >|> handle |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let new_form req =
  let result
        ({ Pool_context.csrf; database_label; language; flash_fetcher; _ } as context)
    =
    Response.bad_request_render_error context
    @@
    let%lwt hint =
      I18n.(find_by_key database_label Key.ActorPermissionCreateHint) language
    in
    Page.Admin.Settings.ActorPermission.create
      ~hint
      context
      (Component.Role.ActorPermissionSearch.input_form ?flash_fetcher csrf language ())
    |> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let handle_toggle_target req =
  let open CCFun in
  let result (_ : Pool_context.t) =
    let open Pool_common in
    let open Guard.Permission in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let find = flip HttpUtils.find_in_urlencoded urlencoded in
    CCResult.both (find Field.Permission) (find Field.Model)
    |> Lwt_result.lift
    >== (fun (permission, model) ->
    let permission = Guard.Permission.of_string_res permission in
    let model = Role.Target.of_string_res model |> CCResult.map_err Error.authorization in
    CCResult.both permission model)
    >|+ fun (permission, model) ->
    Component.Role.ActorPermissionSearch.value_form
      ~empty:(equal Create permission)
      Language.En
      model
    |> Response.Htmx.of_html
  in
  Response.Htmx.handle ~src req result
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let lift = Lwt_result.lift in
  let result { Pool_context.database_label; user; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    Response.bad_request_on_error ~urlencoded new_form
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let find = CCFun.flip HttpUtils.find_in_urlencoded urlencoded in
    let* actors =
      HttpUtils.htmx_urlencoded_list Field.(ValueOf Admin |> array_key) req
      ||> CCList.map
            (Guard.Uuid.Actor.of_string %> CCOption.to_result (Error.Decode Field.Id))
      ||> CCResult.flatten_l
    in
    let%lwt actors =
      let to_id = Guard.Uuid.Actor.to_string %> Admin.Id.of_string in
      actors
      |> Lwt_list.filter_s (fun id ->
        id |> to_id |> Admin.find database_label ||> CCResult.is_ok)
    in
    let* permission = find Field.Permission |> lift >== Guard.Permission.of_string_res in
    let* model =
      find Field.Model
      |> lift
      >== Role.Target.of_string_res %> CCResult.map_err Error.authorization
    in
    let* targets =
      HttpUtils.htmx_urlencoded_list Field.(Value |> array_key) req
      ||> CCList.map
            (Guard.Uuid.Target.of_string %> CCOption.to_result (Error.Decode Field.Id))
      ||> CCResult.flatten_l
    in
    let expand_targets =
      let open Guard.Uuid.Target in
      match targets, model with
      | [], model -> Lwt.return_ok [ model, None ]
      | targets, (`Experiment as model) ->
        let to_id = to_string %> Experiment.Id.of_string in
        targets
        |> Lwt_list.filter_s (fun id ->
          id |> to_id |> Experiment.find database_label ||> CCResult.is_ok)
        ||> CCList.map (fun uuid -> model, Some uuid)
        |> Lwt_result.ok
      | _, model ->
        Logs.err (fun m ->
          m "Admin handler: Missing role %s" ([%show: Role.Target.t] model));
        Lwt.return_error (Error.NotFound Field.Role)
        ||> Pool_common.Utils.with_log_result_error ~src ~tags CCFun.id
    in
    let events =
      let open Cqrs_command.Guardian_command in
      let create permission (model, uuid) actor =
        let open Guard.ActorPermission in
        uuid
        |> CCOption.map_or
             ~default:(create_for_model actor permission model)
             (create_for_id actor permission)
      in
      let open CCList in
      flat_map (fun target ->
        actors |> map (create permission target %> CreateActorPermission.handle ~tags))
      %> CCResult.flatten_l
      %> CCResult.map flatten
    in
    let handle events = Pool_event.handle_events ~tags database_label user events in
    let* () = expand_targets >== events |>> handle in
    Lwt_result.ok
      (Http_utils.redirect_to_with_actions
         active_navigation
         [ Message.set ~success:[ Success.Created Field.Role ] ])
  in
  Response.handle ~src req result
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access

  let index = Guard.Access.Permission.read |> Middleware.Guardian.validate_admin_entity

  let create =
    Cqrs_command.Guardian_command.CreateActorPermission.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let delete =
    Cqrs_command.Guardian_command.DeleteActorPermission.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
