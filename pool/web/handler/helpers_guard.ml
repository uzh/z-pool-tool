open Utils.Lwt_result.Infix
open Pool_message
module Command = Cqrs_command.Guardian_command

let ( %> ) = CCFun.( %> )
let src = Logs.Src.create "handler.helper.guard"

let find_roles database_label = function
  | None -> Lwt.return []
  | Some { Guard.Actor.uuid; _ } ->
    Guard.Persistence.ActorRole.find_by_actor database_label uuid
;;

let find_roles_of_ctx { Pool_context.database_label; user; _ } =
  user
  |> Pool_context.Utils.find_authorizable_opt database_label
  >|> CCOption.map_or ~default:(Lwt.return []) (fun { Guard.Actor.uuid; _ } ->
    Guard.Persistence.ActorRole.find_by_actor database_label uuid)
;;

let has_permission database_label user set =
  let open Utils.Lwt_result.Infix in
  Pool_context.Utils.find_authorizable database_label user
  >>= Guard.Persistence.validate database_label set
  ||> CCResult.is_ok
;;

let can_read_contact permisson_on_target { Pool_context.guardian; _ } =
  let open Guard in
  let open PermissionOnTarget in
  permisson_on_target
  |> CCList.fold_left
       (fun init set -> if init then init else validate set guardian)
       false
;;

let can_read_contact_name context verify_on_ids =
  can_read_contact Contact.Guard.Access.(read_name ~verify_on_ids ()) context
;;

let can_read_contact_info context verify_on_ids =
  can_read_contact Contact.Guard.Access.(read_info ~verify_on_ids ()) context
;;

let can_access_contact_profile context id =
  can_read_contact
    Contact.Guard.Access.(
      read_of_target (Guard.Uuid.target_of Experiment.Id.value id))
    context
;;

let target_model_for_actor_role
  pool
  ({ Guard.ActorRole.target_uuid; _ } as role)
  =
  let find_target_model =
    CCOption.map_or ~default:Lwt.return_none (fun uuid ->
      Guard.Persistence.Target.find_model ~ctx:(Database.to_ctx pool) uuid
      ||> CCResult.to_opt)
  in
  let%lwt target_model = find_target_model target_uuid in
  Lwt.return (role, target_model)
;;

let can_send_direct_message { Pool_context.database_label; user; _ } =
  has_permission database_label user Contact.Guard.Access.send_direct_message
;;

let can_rerun_session_filter
  { Pool_context.database_label; user; _ }
  experiment_id
  session_id
  =
  has_permission
    database_label
    user
    (Cqrs_command.Assignment_command.UpdateMatchesFilter.effects
       experiment_id
       session_id)
;;

let grant_role ~redirect_path ~actor ~target_id database_label req =
  let open Utils.Lwt_result.Infix in
  let lift = Lwt_result.lift in
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let* role =
    Http_utils.find_in_urlencoded Field.Role urlencoded
    |> lift
    >== Role.Role.of_string_res %> CCResult.map_err Error.authorization
  in
  let* role_target =
    Http_utils.htmx_urlencoded_list Field.(Target |> array_key) req
    ||> CCList.map
          (Guard.Uuid.Target.of_string
           %> CCOption.to_result (Error.Decode Field.Id))
    ||> CCResult.flatten_l
  in
  let* expand_targets =
    let open Guard.Uuid.Target in
    if role_target |> CCList.is_empty
    then Lwt.return_ok [ role, None ]
    else (
      match role with
      | `Assistant | `Experimenter ->
        let to_id = to_string %> Experiment.Id.of_string in
        role_target
        |> Lwt_list.filter_s (fun id ->
          id |> to_id |> Experiment.find database_label ||> CCResult.is_ok)
        ||> CCList.map (fun uuid -> role, Some uuid)
        |> Lwt_result.ok
      | `LocationManager ->
        let to_id = to_string %> Pool_location.Id.of_string in
        role_target
        |> Lwt_list.filter_s (fun id ->
          id |> to_id |> Pool_location.find database_label ||> CCResult.is_ok)
        ||> CCList.map (fun uuid -> role, Some uuid)
        |> Lwt_result.ok
      | role ->
        Logs.err (fun m ->
          m "Admin handler: Missing role %s" ([%show: Role.Role.t] role));
        Lwt.return_error (Error.NotFound Field.Role)
        ||> Pool_common.Utils.with_log_result_error ~src ~tags CCFun.id)
  in
  let roles =
    expand_targets
    |> Lwt_list.map_s
         (Guard.Persistence.Actor.validate_assign_role database_label actor)
    ||> CCList.all_ok
  in
  let events roles =
    let open Command in
    (* TODO: validate if role can be granted *)
    GrantRoles.handle ~tags { target_id; roles } |> lift
  in
  let handle events =
    Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
  in
  let* () = roles >>= events |>> handle in
  Lwt_result.ok
    (Http_utils.redirect_to_with_actions
       redirect_path
       [ Http_utils.Message.set ~success:[ Success.Created Field.Role ] ])
;;

let revoke_role ~redirect_path ~target_id database_label req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let role =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let role =
      let open CCResult in
      Http_utils.find_in_urlencoded Field.Role urlencoded
      >>= Role.Role.of_string_res %> CCResult.map_err Error.authorization
    in
    let uuid =
      Http_utils.find_in_urlencoded_opt Field.Target urlencoded
      |> CCFun.flip CCOption.bind Guard.Uuid.Target.of_string
    in
    role |> Lwt_result.lift >|+ fun role -> role, uuid
  in
  let events role =
    let open Command in
    RevokeRole.handle ~tags { target_id; role } |> Lwt_result.lift
  in
  let handle events =
    let%lwt () =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
    in
    Http_utils.redirect_to_with_actions
      ~skip_externalize:true
      redirect_path
      [ Http_utils.Message.set ~success:[ Success.RoleUnassigned ] ]
  in
  role >>= events |>> handle
;;

let handle_toggle_role target_id req =
  let result (_ : Pool_context.t) =
    Sihl.Web.Request.to_urlencoded req
    ||> Http_utils.find_in_urlencoded Field.Role
    >== Role.Role.of_string_res %> CCResult.map_err Error.authorization
    >|+ fun key ->
    Component.Role.Search.value_form
      Pool_common.Language.En
      ~key
      ~path:"/admin/admins"
      target_id
      ()
    |> Http_utils.Htmx.html_to_plain_text_response
  in
  result |> Http_utils.Htmx.handle_error_message ~src req
;;

let search_role_entities target req =
  let result { Pool_context.database_label; language; user; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let target_id = target.Guard.Target.uuid in
    let* actor =
      Pool_context.Utils.find_authorizable ~admin_only:true database_label user
    in
    let query = Http_utils.find_in_urlencoded_opt Field.Search urlencoded in
    let* search_role =
      let open CCOption in
      Http_utils.find_in_urlencoded_opt Field.Role urlencoded
      |> CCFun.flip bind (fun role ->
        try Role.Role.of_string role |> return with
        | _ -> None)
      |> to_result (Error.NotFound Field.Role)
      |> Lwt_result.lift
    in
    let entities_to_exclude encode_id =
      Http_utils.htmx_urlencoded_list Field.(array_key Target) req
      ||> CCList.map encode_id
    in
    let execute_search search_fnc to_html =
      (match query with
       | None -> Lwt.return []
       | Some query -> search_fnc query actor)
      ||> to_html language
      ||> Http_utils.Htmx.multi_html_to_plain_text_response %> CCResult.return
    in
    let open Guard.Persistence in
    match search_role with
    | `Assistant | `Experimenter ->
      let open Experiment.Guard.Access in
      let%lwt exclude = entities_to_exclude Experiment.Id.of_string in
      let search_experiment value actor =
        Experiment.find_targets_grantable_by_target
          ~exclude
          database_label
          target_id
          search_role
          value
        >|> Lwt_list.filter_s (fun (id, _) ->
          validate database_label (read id) actor ||> CCResult.is_ok)
      in
      execute_search search_experiment Component.Search.Experiment.query_results
    | `LocationManager ->
      let open Pool_location.Guard.Access in
      let open Pool_location in
      let%lwt exclude = entities_to_exclude Id.of_string in
      let search_location value actor =
        find_targets_grantable_by_target ~exclude database_label target_id value
        >|> Lwt_list.filter_s (fun (id, _) ->
          validate database_label (read id) actor ||> CCResult.is_ok)
      in
      execute_search search_location Component.Search.Location.query_results
    | _ -> Lwt_result.fail (Error.Invalid Field.Role)
  in
  result |> Http_utils.Htmx.handle_error_message ~src req
;;
