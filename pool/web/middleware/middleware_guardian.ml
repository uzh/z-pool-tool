open CCFun.Infix

let src = Logs.Src.create "middleware.guardian"

let require_user_type_of (user_type : Pool_context.UserType.t list) =
  let filter handler req =
    let open Utils.Lwt_result.Infix in
    let validate_user_type { Pool_context.user; _ } =
      if Pool_context.UserType.user_in user_type user
      then Ok ()
      else
        Error
          (Pool_context.dashboard_path
             ~guest:(Http_utils.intended_of_request req "/login")
             user)
    in
    Pool_context.find req
    |> Lwt_result.lift
    >|- (fun _ -> "/not-found")
    >== validate_user_type
    >|> function
    | Ok _ -> handler req
    | Error path -> Http_utils.redirect_to path
  in
  Rock.Middleware.create ~name:"guardian.require_type" ~filter
;;

let debug_log ?tags auth effects =
  Logs.debug ~src (fun m ->
    m
      ?tags
      "Guard admin middleware:\n%s\nEFFECTS: %s"
      ([%show: Guard.Actor.t] auth)
      ([%show: Guard.ValidationSet.t] effects))
;;

let access_denied database_label =
  let open Pool_message.Error in
  fun err ->
    let (_ : t) =
      Pool_common.Utils.with_log_error
        ~src
        ~tags:(Pool_database.Logger.Tags.create database_label)
        err
    in
    AccessDenied
;;

let validate_access_request_fcn fcn ?any_id effects req =
  let open Utils.Lwt_result.Infix in
  let open Pool_message.Error in
  let* ({ Pool_context.user; database_label; _ } as context) =
    req |> Pool_context.find |> Lwt_result.lift
  in
  Lwt_result.map_error (access_denied database_label)
  @@
  match user with
  | Pool_context.Guest | Pool_context.Contact _ -> Lwt.return_error AccessDenied
  | Pool_context.Admin admin ->
    let ctx = Pool_database.to_ctx database_label in
    let* auth = Admin.Guard.Actor.to_authorizable ~ctx admin in
    fcn context auth ?any_id effects req
;;

let validate_access_request_dependent =
  let open Utils.Lwt_result.Infix in
  validate_access_request_fcn
    (fun
        ({ Pool_context.database_label; _ } as context)
        auth
        ?any_id
        effects
        req
      ->
       let* effects = effects req |> Lwt_result.lift in
       let tags = Pool_context.Logger.Tags.context context in
       let () = debug_log ~tags auth effects in
       Guard.Persistence.validate ?any_id database_label effects auth)
;;

let validate_access_request_dependent_lwt ?any_id effects req =
  let open Utils.Lwt_result.Infix in
  let* ({ Pool_context.user; database_label; _ } as context) =
    req |> Pool_context.find |> Lwt_result.lift
  in
  Lwt_result.map_error (access_denied database_label)
  @@
  match user with
  | Pool_context.Guest | Pool_context.Contact _ ->
    Lwt.return_error Pool_message.Error.AccessDenied
  | Pool_context.Admin admin ->
    let ctx = Pool_database.to_ctx database_label in
    let* auth = Admin.Guard.Actor.to_authorizable ~ctx admin in
    let* effects = effects req in
    let tags = Pool_context.Logger.Tags.context context in
    let () = debug_log ~tags auth effects in
    Guard.Persistence.validate ?any_id database_label effects auth
;;

let validate_admin_entity_base validate =
  let filter handler req =
    let open Pool_common in
    let open Pool_message in
    match%lwt validate req with
    | Ok _ -> handler req
    | Error err ->
      let (_ : Error.t) = Utils.with_log_error ~level:Logs.Info err in
      let open Http_utils.Htmx in
      (match is_hx_request req with
       | true ->
         error_notification Language.En Error.AccessDenied
         |> html_to_plain_text_response
         |> Lwt.return
       | false ->
         (match Http_utils.is_req_from_root_host req with
          | false -> "/denied"
          | true -> "/root/denied")
         |> Http_utils.redirect_to)
  in
  Rock.Middleware.create ~name:"guardian.generic" ~filter
;;

let validate_admin_entity ?any_id effects =
  validate_access_request_dependent ?any_id (fun (_ : Rock.Request.t) ->
    Ok effects)
  |> validate_admin_entity_base
;;

let validate_generic ?any_id generic_fcn =
  generic_fcn %> CCResult.return
  |> validate_access_request_dependent ?any_id
  |> validate_admin_entity_base
;;

let validate_generic_result ?any_id =
  validate_access_request_dependent ?any_id %> validate_admin_entity_base
;;

let validate_generic_lwt_result ?any_id =
  validate_access_request_dependent_lwt ?any_id %> validate_admin_entity_base
;;

let id_effects encode field effect_set =
  Http_utils.find_id encode field %> effect_set
;;

let denied =
  Rock.Middleware.create ~name:"guardian.denied" ~filter:(fun _ _ ->
    Http_utils.redirect_to "/denied")
;;
