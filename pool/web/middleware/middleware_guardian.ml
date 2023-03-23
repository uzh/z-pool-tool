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

let validate_access_request_dependent effects req =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let* ({ Pool_context.user; database_label; _ } as context) =
    req |> Pool_context.find |> Lwt_result.lift
  in
  let ctx = Pool_database.to_ctx database_label in
  let* effects = effects req |> Lwt_result.lift in
  Lwt_result.map_error (fun err ->
    let (_ : error) =
      Pool_common.Utils.with_log_error
        ~tags:(Pool_database.Logger.Tags.create database_label)
        err
    in
    AccessDenied)
  @@
  match user with
  | Pool_context.Guest | Pool_context.Contact _ -> Lwt.return_error AccessDenied
  | Pool_context.Admin admin ->
    let* auth = Admin.Guard.Actor.to_authorizable ~ctx admin in
    let () =
      Logs.debug ~src (fun m ->
        m
          ~tags:(Pool_context.Logger.Tags.context context)
          "Guard admin middleware:\n%s\n%s\nEFFECTS: %s"
          ([%show: Admin.t] admin)
          ([%show: Guard.Actor.t] auth)
          ([%show: Guard.ValidationSet.t] effects))
    in
    Guard.Persistence.validate ~ctx authorization effects auth
;;

let validate_admin_entity_base validate =
  let filter handler req =
    match%lwt validate req with
    | Ok _ -> handler req
    | Error _ ->
      (match Http_utils.is_req_from_root_host req with
       | false -> "/denied"
       | true -> "/root/denied")
      |> Http_utils.redirect_to
  in
  Rock.Middleware.create ~name:"guardian.generic" ~filter
;;

let validate_admin_entity effects =
  validate_access_request_dependent (fun (_ : Rock.Request.t) -> Ok effects)
  |> validate_admin_entity_base
;;

let validate_generic generic_fcn =
  generic_fcn %> CCResult.return
  |> validate_access_request_dependent
  |> validate_admin_entity_base
;;

let validate_generic_result =
  validate_access_request_dependent %> validate_admin_entity_base
;;

let id_effects encode field effect_set =
  Http_utils.find_id encode field %> effect_set
;;

let denied =
  Rock.Middleware.create ~name:"guardian.denied" ~filter:(fun _ _ ->
    Http_utils.redirect_to "/denied")
;;
