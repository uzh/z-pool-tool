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

let validate_access { Pool_context.user; database_label; _ } effects =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let ctx = Pool_tenant.to_ctx database_label in
  Lwt_result.map_error (fun err ->
    let (_ : error) = Pool_common.Utils.with_log_error err in
    AccessDenied)
  @@
  match user with
  | Pool_context.Guest | Pool_context.Contact _ -> Lwt.return_error AccessDenied
  | Pool_context.Admin admin ->
    let* auth = Admin.Guard.Actor.to_authorizable ~ctx admin in
    let () =
      Logs.debug (fun m ->
        m
          "Guard admin middleware:\n%s\n%s\nEFFECTS: %s"
          ([%show: Admin.t] admin)
          ([%show: [> `Admin ] Guard.Authorizable.t] auth)
          ([%show: Guard.Authorizer.effect list] effects))
    in
    Guard.Persistence.checker_of_effects ~ctx effects auth
    |> Lwt_result.map_error authorization
;;

let validate_admin_entity effects =
  let open Utils.Lwt_result.Infix in
  let filter handler req =
    let result =
      req
      |> Pool_context.find
      |> Lwt_result.lift
      >>= CCFun.flip validate_access effects
    in
    match%lwt result with
    | Ok _ -> handler req
    | Error _ -> Http_utils.redirect_to "/denied"
  in
  Rock.Middleware.create ~name:"guardian.role_entity" ~filter
;;

let id_effects encode field effects req context =
  let id = Http_utils.find_id encode field req in
  let effects =
    effects |> CCList.map (fun effect -> effect id) |> CCList.flatten
  in
  context, effects
;;

let validate_generic generic_fcn =
  let filter handler req =
    let result =
      let open Utils.Lwt_result.Infix in
      req
      |> Pool_context.find
      |> Lwt_result.lift
      >|+ generic_fcn req
      >>= CCFun.uncurry validate_access
    in
    match%lwt result with
    | Ok _ -> handler req
    | Error _ ->
      let redirect =
        match Http_utils.is_req_from_root_host req with
        | false -> "/denied"
        | true -> "/root/denied"
      in
      Http_utils.redirect_to redirect
  in
  Rock.Middleware.create ~name:"guardian.generic" ~filter
;;

let denied =
  Rock.Middleware.create ~name:"guardian.denied" ~filter:(fun _ _ ->
    Http_utils.redirect_to "/denied")
;;
