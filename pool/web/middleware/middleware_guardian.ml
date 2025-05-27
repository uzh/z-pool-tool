open CCFun.Infix

let src = Logs.Src.create "middleware.guardian"
let access_denied_err = Pool_message.Error.AccessDenied

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
        ~tags:(Database.Logger.Tags.create database_label)
        err
    in
    AccessDenied
;;

let validate_web_access_request_dependent ?any_id effects req =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  let* ({ user; database_label; _ } as context) = req |> find |> Lwt_result.lift in
  Lwt_result.map_error (access_denied database_label)
  @@
  match user with
  | Guest | Contact _ -> Lwt.return_error access_denied_err
  | Admin admin ->
    let ctx = Database.to_ctx database_label in
    let* auth = Admin.Guard.Actor.to_authorizable ~ctx admin in
    let* effects = effects req in
    let tags = Logger.Tags.context context in
    let () = debug_log ~tags auth effects in
    Guard.Persistence.validate ?any_id database_label effects auth
;;

let validate_api_access_request_dependent ?any_id effects req =
  let open Utils.Lwt_result.Infix in
  let open Pool_context.Api in
  let* ({ api_key; database_label; _ } as context) = req |> find |> Lwt_result.lift in
  Lwt_result.map_error (access_denied database_label)
  @@
  let ctx = Database.to_ctx database_label in
  let* auth = Api_key.Actor.to_authorizable ~ctx api_key in
  let* effects = effects req in
  let tags = Pool_context.Logger.Api.Tags.context context in
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
      let htmx_response () =
        Http_response.Htmx.inline_error Language.En Error.AccessDenied
        |> Http_response.Htmx.of_html
        |> Lwt.return
      in
      (match is_hx_request req with
       | true -> htmx_response ()
       | false ->
         let open Http_utils in
         (match Api.is_api_request req with
          | true ->
            Http_response.Api.respond_error ~status:`Forbidden Error.AccessDenied
            |> Lwt.return
          | false ->
            Http_utils.redirect_to
            @@
              (match Http_utils.is_req_from_root_host req with
              | false -> "/denied"
              | true -> "/root/denied")))
  in
  Rock.Middleware.create ~name:"guardian.generic" ~filter
;;

let validate_admin_entity ?any_id effects =
  validate_web_access_request_dependent ?any_id (fun (_ : Rock.Request.t) ->
    Lwt_result.return effects)
  |> validate_admin_entity_base
;;

let validate_generic ?any_id make_set =
  validate_web_access_request_dependent ?any_id (make_set %> Lwt_result.lift)
  |> validate_admin_entity_base
;;

let validate_generic_lwt ?any_id =
  validate_web_access_request_dependent ?any_id %> validate_admin_entity_base
;;

let id_effects encode field make_set =
  let open CCResult.Infix in
  (fun req -> Http_utils.find_id_save encode field req >|= make_set) |> validate_generic
;;

let api_validate_admin_entity ?any_id effects =
  validate_api_access_request_dependent ?any_id (fun (_ : Rock.Request.t) ->
    Lwt_result.return effects)
  |> validate_admin_entity_base
;;

let api_validate_generic ?any_id make_set =
  validate_api_access_request_dependent ?any_id (make_set %> Lwt_result.lift)
  |> validate_admin_entity_base
;;

let api_id_effects encode field make_set =
  let open CCResult.Infix in
  (fun req -> Http_utils.find_id_save encode field req >|= make_set)
  |> api_validate_generic
;;

let denied =
  Rock.Middleware.create ~name:"guardian.denied" ~filter:(fun _ _ ->
    Http_utils.redirect_to "/denied")
;;
