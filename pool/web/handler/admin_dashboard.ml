module Response = Http_response

let src = Logs.Src.create "handler.admin.dashboard"
let create_layout req = General.create_tenant_layout req

let statistics_from_request req db_ctx =
  let open CCOption.Infix in
  let period =
    Sihl.Web.Request.query Pool_message.Field.(show Period) req >>= Statistics.read_period
  in
  let%lwt statistics = Statistics.Pool.create ?period db_ctx () in
  Lwt.return (period, statistics)
;;

let sessions_query_from_req req =
  let open Session in
  Query.from_request ~sortable_by ~default:incomplete_default_query req
;;

let index req =
  let result ({ Pool_context.user; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Pool_context.connection context @@ fun db_ctx ->
    let* actor =
      Pool_context.Utils.find_authorizable db_ctx user >|- Response.not_found
    in
    Response.bad_request_render_error context
    @@
    let%lwt clean_layout =
      let open Guard in
      let open CCList in
      let recruiter_roles : Role.Role.t list = [ `Operator; `Recruiter ] in
      Persistence.ActorRole.find_by_actor db_ctx actor.Actor.uuid
      ||> find_opt (fun (role, _, _) -> mem role.ActorRole.role recruiter_roles)
      ||> CCOption.is_some
    in
    let%lwt statistics =
      Guard.Persistence.validate db_ctx Statistics.Guard.Access.read actor
      ||> CCResult.is_ok
      >|> function
      | true -> statistics_from_request req db_ctx ||> CCOption.pure
      | false -> Lwt.return_none
    in
    let%lwt duplicate_contacts_count =
      match%lwt Helpers.Guard.can_manage_duplicate_contacts context with
      | false -> Lwt.return_none
      | true -> Duplicate_contacts.count db_ctx ||> CCOption.pure
    in
    let query = sessions_query_from_req req in
    let%lwt incomplete_sessions =
      Session.find_incomplete_by_admin ~query actor db_ctx
    in
    let open Page.Admin.Dashboard in
    let%lwt layout =
      if clean_layout
      then Clean incomplete_sessions |> Lwt.return
      else (
        let%lwt upcoming_sessions =
          Session.find_upcoming_by_admin ~query actor db_ctx
        in
        Admin (incomplete_sessions, upcoming_sessions) |> Lwt.return)
    in
    index statistics duplicate_contacts_count layout context
    |> create_layout req ~active_navigation:"/admin/dashboard" context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let htmx_session_helper table req =
  let result ({ Pool_context.language; user; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Pool_context.connection context @@ fun db_ctx ->
    let* actor = Pool_context.Utils.find_authorizable db_ctx user in
    let%lwt sessions =
      let query = sessions_query_from_req req in
      (fun fnc -> fnc ?query:(Some query) actor db_ctx)
      @@
      match table with
      | `incomplete -> Session.find_incomplete_by_admin
      | `upcoming -> Session.find_upcoming_by_admin
    in
    let html =
      let open Page.Admin.Dashboard.Partials in
      match table with
      | `incomplete -> incomplete_sessions_list
      | `upcoming -> upcoming_sessions_list
    in
    html language sessions |> Response.Htmx.of_html |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;

let incomplete_sessions = htmx_session_helper `incomplete
let upcoming_sessions = htmx_session_helper `upcoming

let statistics req =
  let result ({ Pool_context.language; _ } as context) =
    let%lwt statistics = Pool_context.connection context @@ statistics_from_request req in
    Component.Statistics.Pool.create language statistics
    |> Response.Htmx.of_html
    |> Lwt.return_ok
  in
  Response.Htmx.handle ~src req result
;;

module Access : sig
  module Statistics : module type of Helpers.Access
end = struct
  module Guardian = Middleware.Guardian

  module Statistics = struct
    include Helpers.Access

    let read = Statistics.Guard.Access.read |> Guardian.validate_admin_entity
  end
end
