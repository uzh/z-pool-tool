let src = Logs.Src.create "handler.admin.dashboard"
let create_layout req = General.create_tenant_layout req

let statistics_from_request req database_label =
  let open CCOption.Infix in
  let period =
    Sihl.Web.Request.query Pool_common.Message.Field.(show Period) req
    >>= Statistics.read_period
  in
  let%lwt statistics = Statistics.create ?period database_label () in
  Lwt.return (period, statistics)
;;

let sessions_query_from_req req =
  let open Session in
  Query.from_request ~sortable_by ~default:incomplete_default_query req
;;

let index req =
  let result ({ Pool_context.database_label; user; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/error")
    @@
    let* actor = Pool_context.Utils.find_authorizable database_label user in
    let%lwt recruiter_layout =
      let open Guard in
      let open CCList in
      let recruiter_roles : Role.Role.t list = [ `Operator; `Recruiter ] in
      Persistence.ActorRole.find_by_actor database_label actor.Actor.uuid
      ||> find_opt (fun (role, _, _) -> mem role.ActorRole.role recruiter_roles)
      ||> CCOption.is_some
    in
    let%lwt statistics =
      Guard.Persistence.validate
        database_label
        Statistics.Guard.Access.read
        actor
      ||> CCResult.is_ok
      >|> function
      | true -> statistics_from_request req database_label ||> CCOption.pure
      | false -> Lwt.return_none
    in
    let query = sessions_query_from_req req in
    let%lwt incomplete_sessions =
      Session.find_incomplete_by_admin ~query actor database_label
    in
    Page.Admin.Dashboard.index
      statistics
      incomplete_sessions
      recruiter_layout
      context
    |> create_layout req ~active_navigation:"/admin/dashboard" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Http_utils.extract_happy_path ~src req
;;

let incomplete_sessions req =
  let result { Pool_context.database_label; language; user; _ } =
    let open Utils.Lwt_result.Infix in
    let* actor = Pool_context.Utils.find_authorizable database_label user in
    let%lwt sessions =
      let query = sessions_query_from_req req in
      (fun fnc -> fnc ?query:(Some query) actor database_label)
        Session.find_incomplete_by_admin
    in
    let html = Page.Admin.Dashboard.Partials.incomplete_sessions_list in
    html language sessions
    |> Http_utils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result |> Http_utils.Htmx.handle_error_message req
;;

let statistics req =
  let result { Pool_context.database_label; language; _ } =
    let%lwt statistics = statistics_from_request req database_label in
    Component.Statistics.create language statistics
    |> Http_utils.Htmx.html_to_plain_text_response
    |> Lwt.return_ok
  in
  result
  |> Http_utils.Htmx.handle_error_message ~error_as_notification:true ~src req
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
