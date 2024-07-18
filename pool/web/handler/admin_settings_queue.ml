open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Command = Cqrs_command.Queue_command

let src = Logs.Src.create "handler.admin.settings_queue"
let base_path = "/admin/settings/queue"
let job_id = HttpUtils.find_id Pool_queue.Id.of_string Field.Queue

let show req =
  HttpUtils.Htmx.handler
    ~active_navigation:base_path
    ~error_path:"/admin"
    ~create_layout:General.create_tenant_layout
    ~query:(module Pool_queue.Mapping)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt queue = Pool_queue.find_by ~query database_label in
  let open Page.Admin.Settings.Queue in
  (if HttpUtils.Htmx.is_hx_request req then data_table else index) context queue
  |> Lwt_result.return
;;

let detail req =
  let result ({ Pool_context.database_label; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/settings/queue")
    @@
    let id = job_id req in
    let* instance = Pool_queue.find database_label id in
    Page.Admin.Settings.Queue.detail context instance
    |> General.create_tenant_layout req ~active_navigation:base_path context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let resend req =
  let open Utils.Lwt_result.Infix in
  let id = job_id req in
  let path = Format.asprintf "%s/%s" base_path (Pool_queue.Id.value id) in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* job = Pool_queue.find database_label id in
    let find_related = Pool_queue.Mapping.find_related database_label job in
    let%lwt job_contact =
      match%lwt find_related `contact with
      | None -> Lwt.return_none
      | Some contact_id ->
        let open Contact in
        contact_id |> Id.of_common |> find database_label ||> CCResult.to_opt
    in
    let%lwt job_experiment =
      match%lwt find_related `experiment with
      | None -> Lwt.return_none
      | Some experiment_id ->
        let open Experiment in
        experiment_id |> Id.of_common |> find database_label ||> CCResult.to_opt
    in
    let* () =
      Command.Resend.handle ?contact:job_contact ?experiment:job_experiment job
      |> Lwt_result.lift
      |>> Pool_event.handle_events ~tags database_label
    in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_message.(Success.Resent Field.Message) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : sig
  include module type of Helpers.Access

  val resend : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index = Pool_queue.Guard.Access.index |> Guardian.validate_admin_entity
  let read = Pool_queue.Guard.Access.read |> Guardian.validate_admin_entity
  let resend = Command.Resend.effects |> Guardian.validate_admin_entity
end
