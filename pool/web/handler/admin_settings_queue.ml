open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Command = Cqrs_command.Queue_command
module Response = Http_response

let src = Logs.Src.create "handler.admin.settings_queue"
let base_path = "/admin/settings/queue"
let job_id = HttpUtils.find_id Pool_queue.Id.of_string Field.Queue
let show = Helpers.QueueJobs.htmx_handler `Current
let show_archive = Helpers.QueueJobs.htmx_handler `History

let detail req =
  let result ({ Pool_context.database_label; _ } as context) =
    let open Pool_queue in
    let open JobName in
    let id = job_id req in
    let* instance = find database_label id >|- Response.not_found in
    Response.bad_request_render_error context
    @@
    let%lwt text_message_dlr =
      match Instance.name instance with
      | SendTextMessage -> Text_message.find_report_by_queue_id database_label id
      | SendEmail | CheckMatchesFilter -> Lwt.return_none
    in
    Page.Admin.Settings.Queue.detail context ?text_message_dlr instance
    |> General.create_tenant_layout req ~active_navigation:base_path context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let resend req =
  let open Utils.Lwt_result.Infix in
  let open Pool_queue in
  let id = job_id req in
  let path = Format.asprintf "%s/%s" base_path (Id.value id) in
  let result { Pool_context.database_label; user; _ } =
    let* job = find database_label id >|- Response.not_found in
    Response.bad_request_on_error show
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let find_related = find_related database_label job in
    let%lwt job_contact =
      find_related History.User
      >|> CCOption.map_or ~default:Lwt.return_none (fun contact_id ->
        let open Contact in
        contact_id |> Id.of_common |> find database_label ||> CCResult.to_opt)
    in
    let%lwt job_experiment =
      find_related History.Experiment
      >|> CCOption.map_or ~default:Lwt.return_none (fun experiment_id ->
        let open Experiment in
        experiment_id |> Id.of_common |> find database_label ||> CCResult.to_opt)
    in
    let* () =
      Command.Resend.handle ?contact:job_contact ?experiment:job_experiment job
      |> Lwt_result.lift
      |>> Pool_event.handle_events ~tags database_label user
    in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_message.(Success.Resent Field.Message) ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

module Access : sig
  include module type of Helpers.Access

  val resend : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index =
    Pool_queue.Guard.Access.index () |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let read = Pool_queue.Guard.Access.read |> Guardian.validate_admin_entity
  let resend = Command.Resend.effects |> Guardian.validate_admin_entity
end
