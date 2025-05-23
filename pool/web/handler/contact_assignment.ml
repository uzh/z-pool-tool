module HttpUtils = Http_utils
module Response = Http_response

let src = Logs.Src.create "handler.contact.assignment"
let create_layout = Contact_general.create_layout

let create req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id, id =
    let open Pool_message.Field in
    ( HttpUtils.find_id Experiment.Id.of_string Experiment req
    , HttpUtils.find_id Session.Id.of_string Session req )
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (experiment_id |> Experiment.Id.value)
  in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let* contact =
      Pool_context.find_contact context
      |> Lwt_result.lift
      >|- CCFun.const Response.access_denied
    in
    let* experiment =
      Experiment.find_full_by_contact database_label experiment_id contact
      >|- Response.not_found
    in
    let* session = Session.find_open database_label id >|- Response.not_found in
    Response.bad_request_on_error Contact_session.show
    @@
    let%lwt follow_up_sessions = Session.find_follow_ups database_label id in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let%lwt confirmation_email =
      Message_template.AssignmentConfirmation.prepare
        ~follow_up_sessions
        tenant
        contact
        experiment
        session
    in
    let%lwt already_enrolled =
      Assignment.assignment_to_experiment_exists database_label experiment_id contact
    in
    let events =
      let open Cqrs_command.Assignment_command.Create in
      handle
        ~tags
        { contact; session; follow_up_sessions; experiment }
        confirmation_email
        already_enrolled
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set ~success:[ Pool_message.Success.AssignmentCreated ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;
