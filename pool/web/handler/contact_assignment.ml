module HttpUtils = Http_utils

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
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* experiment =
         Experiment.find_full_by_contact database_label experiment_id contact
       in
       let%lwt contact_person =
         Experiment.find_contact_person database_label experiment
       in
       let* session = Session.find_open database_label id in
       let%lwt follow_up_sessions = Session.find_follow_ups database_label id in
       let tenant = Pool_context.Tenant.get_tenant_exn req in
       let%lwt confirmation_email =
         Message_template.AssignmentConfirmation.prepare
           ~follow_up_sessions
           tenant
           contact
           experiment
           session
           contact_person
       in
       let%lwt already_enrolled =
         let open Utils.Lwt_result.Infix in
         Assignment.find_all_public_by_experiment_and_contact_opt
           database_label
           experiment.Experiment.id
           contact
         ||> CCList.is_empty
         ||> not
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
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ HttpUtils.Message.set
               ~success:[ Pool_message.Success.AssignmentCreated ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;
