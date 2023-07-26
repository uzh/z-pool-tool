open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.experiments.tags"
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let session_id = HttpUtils.find_id Session.Id.of_string Field.Session

let handle_tag action req =
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id = experiment_id req in
  let base_path =
    experiment_id
    |> Experiment.Id.value
    |> Format.asprintf "/admin/experiments/%s"
  in
  let path =
    match action with
    | `Assign
    | `AssignExperimentParticipationTag
    | `Remove
    | `RemoveExperimentParticipationTag -> Format.asprintf "%s" base_path
    | `AssignSessionParticipationTag | `RemoveSessionParticipationTag ->
      let session_id = session_id req in
      Format.asprintf
        "%s/sessions/%s/edit"
        base_path
        (Session.Id.value session_id)
  in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err -> err, path)
    @@ let* experiment = Experiment.find database_label experiment_id in
       let handle_assign decode handle =
         urlencoded
         |> decode
         |> Lwt_result.lift
         >== handle
         >|+ CCPair.make Pool_common.Message.TagAssigned
       in
       let handle_remove handle =
         HttpUtils.find_id Tags.Id.of_string Field.Tag req
         |> Tags.find database_label
         >== handle
         >|+ CCPair.make Pool_common.Message.TagRemoved
       in
       let* message, events =
         let open Tags.ParticipationTags in
         match action with
         | `Assign ->
           let open Cqrs_command.Tags_command.AssignTagToExperiment in
           let fnc = handle ~tags experiment in
           handle_assign decode fnc
         | `AssignExperimentParticipationTag ->
           let open Cqrs_command.Tags_command.AssignParticipationTagToEntity in
           let fnc =
             handle ~tags (Experiment (Experiment.Id.to_common experiment_id))
           in
           handle_assign decode fnc
         | `AssignSessionParticipationTag ->
           let open Cqrs_command.Tags_command.AssignParticipationTagToEntity in
           let session_id = session_id req in
           let fnc = handle ~tags (Session (Session.Id.to_common session_id)) in
           handle_assign decode fnc
         | `Remove ->
           let open Cqrs_command.Tags_command.RemoveTagFromExperiment in
           let fnc = handle ~tags experiment in
           handle_remove fnc
         | `RemoveExperimentParticipationTag ->
           let open Cqrs_command.Tags_command.RemoveParticipationTagFromEntity in
           let fnc =
             handle ~tags (Experiment (Experiment.Id.to_common experiment_id))
           in
           handle_remove fnc
         | `RemoveSessionParticipationTag ->
           let open Cqrs_command.Tags_command.RemoveParticipationTagFromEntity in
           let session_id = session_id req in
           let fnc = handle ~tags (Session (Session.Id.to_common session_id)) in
           handle_remove fnc
       in
       let handle =
         Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
       in
       let return_to_edit () =
         HttpUtils.redirect_to_with_actions
           path
           [ Message.set ~success:[ message ] ]
       in
       events |> handle >|> return_to_edit |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let assign_tag = handle_tag `Assign
let remove_tag = handle_tag `Remove

let assign_experiment_participation_tag =
  handle_tag `AssignExperimentParticipationTag
;;

let remove_experiment_participation_tag =
  handle_tag `RemoveExperimentParticipationTag
;;

let assign_session_participation_tag = handle_tag `AssignSessionParticipationTag
let remove_session_participation_tag = handle_tag `RemoveSessionParticipationTag
