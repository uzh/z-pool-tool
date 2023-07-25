open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.experiments.tags"
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let handle_tag action req =
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id = experiment_id req in
  let path =
    experiment_id
    |> Experiment.Id.value
    |> Format.asprintf "/admin/experiments/%s/edit"
  in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err -> err, path)
    @@ let* experiment = Experiment.find database_label experiment_id in
       let* message, events =
         match action with
         | `Assign ->
           let open Cqrs_command.Tags_command.AssignTagToExperiment in
           urlencoded
           |> decode
           |> Lwt_result.lift
           >== handle ~tags experiment
           >|+ CCPair.make Pool_common.Message.TagAssigned
         | `Remove ->
           let open Cqrs_command.Tags_command.RemoveTagFromExperiment in
           HttpUtils.find_id Tags.Id.of_string Field.Tag req
           |> Tags.find database_label
           >== handle experiment
           >|+ CCPair.make Pool_common.Message.TagRemoved
       in
       let handle =
         Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
       in
       let return_to_overview () =
         HttpUtils.redirect_to_with_actions
           path
           [ Message.set ~success:[ message ] ]
       in
       events |> handle >|> return_to_overview |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let assign_tag = handle_tag `Assign
let remove_tag = handle_tag `Remove
