module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout = Contact_general.create_layout

let handle req action =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    let open Pool_common.Message.Field in
    HttpUtils.find_id Experiment.Id.of_string Experiment req
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (Experiment.Id.value experiment_id)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* experiment =
         Experiment.find_public database_label experiment_id contact
       in
       let tags = Logger.req req in
       let events =
         match action with
         | `Create ->
           Waiting_list.{ contact; experiment }
           |> Cqrs_command.Waiting_list_command.Create.handle ~tags
           |> Lwt_result.lift
         | `Destroy ->
           let* waiting_list =
             Waiting_list.find_by_contact_and_experiment
               database_label
               contact
               experiment
           in
           let open CCResult.Infix in
           waiting_list
           |> CCOption.to_result
                Pool_common.Message.(NotFound Field.WaitingList)
           >>= Cqrs_command.Waiting_list_command.Destroy.handle ~tags
           |> Lwt_result.lift
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event ~tags database_label) events
         in
         let success_message =
           let open Pool_common.Message in
           match action with
           | `Create -> AddedToWaitingList
           | `Destroy -> RemovedFromWaitingList
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set ~success:[ success_message ] ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req = handle req `Create
let delete req = handle req `Destroy
