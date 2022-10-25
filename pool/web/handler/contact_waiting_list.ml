module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout = Contact_general.create_layout

let handle req action =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    Sihl.Web.Router.param req Pool_common.Message.Field.(Experiment |> show)
    |> Pool_common.Id.of_string
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (Pool_common.Id.value experiment_id)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* experiment =
         Experiment.find_public tenant_db experiment_id contact
       in
       let events =
         match action with
         | `Create ->
           Waiting_list.{ contact; experiment }
           |> Cqrs_command.Waiting_list_command.Create.handle
           |> Lwt_result.lift
         | `Destroy ->
           let* waiting_list =
             Waiting_list.find_by_contact_and_experiment
               tenant_db
               contact
               experiment
           in
           let open CCResult.Infix in
           waiting_list
           |> CCOption.to_result
                Pool_common.Message.(NotFound Field.WaitingList)
           >>= Cqrs_command.Waiting_list_command.Destroy.handle
           |> Lwt_result.lift
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event tenant_db) events
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
