module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.contact.waiting_list"
let create_layout = Contact_general.create_layout

let handle req action =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    let open Pool_message.Field in
    HttpUtils.find_id Experiment.Id.of_string Experiment req
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (Experiment.Id.value experiment_id)
  in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let* contact =
      Pool_context.find_contact context
      |> Lwt_result.lift
      >|- CCFun.const Response.access_denied
    in
    let* experiment =
      Experiment.find_public database_label experiment_id contact >|- Response.not_found
    in
    Response.bad_request_on_error Contact_experiment.show
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Waiting_list_command in
      match action with
      | `Create ->
        let tenant = Pool_context.Tenant.get_tenant_exn req in
        let* confirmation_email =
          Message_template.WaitingListConfirmation.create tenant contact experiment
        in
        { Waiting_list.contact; experiment }
        |> Create.handle ~tags confirmation_email
        |> Lwt_result.lift
      | `Destroy ->
        let%lwt waiting_list =
          Waiting_list.find_by_contact_and_experiment database_label contact experiment_id
        in
        let open CCResult.Infix in
        waiting_list
        |> CCOption.to_result Pool_message.(Error.NotFound Field.WaitingList)
        >>= Destroy.handle ~tags
        |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      let success_message =
        let open Pool_message.Success in
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
  Response.handle ~src req result
;;

let create req = handle req `Create
let delete req = handle req `Destroy
