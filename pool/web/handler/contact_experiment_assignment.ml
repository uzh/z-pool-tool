module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    HttpUtils.(
      get_field_router_param req Experiment, get_field_router_param req Session)
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (experiment_id |> Pool_common.Id.value)
  in
  let result context =
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let* contact = HttpUtils.get_current_contact tenant_db req in
    let* experiment =
      Experiment_type.find_public tenant_db experiment_id contact
    in
    let* session = Session.find_public tenant_db id contact in
    let%lwt already_enrolled =
      let open Lwt.Infix in
      Assignment_type.find_opt_by_experiment tenant_db contact experiment
      >|= CCOption.is_some
    in
    let events =
      Cqrs_command.Assignment_command.Create.(
        handle { contact; session; experiment } already_enrolled)
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set
            ~success:[ Pool_common.Message.(AssignmentCreated) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
