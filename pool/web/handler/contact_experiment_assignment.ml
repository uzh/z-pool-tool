module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    Sihl.Web.Router.param req Pool_common.Message.Field.(Experiment |> show)
    |> Pool_common.Id.of_string
  in
  let redirect_path =
    Format.asprintf "/experiments/%s" (experiment_id |> Pool_common.Id.value)
  in
  let result context =
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let id =
      Sihl.Web.Router.param req Pool_common.Message.Field.(Id |> show)
      |> Pool_common.Id.of_string
    in
    (* TODO[timhub]: Check if user is already enrolled *)
    let* contact = HttpUtils.get_current_contact tenant_db req in
    let* experiment =
      Experiment_type.find_public tenant_db experiment_id contact
    in
    let* session = Session.find_public tenant_db id contact in
    let events =
      Cqrs_command.Assignment_command.Create.(
        handle { contact; session; experiment })
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
