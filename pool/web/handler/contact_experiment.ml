module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let index req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let error_path = "/dashboard" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* contact = HttpUtils.get_current_contact tenant_db req in
    let%lwt expermient_list =
      Experiment.find_all_public_by_contact tenant_db contact
    in
    Page.Contact.Experiment.index expermient_list context
    |> create_layout ~active_navigation:"/experiments" req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/experiments" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Experiment
      |> Pool_common.Id.of_string
    in
    let* contact = HttpUtils.get_current_contact tenant_db req in
    let* experiment = Experiment.find_public tenant_db id contact in
    let* sessions =
      Session.find_all_public_for_experiment
        tenant_db
        contact
        experiment.Experiment.Public.id
    in
    let* session_user_is_assigned =
      let%lwt existing_assignment =
        Assignment.find_by_experiment_and_contact_opt
          tenant_db
          experiment.Experiment.Public.id
          contact
      in
      match existing_assignment with
      | None -> Lwt.return_ok None
      | Some assignment ->
        Session.find_public_by_assignment
          tenant_db
          assignment.Assignment.Public.id
        |> Lwt_result.map (fun session -> Some session)
    in
    let%lwt user_is_on_waiting_list =
      Waiting_list.user_is_enlisted tenant_db contact experiment
    in
    Page.Contact.Experiment.show
      experiment
      sessions
      session_user_is_assigned
      user_is_on_waiting_list
      context
    |> Lwt.return_ok
    >>= create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
