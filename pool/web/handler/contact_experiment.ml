module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/dashboard" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let%lwt experiment_list =
         Experiment.find_all_public_by_contact database_label contact
       in
       Page.Contact.Experiment.index experiment_list context
       |> create_layout ~active_navigation:"/experiments" req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/experiments" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let id =
      let open Pool_common.Message.Field in
      HttpUtils.find_id Experiment.Id.of_string Experiment req
    in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let* experiment = Experiment.find_public database_label id contact in
    let* sessions =
      Session.find_all_public_for_experiment
        database_label
        contact
        experiment.Experiment.Public.id
    in
    let* session_user_is_assigned =
      let%lwt existing_assignment =
        Assignment.find_by_experiment_and_contact_opt
          database_label
          experiment.Experiment.Public.id
          contact
      in
      match existing_assignment with
      | None -> Lwt.return_ok None
      | Some assignment ->
        Session.find_public_by_assignment
          database_label
          assignment.Assignment.Public.id
        |> Lwt_result.map (fun session -> Some session)
    in
    let%lwt user_is_on_waiting_list =
      Waiting_list.user_is_enlisted database_label contact experiment
    in
    Page.Contact.Experiment.show
      experiment
      sessions
      session_user_is_assigned
      user_is_on_waiting_list
      context
    |> Lwt.return_ok
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
