module HttpUtils = Http_utils

let create_layout = Contact_general.create_layout

let show req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    HttpUtils.(
      get_field_router_param req Experiment, get_field_router_param req Session)
  in
  let error_path =
    Format.asprintf "/experiments/%s" (experiment_id |> Pool_common.Id.value)
  in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let* contact = HttpUtils.get_current_contact tenant_db req in
    let* experiment = Experiment.find_public tenant_db experiment_id contact in
    let* () =
      Assignment.find_by_experiment_and_contact_opt
        tenant_db
        experiment.Experiment.Public.id
        contact
      >|> function
      | Some _ ->
        Lwt.return_error Pool_common.Message.AlreadySignedUpForExperiment
      | None -> Lwt.return_ok ()
    in
    let* session = Session.find_public tenant_db id contact in
    Page.Contact.Experiment.Assignment.detail session experiment context
    |> Lwt.return_ok
    >>= create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
