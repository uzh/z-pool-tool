module HttpUtils = Http_utils
module Response = Http_response

let src = Logs.Src.create "handler.contact.session"
let create_layout = Contact_general.create_layout
let session_path = HttpUtils.Url.Contact.session_path

let index req =
  Response.Htmx.index_handler
    ~active_navigation:(session_path ())
    ~create_layout
    ~query:(module Session.Public)
    req
  @@ fun ({ Pool_context.database_label; user; language; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* contact = Pool_context.get_contact_user user |> Lwt_result.lift in
  let%lwt sessions = Session.query_by_contact ~query database_label contact in
  let open Page.Contact.Session in
  match HttpUtils.Htmx.is_hx_request req with
  | true -> list context sessions |> Lwt_result.return
  | false ->
    let%lwt title =
      I18n.find_by_key database_label I18n.Key.DashboardUpcomingSessions language
    in
    index context title sessions |> Lwt_result.return
;;

let show req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_message.Field in
    ( HttpUtils.find_id Experiment.Id.of_string Experiment req
    , HttpUtils.find_id Session.Id.of_string Session req )
  in
  let result ({ Pool_context.database_label; _ } as context) =
    let* contact =
      Pool_context.find_contact context
      |> Lwt_result.lift
      >|- CCFun.const Response.access_denied
    in
    let* experiment =
      Experiment.find_public database_label experiment_id contact >|- Response.not_found
    in
    Response.bad_request_render_error context
    @@
    let* () =
      Assignment.Public.find_all_by_experiment
        database_label
        (experiment |> Experiment.Public.id)
        contact
      >|> function
      | [] -> Lwt.return_ok ()
      | _ -> Lwt.return_error Pool_message.Error.AlreadySignedUpForExperiment
    in
    let* session = Session.find_public database_label id in
    let%lwt follow_ups =
      Session.find_follow_ups database_label id ||> CCList.map Session.to_public
    in
    Page.Contact.Assignment.detail session follow_ups experiment context
    |> Lwt.return_ok
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;
