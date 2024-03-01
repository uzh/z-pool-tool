module HttpUtils = Http_utils

let src = Logs.Src.create "handler.contact.session"
let create_layout = Contact_general.create_layout

let show req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_message.Field in
    ( HttpUtils.find_id Experiment.Id.of_string Experiment req
    , HttpUtils.find_id Session.Id.of_string Session req )
  in
  let error_path =
    Format.asprintf "/experiments/%s" (experiment_id |> Experiment.Id.value)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* experiment =
         Experiment.find_public database_label experiment_id contact
       in
       let* () =
         Assignment.find_all_public_by_experiment_and_contact_opt
           database_label
           (experiment |> Experiment.Public.id)
           contact
         >|> function
         | [] -> Lwt.return_ok ()
         | _ -> Lwt.return_error Pool_message.Error.AlreadySignedUpForExperiment
       in
       let* session = Session.find_public database_label id in
       let%lwt follow_ups =
         Session.find_follow_ups database_label id
         ||> CCList.map Session.to_public
       in
       Page.Contact.Experiment.Assignment.detail
         session
         follow_ups
         experiment
         context
       |> Lwt.return_ok
       >>= create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;
