module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout req

(* Use this to extract ids from requests, the params are not named :id, because
   two ids appear in a route *)
let id req field =
  Sihl.Web.Router.param req @@ Pool_common.Message.Field.show field
  |> Pool_common.Id.of_string
;;

let location urlencoded tenant_db =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  Field.(Location |> show)
  |> CCList.pure
  |> HttpUtils.urlencoded_to_params urlencoded
  |> CCOption.to_result (NotFound Field.Location)
  |> Lwt_result.lift
  >|+ List.assoc Field.(Location |> show)
  >|+ Pool_location.Id.of_string
  >>= Pool_location.find tenant_db
;;

let reschedule_messages tenant_db sys_languages session =
  let open Utils.Lwt_result.Infix in
  let create_message sys_languages contact (session : Session.t) template =
    (* TODO[tinhub]: What text element are required? Do we need custom
       template? *)
    let name = Contact.fullname contact in
    let email = Contact.email_address contact in
    let session_overview =
      (CCList.map (fun lang ->
         ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
         , Session.(to_email_text lang session) )))
        sys_languages
    in
    Email.Helper.prepare_boilerplate_email
      template
      (email |> Pool_user.EmailAddress.value)
      (("name", name) :: session_overview)
  in
  let* assignments = Assignment.find_by_session tenant_db session.Session.id in
  let* default_language =
    CCList.head_opt sys_languages
    |> CCOption.to_result Pool_common.Message.(Retrieve Field.Language)
    |> Lwt_result.lift
  in
  let i18n_texts = Hashtbl.create ~random:true (CCList.length sys_languages) in
  Lwt_list.map_s
    (fun (Assignment.{ contact; _ } : Assignment.t) ->
      let message_language =
        CCOption.value ~default:default_language contact.Contact.language
      in
      match Hashtbl.find_opt i18n_texts message_language with
      | Some template ->
        create_message sys_languages contact session template |> Lwt_result.ok
      | None ->
        let* subject =
          I18n.(
            find_by_key tenant_db Key.RescheduleSessionSubject message_language)
        in
        let* text =
          I18n.(
            find_by_key tenant_db Key.RescheduleSessionText message_language)
        in
        let template =
          Email.CustomTemplate.
            { subject = Subject.I18n subject; content = Content.I18n text }
        in
        let () = Hashtbl.add i18n_texts message_language template in
        create_message sys_languages contact session template |> Lwt_result.ok)
    assignments
  ||> CCResult.flatten_l
;;

let list req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let experiment_id = id req Pool_common.Message.Field.Experiment in
    let* experiment = Experiment.find tenant_db experiment_id in
    let* sessions = Session.find_all_for_experiment tenant_db experiment_id in
    let grouped_sessions, chronological =
      match Sihl.Web.Request.query "chronological" req with
      | Some "true" -> CCList.map (fun s -> s, []) sessions, true
      | None | Some _ -> Session.group_and_sort sessions, false
    in
    Page.Admin.Session.index context experiment grouped_sessions chronological
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (experiment_id |> Pool_common.Id.value)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = Experiment.find tenant_db experiment_id in
       let%lwt locations = Pool_location.find_all tenant_db in
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let* sys_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       Page.Admin.Session.new_form
         context
         experiment
         locations
         sys_languages
         flash_fetcher
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (Pool_common.Id.value experiment_id)
  in
  let result context =
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/%s" path "create"
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* location = location urlencoded tenant_db in
    let* events =
      let open CCResult.Infix in
      Cqrs_command.Session_command.Create.(
        urlencoded |> decode >>= handle experiment_id location)
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_common.Message.(Created Field.Experiment) ]
      ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let detail req page =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (Pool_common.Id.value experiment_id)
  in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let session_id = id req Pool_common.Message.Field.session in
    let* session = Session.find tenant_db session_id in
    let* experiment = Experiment.find tenant_db experiment_id in
    (match page with
     | `Detail ->
       let* assignments =
         Assignment.find_by_session tenant_db session.Session.id
       in
       Page.Admin.Session.detail context experiment session assignments
       |> Lwt.return_ok
     | `Edit ->
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let* experiment = Experiment.find tenant_db experiment_id in
       let%lwt locations = Pool_location.find_all tenant_db in
       let* sys_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       Page.Admin.Session.edit
         context
         experiment
         session
         locations
         sys_languages
         flash_fetcher
       |> Lwt.return_ok
     | `Reschedule ->
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let* experiment = Experiment.find tenant_db experiment_id in
       Page.Admin.Session.reschedule_session
         context
         experiment
         session
         flash_fetcher
       |> Lwt.return_ok)
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show req = detail req `Detail
let edit req = detail req `Edit
let reschedule_form req = detail req `Reschedule

let update_handler action req =
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let session_id = id req Pool_common.Message.Field.session in
  let path =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      (Pool_common.Id.value experiment_id)
      (Pool_common.Id.value session_id)
  in
  let error_path, success_msg =
    let open Pool_common.Message in
    match action with
    | `Update -> "edit", Updated Field.session
    | `Reschedule -> "reschedule", Rescheduled Field.session
  in
  let result context =
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/%s" path error_path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* session = Session.find tenant_db session_id in
    let* follow_ups = Session.find_follow_ups tenant_db session.Session.id in
    let* parent =
      match session.Session.follow_up_to with
      | None -> Lwt_result.return None
      | Some parent_id -> parent_id |> Session.find tenant_db >|+ CCOption.some
    in
    let* events =
      match action with
      | `Update ->
        let* location = location urlencoded tenant_db in
        let open CCResult.Infix in
        Cqrs_command.Session_command.Update.(
          urlencoded
          |> decode
          >>= handle ?parent_session:parent follow_ups session location
          |> Lwt_result.lift)
      | `Reschedule ->
        let open Cqrs_command.Session_command.Reschedule in
        let* (decoded : Session.reschedule) =
          urlencoded |> decode |> Lwt_result.lift
        in
        let* system_languages =
          Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
        in
        let (Session.{ start; duration } : Session.reschedule) = decoded in
        let* messages =
          Session.{ session with start; duration }
          |> reschedule_messages tenant_db system_languages
        in
        decoded
        |> handle ?parent_session:parent follow_ups session messages
        |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ success_msg ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let update = update_handler `Update
let reschedule = update_handler `Reschedule

let disabler req command ctor =
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (Pool_common.Id.value experiment_id)
  in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let open Utils.Lwt_result.Infix in
    let tenant_db = context.Pool_context.tenant_db in
    let session_id = id req Pool_common.Message.Field.Session in
    let* session = Session.find tenant_db session_id in
    let* events = session |> command |> Lwt_result.lift in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      error_path
      [ Message.set ~success:[ Pool_common.Message.(ctor Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

(* TODO [aerben] add a confirmation before cancelling *)
(* TODO [aerben] if already canceled, allow uncancel *)
let cancel req =
  disabler req Cqrs_command.Session_command.Cancel.handle (fun m ->
    Pool_common.Message.Canceled m)
;;

(* TODO [aerben] add a confirmation before deleting *)
let delete req =
  disabler req Cqrs_command.Session_command.Delete.handle (fun m ->
    Pool_common.Message.Deleted m)
;;

let follow_up req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/sessions"
      (Pool_common.Id.value experiment_id)
  in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let session_id = id req Pool_common.Message.Field.session in
    let* parent_session = Session.find tenant_db session_id in
    let* experiment = Experiment.find tenant_db experiment_id in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* sys_languages =
      Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
    in
    let%lwt locations = Pool_location.find_all tenant_db in
    Page.Admin.Session.follow_up
      context
      experiment
      parent_session
      locations
      sys_languages
      flash_fetcher
    |> Lwt.return_ok
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

(* TODO [aerben] make possible to create multiple follow ups? *)
let create_follow_up req =
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let session_id = id req Pool_common.Message.Field.session in
  let path =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      (Pool_common.Id.value experiment_id)
      (Pool_common.Id.value session_id)
  in
  let result context =
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/follow-up" path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* location = location urlencoded tenant_db in
    let* session = Session.find tenant_db session_id in
    let* events =
      let open CCResult.Infix in
      Cqrs_command.Session_command.Create.(
        urlencoded
        |> decode
        >>= handle ~parent_session:session experiment_id location)
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      (Format.asprintf
         "/admin/experiments/%s/sessions"
         (Pool_common.Id.value experiment_id))
      [ Message.set ~success:[ Pool_common.Message.(Created Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;
