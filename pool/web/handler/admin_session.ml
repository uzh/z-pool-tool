module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

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
  >|= List.assoc Field.(Location |> show)
  >|= Pool_location.Id.of_string
  >>= Pool_location.find tenant_db
;;

let list req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let experiment_id = id req Pool_common.Message.Field.Experiment in
    let* experiment = Experiment.find tenant_db experiment_id in
    let* sessions = Session.find_all_for_experiment tenant_db experiment_id in
    let grouped_sessions, chronological =
      match Sihl.Web.Request.query "chronological" req with
      | Some "true" -> CCList.map (fun s -> s, []) sessions, true
      | None | Some _ -> Session.group_and_sort sessions, false
    in
    let%lwt locations = Pool_location.find_all tenant_db in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Page.Admin.Session.index
      context
      experiment
      grouped_sessions
      chronological
      locations
      flash_fetcher
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
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
    let open Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Lwt_result.map_error (fun err ->
        err, path, [ HttpUtils.urlencoded_to_flash urlencoded ])
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
    let open Utils.Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, error_path)
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
      let%lwt locations = Pool_location.find_all tenant_db in
      Page.Admin.Session.edit context experiment session locations flash_fetcher
      |> Lwt.return_ok)
    >>= create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show req = detail req `Detail
let edit req = detail req `Edit

let update req =
  let experiment_id = id req Pool_common.Message.Field.Experiment in
  let session_id = id req Pool_common.Message.Field.session in
  let path =
    Format.asprintf
      "/admin/experiments/%s/sessions/%s"
      (Pool_common.Id.value experiment_id)
      (Pool_common.Id.value session_id)
  in
  let result context =
    let open Utils.Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Lwt_result.map_error (fun err ->
        ( err
        , Format.asprintf "%s/edit" path
        , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* location = location urlencoded tenant_db in
    let* session = Session.find tenant_db session_id in
    let* follow_ups = Session.find_follow_ups tenant_db session.Session.id in
    let* parent =
      match session.Session.follow_up_to with
      | None -> Lwt_result.return None
      | Some parent_id -> parent_id |> Session.find tenant_db >|= CCOption.some
    in
    let* events =
      let open CCResult.Infix in
      Cqrs_command.Session_command.Update.(
        urlencoded
        |> decode
        >>= handle ?parent_session:parent follow_ups session location)
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_common.Message.(Updated Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let disabler req command ctor =
  let error_path = "/admin/experiments/%s/sessions" in
  let result context =
    Lwt_result.map_error (fun err -> err, error_path)
    @@
    let open Utils.Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let session_id = id req Pool_common.Message.Field.Session in
    let* session = Session.find tenant_db session_id in
    let* events = session |> command |> Lwt_result.lift in
    let%lwt () = Pool_event.handle_events tenant_db events in
    let experiment_id = id req Pool_common.Message.Field.Experiment in
    Http_utils.redirect_to_with_actions
      (Format.asprintf
         "/admin/experiments/%s/sessions"
         (Pool_common.Id.value experiment_id))
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
    let open Utils.Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let session_id = id req Pool_common.Message.Field.session in
    let* session = Session.find tenant_db session_id in
    let* experiment = Experiment.find tenant_db experiment_id in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let%lwt locations = Pool_location.find_all tenant_db in
    Page.Admin.Session.follow_up
      context
      experiment
      session
      locations
      flash_fetcher
    |> Lwt.return_ok
    >>= create_layout req context
    >|= Sihl.Web.Response.of_html
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
    let open Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Lwt_result.map_error (fun err ->
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
