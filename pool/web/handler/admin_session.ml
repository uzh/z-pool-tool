module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

(* Use this to extract ids from requests, the params are not named :id, because
   two ids appear in a route *)
let id req field =
  Sihl.Web.Router.param req @@ Pool_common.Message.Field.show field
  |> Pool_common.Id.of_string
;;

let list req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let experiment_id = id req Pool_common.Message.Field.Experiment in
    let* experiment = Experiment.find tenant_db experiment_id in
    let%lwt sessions =
      Session.find_all_for_experiment tenant_db experiment_id
    in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Page.Admin.Session.index context experiment sessions flash_fetcher
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
    let open Utils.Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Lwt_result.map_err (fun err ->
        err, path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* events =
      let open CCResult.Infix in
      Cqrs_command.Session_command.Create.(
        urlencoded |> decode >>= handle experiment_id)
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
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let session_id = id req Pool_common.Message.Field.session in
    let* session = Session.find tenant_db session_id in
    page context experiment_id session
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show req = detail req Page.Admin.Session.detail
let edit req = detail req Page.Admin.Session.edit

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
    Lwt_result.map_err (fun err -> err, path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* session = Session.find tenant_db session_id in
    let* events =
      let open CCResult.Infix in
      Cqrs_command.Session_command.Update.(
        urlencoded |> decode >>= handle session)
      |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events tenant_db events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_common.Message.(Created Field.Experiment) ]
      ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let disabler req command =
  let error_path = "/admin/experiments/%s/sessions" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
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
      [ Message.set ~success:[ Pool_common.Message.(Canceled Field.Session) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

(* TODO [aerben] add a confirmation before cancelling *)
(* TODO [aerben] if already canceled, allow uncancel *)
let cancel req = disabler req Cqrs_command.Session_command.Cancel.handle

(* TODO [aerben] add a confirmation before deleting *)
let delete req = disabler req Cqrs_command.Session_command.Delete.handle
