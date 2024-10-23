open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Command = Cqrs_command.Settings_command

let create_layout req = General.create_tenant_layout req
let base_path = "/admin/settings/text-messages"
let src = Logs.Src.create "handler.admin.setting_text_messages"
let active_navigation = base_path

let index req =
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, "/admin")
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    Page.Admin.Settings.TextMessage.index context ~flash_fetcher tenant
    |> create_layout ~active_navigation req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, base_path)
    @@
    let open Pool_tenant in
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* tenant =
      Pool_context.Tenant.get_tenant_exn req |> fun { id; _ } -> find_full id
    in
    let open Command.UpdateGtxApiKey in
    let* gtx_api_key = validated_gtx_api_key ~tags urlencoded in
    let events = handle ~tags tenant gtx_api_key |> Lwt_result.lift in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        base_path
        [ HttpUtils.Message.set
            ~success:[ Pool_message.(Success.Updated Field.GtxApiKey) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let delete req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, base_path)
    @@
    let open Pool_tenant in
    let tags = Pool_context.Logger.Tags.req req in
    let* tenant =
      Pool_context.Tenant.get_tenant_exn req |> fun { id; _ } -> find_full id
    in
    Command.RemoveGtxApiKey.handle ~tags tenant
    |> Lwt_result.lift
    |>> fun events ->
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions
      base_path
      [ HttpUtils.Message.set
          ~success:[ Pool_message.(Success.Deleted Field.GtxApiKey) ]
      ]
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let delivery_report req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let respond () =
    Sihl.Web.Response.of_plain_text "OK" ~status:`OK |> Lwt.return
  in
  let request_to_string req =
    Sihl.Web.Request.pp_hum Format.str_formatter req;
    Format.flush_str_formatter ()
  in
  let log_error err =
    Logs.err ~src (fun m ->
      m
        "An error occurred parsing the dlr report: %s"
        (Pool_message.Error.show err))
  in
  let log_request_with_ip message =
    let message = "text message dlr: " ^ message in
    Logging_helper.log_request_with_ip ~src message req tags None
  in
  let%lwt result =
    let* { Pool_context.database_label; user; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let* job_id =
      Lwt_result.lift
      @@
      try Ok (HttpUtils.find_id Pool_queue.Id.of_string Field.Queue req) with
      | _ ->
        log_request_with_ip "invalid queue job id provided";
        Error Pool_message.(Error.Invalid Field.Id)
    in
    let* (_ : Pool_queue.Instance.t) =
      Pool_queue.find database_label job_id
      >|- fun err ->
      Format.asprintf "queue job %s not found" (Pool_queue.Id.value job_id)
      |> log_request_with_ip;
      err
    in
    let* () =
      let open Text_message in
      find_report_by_queue_id database_label job_id
      ||> function
      | Some (_ : delivery_report) ->
        Format.asprintf
          "delivery report for queue job %s already received"
          (Pool_queue.Id.value job_id)
        |> log_request_with_ip;
        Error Pool_message.Error.TextMessageDlrAlreadyReceived
      | None -> Ok ()
    in
    let raw = request_to_string req in
    let urlparams = Sihl.Web.Request.query_list req in
    let* events =
      let open CCResult.Infix in
      let open Cqrs_command.Queue_command.CreateTextMessageDeliveryReport in
      decode urlparams job_id raw >>= handle ~tags |> Lwt_result.lift
    in
    Pool_event.handle_events ~tags database_label user events |> Lwt_result.ok
  in
  result |> CCResult.map_err log_error |> CCFun.const (respond ())
;;

module Access : sig
  include module type of Helpers.Access

  val index : Rock.Middleware.t
  val update : Rock.Middleware.t
  val delete : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian
  module Command = Cqrs_command.Settings_command

  let index = Command.UpdateGtxApiKey.effects |> Guardian.validate_admin_entity
  let update = Command.UpdateGtxApiKey.effects |> Guardian.validate_admin_entity
  let delete = Command.RemoveGtxApiKey.effects |> Guardian.validate_admin_entity
end
