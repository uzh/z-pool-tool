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
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, base_path)
    @@
    let open Pool_tenant in
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* tenant =
      Pool_context.Tenant.get_tenant_exn req |> fun { id; _ } -> find_full id
    in
    let open Command.UpdateGtxApiKey in
    let* gtx_api_key =
      validated_gtx_api_key ~tags tenant.Write.gtx_sender urlencoded
    in
    let events = handle ~tags tenant gtx_api_key |> Lwt_result.lift in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
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
  let result { Pool_context.database_label; _ } =
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
    let%lwt () =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
    in
    Http_utils.redirect_to_with_actions
      base_path
      [ HttpUtils.Message.set
          ~success:[ Pool_message.(Success.Deleted Field.GtxApiKey) ]
      ]
  in
  result |> HttpUtils.extract_happy_path ~src req
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
