module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.i18n"
let create_layout req = General.create_tenant_layout req
let i18n_path = HttpUtils.Url.Admin.i18n_path

module I18nMap = CCMap.Make (struct
    type t = I18n.Key.t

    let compare = compare
  end)

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let sort translations =
      let update m t =
        I18nMap.update
          (I18n.key t)
          (function
            | None -> Some [ t ]
            | Some values -> Some (t :: values))
          m
      in
      CCList.fold_left update I18nMap.empty translations
      |> I18nMap.to_seq
      |> CCList.of_seq
      |> CCList.stable_sort (fun (k1, _) (k2, _) -> I18n.Key.compare k1 k2)
      |> Lwt.return
    in
    let%lwt translation_list = I18n.find_all database_label () >|> sort in
    Page.Admin.I18n.list translation_list context
    |> create_layout req ~active_navigation:(i18n_path ()) context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let id =
    HttpUtils.get_field_router_param req Pool_message.Field.i18n
    |> Pool_common.Id.of_string
  in
  let result { Pool_context.database_label; user; _ } =
    Response.bad_request_on_error index
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let property () = I18n.find_with_default_content database_label id in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let events property =
      let open CCResult.Infix in
      let open Cqrs_command.I18n_command.Update in
      urlencoded |> decode >>= handle ~tags property
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        (i18n_path ())
        [ Message.set ~success:[ Pool_message.(Success.Updated Field.I18n) ] ]
    in
    () |> property ||> events |>> handle
  in
  Response.handle ~src req result
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Field = Pool_message.Field
  module I18nCommand = Cqrs_command.I18n_command
  module Guardian = Middleware.Guardian

  let i18n_effects = Guardian.id_effects Pool_common.Id.validate Field.I18n
  let index = I18n.Guard.Access.index |> Guardian.validate_admin_entity
  let update = i18n_effects I18nCommand.Update.effects
end
