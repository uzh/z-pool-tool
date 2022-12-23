module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req

let id req field encode =
  Sihl.Web.Router.param req @@ Field.show field |> encode
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let%lwt template_list = Message_template.all_default database_label () in
       Page.Admin.MessageTemplate.index context template_list
       |> create_layout
            ~active_navigation:"/admin/message-templates"
            req
            context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let edit req =
  let open Utils.Lwt_result.Infix in
  let id = id req Field.MessageTemplate Message_template.Id.of_string in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let* template = Message_template.find database_label id in
       Page.Admin.MessageTemplate.edit context (Some template)
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let write ?id req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let redirect, success =
    let open Pool_common.Message in
    match id with
    (* TODO *)
    | None -> "redirect-back", Created Field.MessageTemplate
    | Some id ->
      ( id
        |> Message_template.Id.value
        |> Format.asprintf "/admin/message-templates/%s/edit"
      , Updated Field.MessageTemplate )
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Logger.req req in
    let events =
      let open Cqrs_command.Message_template_command in
      match id with
      | None -> failwith "TODO"
      | Some id ->
        let* template = Message_template.find database_label id in
        Update.(urlencoded |> decode |> Lwt_result.lift >== handle template)
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let update req =
  let id = id req Field.MessageTemplate Message_template.Id.of_string in
  write ~id req
;;

module Access : sig
  include Helpers.AccessSig
end = struct
  module Command = Cqrs_command.Message_template_command

  let template_effects =
    Middleware.Guardian.id_effects
      Message_template.Id.of_string
      Field.MessageTemplate
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity
      [ `Read, `TargetEntity `MessageTemplate ]
  ;;

  let create = Middleware.Guardian.denied
  let read = Middleware.Guardian.denied

  let update =
    [ Command.Update.effects ]
    |> template_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let delete = Middleware.Guardian.denied
end
