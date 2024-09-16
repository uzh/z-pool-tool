module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_message.Field
module View = Page.Admin.OrganisationalUnit

let field = Field.OrganisationalUnit
let base_path = "/admin/organisational-unit"
let src = Logs.Src.create "handler.admin.organisational_units"
let create_layout req = General.create_tenant_layout req

let id req =
  Sihl.Web.Router.param req @@ Field.show Field.OrganisationalUnit
  |> Organisational_unit.Id.of_string
;;

let database_label_of_req req =
  let open CCResult in
  Pool_context.(req |> find >|= fun { database_label; _ } -> database_label)
;;

let index req =
  let active_navigation = base_path in
  let error_path = "/admin/dashboard" in
  HttpUtils.Htmx.handler
    ~active_navigation
    ~error_path
    ~create_layout
    ~query:(module Organisational_unit)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt organisational_unit_list, query =
    Organisational_unit.find_by query database_label
  in
  (if HttpUtils.Htmx.is_hx_request req then View.list else View.index)
    context
    organisational_unit_list
    query
  |> Lwt_result.return
;;

let write action req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let redirect, success =
    let open Pool_message.Success in
    match action with
    | `Create -> base_path, Created field
    | `Update id ->
      ( Format.asprintf "%s/%s/edit" base_path (Organisational_unit.Id.value id)
      , Updated field )
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Organisational_unit_command in
      match action with
      | `Create ->
        Create.(urlencoded |> decode |> Lwt_result.lift >== handle ~tags)
      | `Update id ->
        let* admin = Pool_context.get_admin_user user |> Lwt_result.lift in
        let* ou = Organisational_unit.find database_label id in
        Update.(
          urlencoded |> decode |> Lwt_result.lift >== handle ~tags admin ou)
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        base_path
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let create = write `Create

let update req =
  let id = id req in
  write (`Update id) req
;;

let show action req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, base_path)
    @@ let* html =
         match action with
         | `New -> View.create context |> Lwt_result.return
         | `Edit ->
           let* ou = req |> id |> Organisational_unit.find database_label in
           View.detail context ou None |> Lwt_result.return
       in
       html
       |> create_layout ~active_navigation:base_path req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let changelog req =
  let ou_id = id req in
  let url =
    HttpUtils.Url.Admin.organisational_unit_path
      ~suffix:"changelog"
      ~id:ou_id
      ()
  in
  let open Organisational_unit in
  Helpers.Changelog.htmx_handler
    ~version_history:(module VersionHistory)
    ~url
    (Id.to_common ou_id)
    req
;;

let new_form = show `New
let edit = show `Edit

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Command = Cqrs_command.Organisational_unit_command
  module Guardian = Middleware.Guardian

  let organisational_units =
    Guardian.id_effects
      Organisational_unit.Id.of_string
      Field.OrganisationalUnit
  ;;

  let index =
    Organisational_unit.Guard.Access.index
    |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create = Command.Create.effects |> Guardian.validate_admin_entity

  let update =
    Command.Update.effects |> organisational_units |> Guardian.validate_generic
  ;;
end
