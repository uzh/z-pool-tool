open CCFun
open Utils.Lwt_result.Infix
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let field = Field.Tag
let base_path = "/admin/settings/tags"
let src = Logs.Src.create "handler.admin.settings_tags"
let active_navigation = base_path
let id req = Sihl.Web.Router.param req @@ Field.show field |> Tags.Id.of_string

let show req =
  let result ({ Pool_context.database_label; _ } as context) =
    Tags.find_all database_label
    ||> Page.Admin.Settings.Tags.index context
    >|> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, "/"
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let new_form req =
  let result context =
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Page.Admin.Settings.Tags.new_form ~flash_fetcher context
    |> General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, base_path
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let edit req =
  let result ({ Pool_context.database_label; _ } as context) =
    let id = HttpUtils.find_id Tags.Id.of_string Field.Tag req in
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    Tags.find database_label id
    >|+ Page.Admin.Settings.Tags.edit ~flash_fetcher context
    >>= General.create_tenant_layout req ~active_navigation context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, base_path
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let write action req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let redirect, success =
    let open Pool_common in
    match action with
    | `Create -> base_path, Message.Created field
    | `Update id ->
      ( Format.asprintf "%s/%s/edit" base_path (Tags.Id.value id)
      , Message.Updated field )
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Tags_command in
      match action with
      | `Create ->
        Create.(urlencoded |> decode |> Lwt_result.lift >== handle ~tags)
      | `Update id ->
        let* (_ : Tags.t) = Tags.find database_label id in
        Update.(urlencoded |> decode |> Lwt_result.lift >== handle ~tags)
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
let update req = write (`Update (id req)) req

module Access : sig
  include module type of Helpers.Access

  val assign_tag_to_contact : Rock.Middleware.t
  val remove_tag_from_contact : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian
  module Command = Cqrs_command.Tags_command

  let contact_effects = Guardian.id_effects Contact.Id.of_string Field.Contact
  let tag_effects = Guardian.id_effects Tags.Id.of_string Field.Tag
  let index = Tags.Guard.Access.index |> Guardian.validate_admin_entity
  let create = Command.Create.effects |> Guardian.validate_admin_entity
  let read = Tags.Guard.Access.read |> tag_effects |> Guardian.validate_generic

  let update =
    Command.Update.effects |> tag_effects |> Guardian.validate_generic
  ;;

  let assign_tag_to_contact =
    Command.AssignTagToContact.effects
    |> contact_effects
    |> Guardian.validate_generic
  ;;

  let remove_tag_from_contact =
    Command.RemoveTagFromContact.effects
    |> contact_effects
    |> Guardian.validate_generic
  ;;
end
