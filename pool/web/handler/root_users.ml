open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Database = Database
module RootCommand = Cqrs_command.Root_command

let src = Logs.Src.create "handler.root.users"
let tenants_path = "/root/tenants"
let active_navigation = "/root/users"

let index req =
  let context = Pool_context.find_exn req in
  let%lwt root_list = Admin.all ~query:Admin.default_query Database.Pool.Root.label in
  let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
  Page.Root.Users.list root_list context flash_fetcher
  |> General.create_root_layout ~active_navigation context
  ||> Sihl.Web.Response.of_html
;;

let create req =
  let result { Pool_context.database_label; user; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let tags = Pool_context.Logger.Tags.req req in
    let create_user () =
      HttpUtils.find_in_urlencoded
        ~error:Error.EmailAddressMissingRoot
        Field.Email
        urlencoded
      |> Lwt_result.lift
      >== Pool_user.EmailAddress.create
      >>= HttpUtils.validate_email_existance database_label
    in
    let events () =
      let open CCResult.Infix in
      RootCommand.Create.(urlencoded |> decode >>= handle ~tags)
    in
    let handle = Pool_event.handle_events ~tags Database.Pool.Root.label user in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        tenants_path
        [ Message.set ~success:[ Success.Created Field.Root ] ]
    in
    create_user ()
    >== events
    >|- (fun err -> err, active_navigation, [ HttpUtils.urlencoded_to_flash urlencoded ])
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let toggle_status req =
  let result { Pool_context.database_label; user; _ } =
    let open CCFun in
    let tags = Pool_context.Logger.Tags.req req in
    let id = HttpUtils.find_id Admin.Id.of_string Field.Admin req in
    let events = RootCommand.ToggleStatus.handle ~tags %> Lwt_result.lift in
    let handle = Pool_event.handle_events ~tags database_label user in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        tenants_path
        [ Message.set ~success:[ Success.Updated Field.Root ] ]
    in
    id
    |> Admin.find Database.Pool.Root.label
    >>= events
    >|- (fun err -> err, active_navigation)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : sig
  include module type of Helpers.Access

  val toggle_status : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Command = Cqrs_command.Root_command
  module Guardian = Middleware.Guardian

  let root_effects = Guardian.id_effects Admin.Id.validate Field.Root
  let index = Admin.Guard.Access.index |> Guardian.validate_admin_entity
  let create = Guardian.validate_admin_entity Command.Create.effects
  let read = root_effects Admin.Guard.Access.read
  let toggle_status = Command.ToggleStatus.effects |> Guardian.validate_admin_entity
end
