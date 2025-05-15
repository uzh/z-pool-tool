open Utils.Lwt_result.Infix
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Database = Database
module RootCommand = Cqrs_command.Root_command
module Response = Http_response

let src = Logs.Src.create "handler.root.users"
let pool_path = HttpUtils.Url.Root.pool_path
let active_navigation = "/root/users"

let index req =
  let context = Pool_context.find_exn req in
  let%lwt root_list = Admin.all ~query:Admin.default_query Database.Pool.Root.label in
  Page.Root.Users.list root_list context
  |> General.create_root_layout ~active_navigation context
  ||> Sihl.Web.Response.of_html
;;

let create req =
  let result { Pool_context.database_label; user; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    Response.bad_request_on_error ~urlencoded index
    @@
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
        (pool_path ())
        [ Message.set ~success:[ Success.Created Field.Root ] ]
    in
    create_user () >== events |>> handle |>> return_to_overview
  in
  Response.handle ~src req result
;;

let toggle_status req =
  let result { Pool_context.database_label; user; _ } =
    let open CCFun in
    Response.bad_request_on_error index
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let id = HttpUtils.find_id Admin.Id.of_string Field.Admin req in
    let events = RootCommand.ToggleStatus.handle ~tags %> Lwt_result.lift in
    let handle = Pool_event.handle_events ~tags database_label user in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        (pool_path ())
        [ Message.set ~success:[ Success.Updated Field.Root ] ]
    in
    id |> Admin.find Database.Pool.Root.label >>= events |>> handle |>> return_to_overview
  in
  Response.handle ~src req result
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
