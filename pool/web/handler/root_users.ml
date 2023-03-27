open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Database = Pool_database
module Field = Pool_common.Message.Field
module RootCommand = Cqrs_command.Root_command

let tenants_path = "/root/tenants"
let active_navigation = "/root/users"

let index req =
  let context = Pool_context.find_exn req in
  let%lwt root_list = Admin.find_all Pool_database.root () in
  Page.Root.Users.list root_list context
  |> General.create_root_layout ~active_navigation context
  ||> Sihl.Web.Response.of_html
;;

let create req =
  let result { Pool_context.database_label; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let user () =
      Sihl.Web.Request.urlencoded Field.(Email |> show) req
      ||> CCOption.to_result Pool_common.Message.EmailAddressMissingRoot
      >>= HttpUtils.validate_email_existance database_label
    in
    let events () =
      let open CCResult.Infix in
      let open RootCommand.Create in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded |> decode >>= handle ~tags |> Lwt_result.lift
    in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags Database.root)
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        tenants_path
        [ Message.set ~success:[ Pool_common.Message.Created Field.Root ] ]
    in
    user ()
    >>= events
    >|- (fun err -> err, tenants_path)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let toggle_status req =
  let result { Pool_context.database_label; _ } =
    let open CCFun in
    let tags = Pool_context.Logger.Tags.req req in
    let id = HttpUtils.find_id Admin.Id.of_string Field.Admin req in
    let events = RootCommand.ToggleStatus.handle ~tags %> Lwt_result.lift in
    let handle =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label)
    in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        tenants_path
        [ Message.set ~success:[ Pool_common.Message.(Updated Field.Root) ] ]
    in
    id
    |> Admin.find Pool_database.root
    >>= events
    >|- (fun err -> err, tenants_path)
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

module Access : sig
  include module type of Helpers.Access

  val toggle_status : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Command = Cqrs_command.Root_command
  module Guardian = Middleware.Guardian

  let root_effects = Guardian.id_effects Pool_common.Id.of_string Field.Root

  let read_effects =
    Guard.(ValidationSet.One (Action.Read, TargetSpec.Entity `System))
  ;;

  let index = Guardian.validate_admin_entity read_effects
  let create = Guardian.validate_admin_entity Command.Create.effects
  let read = Guardian.validate_admin_entity read_effects

  let toggle_status =
    Command.ToggleStatus.effects |> root_effects |> Guardian.validate_generic
  ;;
end
