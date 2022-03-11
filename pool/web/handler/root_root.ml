module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Database = Pool_database

let create req =
  let result context =
    let open Utils.Lwt_result.Infix in
    let user () =
      let tenant_db = context.Pool_context.tenant_db in
      Sihl.Web.Request.urlencoded "email" req
      ||> CCOption.to_result Pool_common.Message.EmailAddressMissingRoot
      >>= HttpUtils.validate_email_existance tenant_db
    in
    let events () =
      let open CCResult.Infix in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> Cqrs_command.Root_command.Create.decode
      >>= Cqrs_command.Root_command.Create.handle
      |> Lwt_result.lift
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event Database.root) in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        "/root/tenants"
        [ Message.set ~success:[ Pool_common.Message.(Created Root) ] ]
    in
    ()
    |> user
    >>= events
    |> Lwt_result.map_err (fun err -> err, "/root/tenants/")
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;

let toggle_status req =
  let open Utils.Lwt_result.Infix in
  let result context =
    let tenant_db = context.Pool_context.tenant_db in
    let id = Sihl.Web.Router.param req "id" |> Pool_common.Id.of_string in
    let events user =
      Cqrs_command.Root_command.ToggleStatus.handle user |> Lwt_result.lift
    in
    let handle = Lwt_list.iter_s (Pool_event.handle_event tenant_db) in
    let return_to_overview () =
      Http_utils.redirect_to_with_actions
        "/root/tenants"
        [ Message.set ~success:[ Pool_common.Message.(Updated Root) ] ]
    in
    id
    |> Root.find
    >>= events
    |> Lwt_result.map_err (fun err -> err, "/root/tenants/")
    |>> handle
    |>> return_to_overview
  in
  result |> HttpUtils.extract_happy_path req
;;
