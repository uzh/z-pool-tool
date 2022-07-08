module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let%lwt contacts = Contact.find_all tenant_db () in
       Page.Admin.Contact.index context contacts
       |> create_layout req ~active_navigation:"/admin/contacts" context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail_view action req =
  (* TODO: Impelement authorization *)
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/contacts")
    @@
    let open Lwt_result.Syntax in
    let* contact =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Contact
      |> Pool_common.Id.of_string
      |> Contact.find tenant_db
    in
    match action with
    | `Show ->
      Page.Admin.Contact.detail context contact
      |> create_layout req context
      >|= Sihl.Web.Response.of_html
    | `Edit ->
      let user_update_csrf = Contact_user_profile.user_update_csrf in
      let* tenant_languages =
        Pool_context.Tenant.find req
        |> Lwt_result.lift
        >|= fun c -> c.Pool_context.Tenant.tenant_languages
      in
      Page.Admin.Contact.edit context user_update_csrf tenant_languages contact
      |> create_layout req context
      >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail = detail_view `Show
let edit = detail_view `Edit

let update _ =
  (* TODO: Impelement authorization *)
  let open Tyxml.Html in
  div [ txt "This handler is not implemented yet." ]
  |> CCList.pure
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;
