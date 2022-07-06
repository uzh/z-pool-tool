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

let detail req =
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
    Page.Admin.Contact.detail context contact
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
