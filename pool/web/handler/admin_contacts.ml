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
      let user_update_csrf = Htmx.user_update_csrf in
      let* tenant_languages =
        Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
      in
      let%lwt custom_fields =
        Custom_field.find_all_by_contact
          ~is_admin:true
          tenant_db
          (Contact.id contact)
      in
      Page.Admin.Contact.edit
        context
        user_update_csrf
        tenant_languages
        contact
        custom_fields
      |> create_layout req context
      >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail = detail_view `Show
let edit = detail_view `Edit

let update req =
  let redirect err =
    HttpUtils.htmx_redirect
      "/admin/contacts"
      ~actions:[ Message.set ~error:[ err ] ]
      ()
  in
  let result { Pool_context.tenant_db; _ } =
    let%lwt contact =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Contact
      |> Pool_common.Id.of_string
      |> Contact.find tenant_db
    in
    match contact with
    | Ok contact -> Helpers.PartialUpdate.update ~contact req
    | Error err -> redirect err
  in
  let context = req |> Pool_context.find in
  match context with
  | Ok context -> result context
  | Error err -> redirect err
;;
