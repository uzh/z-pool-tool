module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let%lwt contacts = Contact.find_all database_label () in
       Page.Admin.Contact.index context contacts
       |> create_layout req ~active_navigation:"/admin/contacts" context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail_view action req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/contacts")
    @@ let* contact =
         HttpUtils.get_field_router_param req Pool_common.Message.Field.Contact
         |> Pool_common.Id.of_string
         |> Contact.find database_label
       in
       match action with
       | `Show ->
         Page.Admin.Contact.detail context contact
         |> create_layout req context
         >|+ Sihl.Web.Response.of_html
       | `Edit ->
         let* tenant_languages =
           Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
         in
         let%lwt custom_fields =
           Custom_field.find_all_by_contact
             database_label
             user
             (Contact.id contact)
         in
         Page.Admin.Contact.edit context tenant_languages contact custom_fields
         |> create_layout req context
         >|+ Sihl.Web.Response.of_html
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
  let result { Pool_context.database_label; _ } =
    let%lwt contact =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Contact
      |> Pool_common.Id.of_string
      |> Contact.find database_label
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

module Access : Helpers.AccessSig = struct
  module ContactCommand = Cqrs_command.Contact_command

  let contact_effects =
    Middleware.Guardian.id_effects Contact.Id.of_string Field.Contact
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity [ `Read, `TargetEntity `Contact ]
  ;;

  let create = Middleware.Guardian.denied
  let delete = Middleware.Guardian.denied

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Contact.Id.value)
        ; `Read, `TargetEntity `Contact
        ])
    ]
    |> contact_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    [ ContactCommand.Update.effects ]
    |> contact_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
