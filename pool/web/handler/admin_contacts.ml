module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req
let contact_id = HttpUtils.find_id Contact.Id.of_string Field.Contact

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@
    let query =
      let open Contact in
      Query.from_request ~searchable_by ~sortable_by req
    in
    let%lwt contacts = Contact.find_all ~query database_label () in
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
         HttpUtils.get_field_router_param req Field.Contact
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
      HttpUtils.get_field_router_param req Field.Contact
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

let delete_answer req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let contact_id = contact_id req in
  let error_path =
    Format.asprintf "/admin/contacts/%s/edit" (Pool_common.Id.value contact_id)
  in
  let result { Pool_context.database_label; user; language; _ } =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let is_admin = Pool_context.user_is_admin user in
    let* contact = contact_id |> Contact.find database_label in
    let* custom_field =
      HttpUtils.find_id Custom_field.Id.of_string Field.CustomField req
      |> Custom_field.find_by_contact
           ~is_admin
           database_label
           (Contact.id contact)
    in
    let* () =
      let open Cqrs_command.Contact_command.ClearAnswer in
      handle ~tags custom_field contact
      |> Lwt_result.lift
      |>> Pool_event.handle_events ~tags database_label
    in
    let* custom_field =
      HttpUtils.find_id Custom_field.Id.of_string Field.CustomField req
      |> Custom_field.find_by_contact
           ~is_admin
           database_label
           (Contact.id contact)
    in
    Htmx.(
      custom_field_to_htmx
        ~hx_post:(admin_profile_hx_post (Contact.id contact))
        language
        is_admin
        custom_field
        ())
    |> CCList.pure
    |> HttpUtils.multi_html_to_plain_text_response ~status:200
    |> Lwt_result.return
  in
  result |> HttpUtils.extract_happy_path_htmx req
;;

module Access : sig
  include Helpers.AccessSig

  val delete_answer : Rock.Middleware.t
end = struct
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

  let delete_answer =
    [ ContactCommand.ClearAnswer.effects ]
    |> contact_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
