module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.contacts"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req
let contact_id = HttpUtils.find_id Contact.Id.of_string Field.Contact
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let index req =
  HttpUtils.Htmx.handler
    ~active_navigation:"/admin/contacts"
    ~error_path:"/admin/dashboard"
    ~query:(module Contact)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun (Pool_context.{ database_label; user; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* actor =
    Pool_context.Utils.find_authorizable ~admin_only:true database_label user
  in
  let%lwt contacts, query =
    Contact.find_all
      ~query
      ~actor
      ~permission:Contact.Guard.Access.index_permission
      database_label
      ()
  in
  let open Page.Admin.Contact in
  (if HttpUtils.Htmx.is_hx_request req then list else index)
    context
    contacts
    query
  |> Lwt_result.return
;;

let detail_view action req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/contacts")
    @@ let* actor = Pool_context.Utils.find_authorizable database_label user in
       let* contact =
         HttpUtils.get_field_router_param req Field.Contact
         |> Pool_common.Id.of_string
         |> Contact.find database_label
       in
       let%lwt contact_tags =
         Tags.(find_all_of_entity database_label Model.Contact)
           (Contact.id contact)
       in
       match action with
       | `Show ->
         let%lwt external_data_ids =
           Assignment.find_external_data_identifiers_by_contact
             database_label
             (Contact.id contact)
         in
         let%lwt admin_comment =
           Contact.find_admin_comment database_label (Contact.id contact)
         in
         let%lwt custom_fields =
           Custom_field.find_all_by_contact
             database_label
             user
             (Contact.id contact)
         in
         Page.Admin.Contact.detail
           ?admin_comment
           context
           contact
           contact_tags
           external_data_ids
           custom_fields
         |> create_layout req context
         >|+ Sihl.Web.Response.of_html
       | `Edit ->
         let%lwt allowed_to_assign =
           Guard.Persistence.validate
             database_label
             (Contact.id contact
              |> Cqrs_command.Tags_command.AssignTagToContact.effects)
             actor
           ||> CCResult.is_ok
         in
         let%lwt allowed_to_promote =
           Guard.Persistence.validate
             database_label
             Cqrs_command.Admin_command.PromoteContact.effects
             actor
           ||> CCResult.is_ok
         in
         let%lwt all_tags =
           let open Tags in
           if allowed_to_assign
           then find_all_validated_with_model database_label Model.Contact actor
           else Lwt.return []
         in
         let tenant_languages =
           Pool_context.Tenant.get_tenant_languages_exn req
         in
         let%lwt custom_fields =
           Custom_field.find_all_by_contact
             database_label
             user
             (Contact.id contact)
         in
         Page.Admin.Contact.edit
           ~allowed_to_assign
           ~allowed_to_promote
           context
           tenant_languages
           contact
           custom_fields
           contact_tags
           all_tags
         |> create_layout req context
         >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let detail = detail_view `Show
let edit = detail_view `Edit

let update req =
  let redirect err =
    HttpUtils.Htmx.htmx_redirect
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

let promote req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let contact_id = contact_id req in
  let error_path =
    Format.asprintf "/admin/contacts/%s/edit" (Pool_common.Id.value contact_id)
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let open Cqrs_command.Admin_command.PromoteContact in
    let* contact = contact_id |> Contact.find database_label in
    handle ~tags (Contact.id contact)
    |> Lwt_result.lift
    |>> Pool_event.handle_events ~tags database_label
    |>> fun () ->
    HttpUtils.redirect_to_with_actions
      (Format.asprintf "/admin/admins/%s" (Pool_common.Id.value contact_id))
      [ Message.set ~success:[ Pool_common.Message.ContactPromoted ] ]
  in
  result |> HttpUtils.Htmx.extract_happy_path ~src req
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
    |> HttpUtils.Htmx.html_to_plain_text_response ~status:200
    |> Lwt_result.return
  in
  result |> HttpUtils.Htmx.extract_happy_path ~src req
;;

let toggle_paused req =
  let open Utils.Lwt_result.Infix in
  let id = contact_id req in
  let redirect_path =
    Format.asprintf "/admin/contacts/%s/edit" (Contact.Id.value id)
  in
  let tags = Pool_context.Logger.Tags.req req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* contact =
      Contact.find database_label id >|- fun err -> err, redirect_path
    in
    Helpers.ContactUpdate.toggle_paused context redirect_path contact tags
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let external_data_ids req =
  let open Utils.Lwt_result.Infix in
  let contact_id = contact_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err ->
      err, Format.asprintf "/admin/contacts/%s" (Contact.Id.value contact_id))
    @@
    let* contact = Contact.find database_label contact_id in
    let%lwt external_data_ids =
      Assignment.find_external_data_identifiers_by_contact
        database_label
        contact_id
    in
    Page.Admin.Contact.external_data_ids context contact external_data_ids
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let htmx_experiments_get req =
  let open Utils.Lwt_result.Infix in
  let contact_id = contact_id req in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let query = Sihl.Web.Request.query Field.(show Search) req in
    let* contact = Contact.find database_label contact_id in
    let* actor =
      Pool_context.Utils.find_authorizable ~admin_only:true database_label user
    in
    let%lwt experiments =
      query
      |> CCOption.map_or ~default:(Lwt.return []) (fun query ->
        Experiment.find_to_enroll_directly ~actor database_label contact ~query)
    in
    Page.Admin.Contact.assign_contact_experiment_list
      context
      contact_id
      experiments
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result
  |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let htmx_experiment_modal req =
  let open Utils.Lwt_result.Infix in
  let contact_id = contact_id req in
  let experiment_id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* contact = Contact.find database_label contact_id in
    let* experiment = Experiment.find database_label experiment_id in
    let%lwt sessions =
      Session.find_all_for_experiment database_label experiment_id
      ||> Session.group_and_sort
    in
    let%lwt matches_filter =
      experiment.Experiment.filter
      |> CCOption.map_or ~default:Lwt.return_true (fun { Filter.query; _ } ->
        Filter.contact_matches_filter database_label query contact)
    in
    Page.Admin.Contact.assign_contact_experiment_modal
      context
      contact_id
      experiment
      sessions
      matches_filter
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result
  |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let enroll_contact_post req =
  let open Utils.Lwt_result.Infix in
  let contact_id = contact_id req in
  let experiment_id = experiment_id req in
  let redirect_path =
    Format.asprintf "/admin/contacts/%s" (Contact.Id.value contact_id)
  in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; _ } =
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    let* contact = Contact.find database_label contact_id in
    let* experiment = Experiment.find database_label experiment_id in
    let* session =
      urlencoded
      |> HttpUtils.find_in_urlencoded Field.Session
      |> Lwt_result.lift
      >|+ Session.Id.of_string
      >>= Session.find database_label
    in
    let%lwt follow_up_sessions =
      Session.find_follow_ups database_label session.Session.id
    in
    let%lwt confirmation =
      let tenant = Pool_context.Tenant.get_tenant_exn req in
      let%lwt contact_person =
        Experiment.find_contact_person database_label experiment
      in
      Message_template.AssignmentConfirmation.prepare
        ~follow_up_sessions
        database_label
        tenant
        contact
        experiment
        session
        contact_person
    in
    let%lwt contact_is_enrolled =
      Experiment.contact_is_enrolled database_label experiment_id contact_id
    in
    let events =
      let open Cqrs_command.Assignment_command.Create in
      handle
        ~tags
        ~ignore_registration_disabled:true
        { contact; session; follow_up_sessions; experiment }
        confirmation
        contact_is_enrolled
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      HttpUtils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_common.Message.AssignmentCreated ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Tags = Admin_contacts_tags

module Access : sig
  include module type of Helpers.Access

  val external_data_ids : Rock.Middleware.t
  val delete_answer : Rock.Middleware.t
  val promote : Rock.Middleware.t
end = struct
  include Helpers.Access
  module ContactCommand = Cqrs_command.Contact_command
  module Guardian = Middleware.Guardian

  let contact_effects = Guardian.id_effects Contact.Id.of_string Field.Contact

  let index =
    Contact.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create_contact_validation_set contact_fcn permission =
    (fun req ->
      let open Utils.Lwt_result.Infix in
      let open Guard.ValidationSet in
      let* { Pool_context.database_label; _ } =
        req |> Pool_context.find |> Lwt_result.lift
      in
      let contact = HttpUtils.find_id Contact.Id.of_string Field.Contact req in
      let%lwt experiments =
        Experiment.find_all_ids_of_contact_id database_label contact
        ||> CCList.map (Guard.Uuid.target_of Experiment.Id.value)
      in
      let%lwt sessions =
        Session.find_all_ids_of_contact_id database_label contact
        ||> CCList.map (Guard.Uuid.target_of Session.Id.value)
      in
      contact_fcn contact
      :: (CCList.map
            (fun id -> permission, `Contact, Some id)
            (experiments @ sessions)
          |> CCList.map one_of_tuple)
      |> or_
      |> Lwt.return_ok)
    |> Guardian.validate_generic_lwt_result
  ;;

  let read =
    create_contact_validation_set
      Contact.Guard.Access.read
      Guard.Permission.Read
  ;;

  let update =
    create_contact_validation_set
      Contact.Guard.Access.update
      Guard.Permission.Update
  ;;

  let external_data_ids = read

  let delete_answer =
    ContactCommand.ClearAnswer.effects
    |> contact_effects
    |> Guardian.validate_generic
  ;;

  let promote = Admin.Guard.Access.create |> Guardian.validate_admin_entity
end
