open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_message.Field
module Response = Http_response

let src = Logs.Src.create "handler.admin.contacts"
let create_layout req = General.create_tenant_layout req
let contact_id = HttpUtils.find_id Contact.Id.of_string Field.Contact
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let contact_path = HttpUtils.Url.Admin.contact_path
let find_contact pool id = Contact.find pool id |> Response.not_found_on_error

let index req =
  Response.Htmx.index_handler
    ~active_navigation:(contact_path ())
    ~query:(module Contact)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun (Pool_context.{ database_label; user; _ } as context) query ->
  let* actor =
    Pool_context.Utils.find_authorizable ~admin_only:true database_label user
  in
  let%lwt contacts, query = Contact.list_by_user ~query database_label actor in
  let open Page.Admin.Contact in
  (if HttpUtils.Htmx.is_hx_request req then list else index) context contacts query
  |> Lwt_result.return
;;

let experiments_query_from_req req =
  let open Experiment in
  Query.from_request
    ~sortable_by
    ~default:Session.participation_default_query
    ~searchable_by
    ?filterable_by
    req
;;

let experiment_history_htmx req =
  let contact_id = contact_id req in
  let experiment_id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* contact = Contact.find database_label contact_id in
    let* experiment = Experiment.find database_label experiment_id in
    let%lwt assignments =
      Assignment.find_by_contact_and_experiment database_label experiment_id contact
    in
    Page.Admin.Contact.experiment_history_modal context experiment assignments
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  Response.Htmx.handle ~error_as_notification:true ~src req result
;;

let past_experiments_htmx req =
  let contact_id = contact_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* contact = Contact.find database_label contact_id in
    let%lwt experiments, query =
      let query = experiments_query_from_req req in
      Experiment.query_participation_history_by_contact ~query database_label contact
    in
    Page.Admin.Contact.experiment_history context contact experiments query
    |> Response.Htmx.of_html
    |> Lwt.return_ok
  in
  Response.Htmx.handle ~src req result
;;

let detail req =
  let result ({ Pool_context.database_label; _ } as context) =
    let open Contact in
    let contact_id = contact_id req in
    let* contact = find_contact database_label contact_id in
    Response.bad_request_render_error context
    @@
    let%lwt contact_tags =
      Tags.(find_all_of_entity database_label Model.Contact) (id contact |> Id.to_common)
    in
    let%lwt external_data_ids =
      Assignment.find_external_data_identifiers_by_contact database_label (id contact)
    in
    let%lwt admin_comment = find_admin_comment database_label (id contact) in
    let%lwt custom_fields =
      Custom_field.find_all_by_contact
        database_label
        context.Pool_context.user
        (id contact)
    in
    let%lwt past_experiments =
      let query = experiments_query_from_req req in
      Experiment.query_participation_history_by_contact ~query database_label contact
    in
    let%lwt can_manage_duplicates = Helpers.Guard.can_manage_duplicate_contacts context in
    let%lwt failed_login_attempt =
      Pool_user.FailedLoginAttempt.Repo.find_current
        database_label
        (Contact.email_address contact)
    in
    Page.Admin.Contact.detail
      ?admin_comment
      ~can_manage_duplicates
      context
      contact
      contact_tags
      external_data_ids
      custom_fields
      past_experiments
      failed_login_attempt
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let edit req =
  let result ({ Pool_context.database_label; user; _ } as context) =
    let id = contact_id req in
    let* contact = find_contact database_label id in
    Response.bad_request_render_error context
    @@
    let* actor = Pool_context.Utils.find_authorizable database_label user in
    let%lwt contact_tags =
      Tags.(find_all_of_entity database_label Model.Contact) (Contact.Id.to_common id)
    in
    let%lwt allowed_to_assign =
      Guard.Persistence.validate
        database_label
        (Cqrs_command.Tags_command.AssignTagToContact.effects id)
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
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let%lwt custom_fields = Custom_field.find_all_by_contact database_label user id in
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
  Response.handle ~src req result
;;

let update req =
  let result { Pool_context.database_label; _ } =
    let* contact =
      HttpUtils.get_field_router_param req Field.Contact
      |> Contact.Id.of_string
      |> Contact.find database_label
    in
    Helpers.PartialUpdate.update ~contact req |> Lwt_result.ok
  in
  Response.Htmx.handle ~src req result
;;

let delete_answer req =
  let tags = Pool_context.Logger.Tags.req req in
  let contact_id = contact_id req in
  let result { Pool_context.database_label; user; language; _ } =
    let is_admin = Pool_context.user_is_admin user in
    let* contact = contact_id |> Contact.find database_label in
    let* custom_field =
      HttpUtils.find_id Custom_field.Id.of_string Field.CustomField req
      |> Custom_field.find_by_contact ~is_admin database_label (Contact.id contact)
    in
    let* () =
      let open Cqrs_command.Contact_command.ClearAnswer in
      handle ~tags custom_field contact
      |> Lwt_result.lift
      |>> Pool_event.handle_events ~tags database_label user
    in
    let* custom_field =
      HttpUtils.find_id Custom_field.Id.of_string Field.CustomField req
      |> Custom_field.find_by_contact ~is_admin database_label (Contact.id contact)
    in
    Htmx.(
      custom_field_to_htmx
        ~hx_post:(admin_profile_hx_post (Contact.id contact))
        language
        is_admin
        custom_field
        ())
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;

let promote req =
  let tags = Pool_context.Logger.Tags.req req in
  let contact_id = contact_id req in
  let result { Pool_context.database_label; user; _ } =
    let* contact = contact_id |> find_contact database_label in
    Response.bad_request_on_error edit
    @@
    let open Cqrs_command.Admin_command.PromoteContact in
    handle ~tags Contact.(id contact |> Id.to_user)
    |> Lwt_result.lift
    |>> Pool_event.handle_events ~tags database_label user
    |>> fun () ->
    let open HttpUtils in
    redirect_to_with_actions
      (Url.Admin.admin_path
         ~id:Contact.(id contact |> Id.to_common |> Admin.Id.of_common)
         ())
      [ Message.set ~success:[ Pool_message.Success.ContactPromoted ] ]
  in
  Response.handle ~src req result
;;

let toggle_paused req =
  let id = contact_id req in
  let redirect_path = contact_path ~id ~suffix:"edit" () in
  let tags = Pool_context.Logger.Tags.req req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* contact = find_contact database_label id in
    Response.bad_request_on_error edit
    @@ Helpers.ContactUpdate.toggle_paused context redirect_path contact tags
  in
  Response.handle ~src req result
;;

let mark_as_deleted req =
  let id = contact_id req in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; user; _ } =
    let* contact = find_contact database_label id in
    Response.bad_request_on_error edit
    @@
    let* () =
      Session.has_upcoming_sessions database_label id
      ||> function
      | false -> Ok ()
      | true -> Error Pool_message.Error.DeleteContactUpcomingSessions
    in
    let open Cqrs_command.Contact_command in
    MarkAsDeleted.handle ~tags contact
    |> Lwt_result.lift
    |>> Pool_event.handle_events database_label user
    |>> fun () ->
    HttpUtils.redirect_to_with_actions
      (contact_path ())
      [ Message.set ~success:[ Pool_message.Success.ContactMarkedAsDeleted ] ]
  in
  Response.handle ~src req result
;;

let toggle_verified req =
  let id = contact_id req in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; user; _ } =
    let* contact = Contact.find database_label id |> Response.not_found_on_error in
    Response.bad_request_on_error edit
    @@
    let events = Cqrs_command.Contact_command.ToggleVerified.handle contact in
    events
    |> Lwt_result.lift
    |>> Pool_event.handle_events ~tags database_label user
    |>> fun () -> HttpUtils.redirect_to (contact_path ~id ~suffix:"edit" ())
  in
  Response.handle ~src req result
;;

let external_data_ids req =
  let contact_id = contact_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* contact = find_contact database_label contact_id in
    Response.bad_request_on_error edit
    @@
    let%lwt external_data_ids =
      Assignment.find_external_data_identifiers_by_contact database_label contact_id
    in
    Page.Admin.Contact.external_data_ids context contact external_data_ids
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let htmx_experiments_get req =
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
    Page.Admin.Contact.assign_contact_experiment_list context contact_id experiments
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  Response.Htmx.handle ~src ~error_as_notification:true req result
;;

let htmx_experiment_modal req =
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
      Experiment.contact_meets_criteria database_label experiment contact
    in
    Page.Admin.Contact.assign_contact_experiment_modal
      context
      contact_id
      experiment
      sessions
      matches_filter
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  Response.Htmx.handle ~error_as_notification:true ~src req result
;;

let enroll_contact_post req =
  let contact_id = contact_id req in
  let experiment_id = experiment_id req in
  let redirect_path = contact_path ~id:contact_id () in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; user; _ } =
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    let* contact = find_contact database_label contact_id in
    Response.bad_request_on_error ~urlencoded edit
    @@
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
      Message_template.AssignmentConfirmation.prepare
        ~follow_up_sessions
        tenant
        contact
        experiment
        session
    in
    let%lwt contact_is_enrolled =
      Experiment.contact_is_enrolled database_label experiment_id contact_id
    in
    let events =
      let open Cqrs_command.Assignment_command.Create in
      handle
        ~tags
        ~direct_enrollment_by_admin:true
        { contact; session; follow_up_sessions; experiment }
        confirmation
        contact_is_enrolled
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = (Pool_event.handle_events ~tags database_label user) events in
      HttpUtils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_message.Success.AssignmentCreated ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let message_history req =
  let queue_table = `History in
  let contact_id = contact_id req in
  Response.Htmx.index_handler
    ~query:(module Pool_queue)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun (Pool_context.{ database_label; _ } as context) query ->
  let* contact = Contact.find database_label contact_id in
  let%lwt messages =
    let open Pool_queue in
    find_instances_by_entity
      queue_table
      ~query
      database_label
      (History.User, Contact.Id.to_common contact_id)
  in
  let open Page.Admin in
  (if HttpUtils.Htmx.is_hx_request req
   then Queue.list context queue_table (Contact.message_history_url contact) messages
   else Contact.message_history context queue_table contact messages)
  |> Lwt_result.return
;;

let unblock req =
  let tags = Pool_context.Logger.Tags.req req in
  let contact_id = contact_id req in
  let result { Pool_context.database_label; user; _ } =
    let* contact = find_contact database_label contact_id in
    Response.bad_request_on_error edit
    @@
    let events =
      let open Contact in
      let open Cqrs_command.User_command.Unblock in
      contact |> user |> handle ~tags
    in
    let handle events =
      let%lwt () = (Pool_event.handle_events ~tags database_label user) events in
      HttpUtils.redirect_to_with_actions
        (contact_path ~id:contact_id ())
        [ Message.set ~success:[ Pool_message.Success.UserUnblocked ] ]
    in
    events |> Lwt_result.lift |>> handle
  in
  Response.handle ~src req result
;;

let changelog req =
  let id = contact_id req in
  let url = HttpUtils.Url.Admin.contact_path ~suffix:"changelog" ~id () in
  let to_human { Pool_context.database_label; language; _ } =
    Custom_field.changelog_to_human database_label language
  in
  Helpers.Changelog.htmx_handler ~to_human ~url (Contact.Id.to_common id) req
;;

module Duplicates = Admin_contact_duplicates

module Access : sig
  include module type of Helpers.Access

  val external_data_ids : Rock.Middleware.t
  val promote : Rock.Middleware.t
  val message_history : Rock.Middleware.t
  val changelog : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let contact_effects = Guardian.id_effects Contact.Id.validate Field.Contact
  let index = Contact.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true

  let create_contact_validation_set contact_fcn permission =
    (fun req ->
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
      :: (CCList.map (fun id -> permission, `Contact, Some id) (experiments @ sessions)
          |> CCList.map one_of_tuple)
      |> or_
      |> Lwt.return_ok)
    |> Guardian.validate_generic_lwt
  ;;

  let read = create_contact_validation_set Contact.Guard.Access.read Guard.Permission.Read

  let update =
    create_contact_validation_set Contact.Guard.Access.update Guard.Permission.Update
  ;;

  let external_data_ids = read
  let promote = Admin.Guard.Access.create |> Guardian.validate_admin_entity

  let message_history =
    contact_effects (fun id ->
      Pool_queue.Guard.Access.index ~id:(Guard.Uuid.target_of Contact.Id.value id) ())
  ;;

  let changelog = read
end
