open CCFun
open Pool_message
module Assignment = Admin_experiments_assignments
module Field = Field
module FilterEntity = Filter
module HttpUtils = Http_utils
module Invitations = Admin_experiments_invitations
module Mailings = Admin_experiments_mailing
module Message = HttpUtils.Message
module MessageTemplates = Admin_experiments_message_templates
module Response = Http_response
module Users = Admin_experiments_users
module WaitingList = Admin_experiments_waiting_list

let src = Logs.Src.create "handler.admin.experiments"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment
let experiment_path = Http_utils.Url.Admin.experiment_path

let find_entity_in_urlencoded urlencoded field fnc =
  let open Utils.Lwt_result.Infix in
  urlencoded
  |> HttpUtils.find_in_urlencoded_opt field
  |> function
  | None -> Lwt_result.return None
  | Some id -> id |> fnc >|+ CCOption.return
;;

let experiment_boolean_fields = Experiment.boolean_fields |> CCList.map Field.show
let customizable_message_templates = []

let contact_person_roles experiment_id =
  let base = [ `Recruiter, None; `Experimenter, None ] in
  match experiment_id with
  | None -> base
  | Some id ->
    id
    |> Guard.Uuid.target_of Experiment.Id.value
    |> fun target_id -> [ `Experimenter, Some target_id ] @ base
;;

let validation_set =
  let open Guard in
  let open ValidationSet in
  let base = [ Permission.Update, `Experiment, None ] in
  CCOption.map_or ~default:base (fun exp_id ->
    let id = Uuid.target_of Experiment.Id.value exp_id in
    base @ [ Permission.Update, `Experiment, Some id ])
  %> CCList.map one_of_tuple
  %> or_
;;

let contact_person_from_urlencoded db_ctx urlencoded experiment_id =
  let open Utils.Lwt_result.Infix in
  let query id =
    let* () =
      id
      |> Guard.Uuid.Actor.of_string
      |> CCOption.to_result (Error.NotFound Field.ContactPerson)
      |> Lwt_result.lift
      >>= (fun id ->
      Guard.Persistence.Actor.find db_ctx id >|- Error.authorization)
      >>= Guard.Persistence.validate db_ctx (validation_set experiment_id)
    in
    id |> Admin.Id.of_string |> Admin.find db_ctx
  in
  find_entity_in_urlencoded urlencoded Field.ContactPerson query
;;

let organisational_unit_from_urlencoded urlencoded db_ctx =
  let query = Organisational_unit.(Id.of_string %> find db_ctx) in
  find_entity_in_urlencoded urlencoded Field.OrganisationalUnit query
;;

let smtp_auth_from_urlencoded urlencoded db_ctx =
  let query auth_id = Email.SmtpAuth.(auth_id |> Id.of_string |> find db_ctx) in
  find_entity_in_urlencoded urlencoded Field.Smtp query
;;

let experiment_message_templates db_ctx experiment =
  let open Message_template in
  let open Experiment in
  let open Utils.Lwt_result.Infix in
  let find_templates label =
    find_all_of_entity_by_label
      db_ctx
      (experiment.Experiment.id |> Id.to_common)
      label
    ||> (fun templates ->
    match experiment.language with
    | None -> templates
    | Some experiment_language ->
      templates
      |> CCList.filter (fun { Message_template.language; _ } ->
        Pool_common.Language.equal language experiment_language))
    ||> CCPair.make label
  in
  customizable_label_by_experiment |> Lwt_list.map_s find_templates
;;

let index req =
  Response.Htmx.index_handler
    ~active_navigation:"/admin/experiments"
    ~create_layout
    ~query:(module Experiment)
    req
  @@ fun ({ Pool_context.user; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  Pool_context.connection context @@ fun db_ctx ->
  let* actor =
    Pool_context.Utils.find_authorizable ~admin_only:true db_ctx user
  in
  let%lwt experiments, query = Experiment.list_by_user ~query db_ctx actor in
  let open Page.Admin.Experiments in
  (if HttpUtils.Htmx.is_hx_request req then list else index) context experiments query
  |> Lwt_result.return
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.flash_fetcher; _ } as context) =
    Response.bad_request_render_error context
    @@
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    Pool_context.connection context @@ fun db_ctx ->
    let%lwt default_email_reminder_lead_time =
      Settings.find_default_reminder_lead_time db_ctx
    in
    let%lwt default_text_msg_reminder_lead_time =
      Settings.find_default_text_msg_reminder_lead_time db_ctx
    in
    let%lwt default_sender = Email.Service.default_sender_of_pool db_ctx in
    let%lwt organisational_units = Organisational_unit.all db_ctx () in
    let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    let%lwt smtp_auth_list = Email.SmtpAuth.find_all db_ctx in
    Page.Admin.Experiments.create
      ?flash_fetcher
      context
      tenant
      organisational_units
      default_email_reminder_lead_time
      default_text_msg_reminder_lead_time
      smtp_auth_list
      default_sender
      text_messages_enabled
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Response.handle ~src req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values experiment_boolean_fields
    ||> HttpUtils.remove_empty_values
  in
  let result ({ Pool_context.user; _ } as context) =
    Response.bad_request_on_error ~urlencoded new_form
    @@
    let tags = Pool_context.Logger.Tags.req req in
    Pool_context.connection context @@ fun db_ctx ->
    let* organisational_unit =
      organisational_unit_from_urlencoded urlencoded db_ctx
    in
    let* smtp_auth = smtp_auth_from_urlencoded urlencoded db_ctx in
    let id = Experiment.Id.create () in
    let* actor =
      Pool_context.Utils.find_authorizable ~admin_only:true db_ctx user
    in
    let%lwt role_events =
      let open Guard in
      let has_general_experimenter_permission actor =
        PermissionOnTarget.create Permission.Manage `Experiment
        |> ValidationSet.one
        |> flip (Persistence.validate db_ctx) actor
        ||> CCResult.is_ok
      in
      match%lwt actor |> has_general_experimenter_permission with
      | false ->
        ActorRole.create
          ~target_uuid:(Uuid.target_of Experiment.Id.value id)
          actor.Actor.uuid
          `Experimenter
        |> fun role ->
        RolesGranted [ role ] |> Pool_event.guard |> CCList.return |> Lwt.return
      | true -> Lwt.return []
    in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Experiment_command.Create in
      let%lwt default_public_title = Experiment.get_default_public_title db_ctx in
      urlencoded
      |> decode default_public_title
      >>= handle ~tags ~id ?organisational_unit ?smtp_auth
      |> Lwt_result.lift
    in
    let handle db_ctx events =
      let%lwt () = Pool_event.handle_events ~tags db_ctx user events in
      Http_utils.redirect_to_with_actions
        "/admin/experiments"
        [ Message.set ~success:[ Success.Created Field.Experiment ] ]
    in
    events >|+ flip ( @ ) role_events |>> handle db_ctx
  in
  result |> Response.handle ~src req
;;

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.user; flash_fetcher; _ } as context) =
    let id = experiment_id req in
    Pool_context.connection context @@ fun db_ctx ->
    let* experiment = Experiment.find db_ctx id |> Response.not_found_on_error in
    Response.bad_request_on_error index
    @@
    let* actor = Pool_context.Utils.find_authorizable db_ctx user in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let%lwt message_templates = experiment_message_templates db_ctx experiment in
    let%lwt current_tags =
      Tags.(find_all_of_entity db_ctx Model.Experiment)
        (id |> Experiment.Id.to_common)
    in
    let%lwt current_participation_tags =
      Tags.(
        ParticipationTags.find_all
          db_ctx
          (ParticipationTags.Experiment (Experiment.Id.to_common id)))
    in
    let%lwt session_count = Experiment.session_count db_ctx id in
    (match edit with
     | false ->
       let* smtp_auth =
         experiment.Experiment.smtp_auth_id
         |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
           Email.SmtpAuth.find db_ctx id >|+ CCOption.return)
       in
       let* statistics = Statistics.ExperimentOverview.create db_ctx experiment in
       let%lwt invitation_reset =
         Experiment.InvitationReset.find_latest_by_experiment db_ctx id
       in
       Page.Admin.Experiments.detail
         ?invitation_reset
         experiment
         session_count
         message_templates
         sys_languages
         smtp_auth
         current_tags
         current_participation_tags
         statistics
         context
       |> Lwt_result.ok
     | true ->
       let%lwt default_email_reminder_lead_time =
         Settings.find_default_reminder_lead_time db_ctx
       in
       let%lwt default_text_msg_reminder_lead_time =
         Settings.find_default_text_msg_reminder_lead_time db_ctx
       in
       let%lwt organisational_units = Organisational_unit.all db_ctx () in
       let%lwt smtp_auth_list = Email.SmtpAuth.find_all db_ctx in
       let%lwt default_sender = Email.Service.default_sender_of_pool db_ctx in
       let%lwt allowed_to_assign =
         Guard.Persistence.validate
           db_ctx
           (id
            |> Experiment.Id.to_common
            |> Contact.Id.of_common
            |> Cqrs_command.Tags_command.AssignTagToContact.effects)
           actor
         ||> CCResult.is_ok
       in
       let find_tags db_ctx model =
         let open Tags in
         if allowed_to_assign
         then find_all_validated_with_model db_ctx model actor
         else Lwt.return []
       in
       let%lwt experiment_tags = find_tags db_ctx Tags.Model.Experiment in
       let%lwt participation_tags = find_tags db_ctx Tags.Model.Contact in
       let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
       Page.Admin.Experiments.edit
         ?flash_fetcher
         ~allowed_to_assign
         ~session_count
         experiment
         context
         tenant
         default_email_reminder_lead_time
         default_text_msg_reminder_lead_time
         organisational_units
         smtp_auth_list
         default_sender
         (experiment_tags, current_tags)
         (participation_tags, current_participation_tags)
         text_messages_enabled
       |> Lwt_result.ok)
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> Response.handle ~src req
;;

let show = detail false
let edit = detail true

let changelog req =
  let experiment_id = experiment_id req in
  let url =
    HttpUtils.Url.Admin.experiment_path ~suffix:"changelog" ~id:experiment_id ()
  in
  let open Experiment in
  Helpers.Changelog.htmx_handler ~url (Id.to_common experiment_id) req
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.user; _ } as context) =
    let id = experiment_id req in
    Pool_context.connection context @@ fun db_ctx ->
    let* experiment = Experiment.find db_ctx id |> Response.not_found_on_error in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values experiment_boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let detail_path =
      Format.asprintf "/admin/experiments/%s" (id |> Experiment.Id.value)
    in
    Response.bad_request_on_error ~urlencoded edit
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* organisational_unit =
      organisational_unit_from_urlencoded urlencoded db_ctx
    in
    let* smtp_auth = smtp_auth_from_urlencoded urlencoded db_ctx in
    let%lwt session_count = Experiment.session_count db_ctx id in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Experiment_command.Update in
      urlencoded
      |> decode
      >>= handle ~tags ~session_count experiment organisational_unit smtp_auth
      |> Lwt_result.lift
    in
    let handle db_ctx events =
      let%lwt () = Pool_event.handle_events ~tags db_ctx user events in
      Http_utils.redirect_to_with_actions
        detail_path
        [ Message.set ~success:[ Success.Updated Field.Experiment ] ]
    in
    events |>> handle db_ctx
  in
  result |> Response.handle ~src req
;;

let delete req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.user; _ } as context) =
    let experiment_id = experiment_id req in
    Pool_context.connection context @@ fun db_ctx ->
    let* experiment =
      Experiment.find db_ctx experiment_id |> Response.not_found_on_error
    in
    let experiments_path = "/admin/experiments" in
    Response.bad_request_on_error show
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt invitation_count = Experiment.invitation_count db_ctx experiment_id in
    let%lwt session_count = Experiment.session_count db_ctx experiment_id in
    let%lwt mailings = Mailing.find_by_experiment db_ctx experiment_id in
    let%lwt assistants =
      Admin.find_all_with_role
        db_ctx
        (`Assistant, Some (Guard.Uuid.target_of Experiment.Id.value experiment_id))
    in
    let%lwt experimenters =
      Admin.find_all_with_role
        db_ctx
        (`Experimenter, Some (Guard.Uuid.target_of Experiment.Id.value experiment_id))
    in
    let%lwt templates =
      let open Pool_common.MessageTemplateLabel in
      let open Message_template in
      [ ExperimentInvitation; SessionReminder; AssignmentConfirmation ]
      |> Lwt_list.map_s
           (find_all_of_entity_by_label
              db_ctx
              (experiment_id |> Experiment.Id.to_common))
      ||> CCList.flatten
    in
    let events =
      let open Cqrs_command.Experiment_command.Delete in
      handle
        ~tags
        { experiment
        ; session_count
        ; invitation_count
        ; mailings
        ; experimenters
        ; assistants
        ; templates
        }
      |> Lwt_result.lift
    in
    let handle db_ctx events =
      let%lwt () = Pool_event.handle_events ~tags db_ctx user events in
      Http_utils.redirect_to_with_actions
        experiments_path
        [ Message.set ~success:[ Success.Created Field.Experiment ] ]
    in
    events |>> handle db_ctx
  in
  result |> Response.handle ~src req
;;

let reset_invitations req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id = experiment_id req in
  let redirect_path = HttpUtils.Url.Admin.experiment_path ~id:experiment_id () in
  let result ({ Pool_context.user; _ } as context) =
    Pool_context.connection context @@ fun db_ctx ->
    let* experiment =
      Experiment.find db_ctx experiment_id |> Response.not_found_on_error
    in
    Response.bad_request_on_error show
    @@
    let events =
      let open Cqrs_command.Experiment_command.ResetInvitations in
      handle ~tags experiment |> Lwt.return
    in
    let handle db_ctx events =
      let%lwt () = Pool_event.handle_events ~tags db_ctx user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Success.ResetInvitations ] ]
    in
    events |>> handle db_ctx 
  in
  Response.handle ~src req result
;;

let search = Helpers.Search.htmx_search_helper `Experiment

module Filter = struct
  open HttpUtils.Filter
  open Utils.Lwt_result.Infix

  let handler fnc req =
    let id = experiment_id req in
    database_connection_from_req req @@
    CCFun.flip Experiment.find id
    |>> (fun e -> fnc (Experiment e) req)
    >|> function
    | Ok res -> Lwt.return res
    | Error err ->
      let path =
        Format.asprintf "/admin/experiments/%s/invitations" (Experiment.Id.value id)
      in
      Http_utils.redirect_to_with_actions path [ Message.set ~error:[ err ] ]
  ;;

  let toggle_predicate_type = handler Admin_filter.handle_toggle_predicate_type
  let add_predicate = handler Admin_filter.handle_add_predicate
  let toggle_key = handler Admin_filter.handle_toggle_key
  let create = handler Admin_filter.write
  let update = handler Admin_filter.write

  let delete req =
    let result ({ Pool_context.user; _ } as context) =
      let experiment_id =
        HttpUtils.find_id Experiment.Id.of_string Field.Experiment req
      in
      let redirect_path =
        HttpUtils.Url.Admin.experiment_path ~id:experiment_id ~suffix:"invitations" ()
      in
      let tags = Pool_context.Logger.Tags.req req in
      Pool_context.connection context @@ fun db_ctx ->
      let* experiment = Experiment.find db_ctx experiment_id in
      let events =
        let open Cqrs_command.Experiment_command.DeleteFilter in
        handle ~tags experiment |> Lwt_result.lift
      in
      let handle db_ctx events =
        let%lwt () = Pool_event.handle_events ~tags db_ctx user events in
        Http_utils.redirect_to_with_actions
          redirect_path
          [ Message.set ~success:[ Success.Deleted Field.Filter ] ]
      in
      events |>> handle db_ctx
    in
    Response.Htmx.handle ~src req result
  ;;
end

let message_history req =
  let queue_table = `History in
  let experiment_id = experiment_id req in
  Response.Htmx.index_handler
    ~query:(module Pool_queue)
    ~create_layout:General.create_tenant_layout
    req
  @@ fun context query ->
  let open Utils.Lwt_result.Infix in
  Pool_context.connection context @@ fun db_ctx ->
  let* experiment = Experiment.find db_ctx experiment_id in
  let%lwt messages =
    let open Pool_queue in
    find_instances_by_entity
      queue_table
      ~query
      db_ctx
      (History.Experiment, Experiment.Id.to_common experiment_id)
  in
  let open Page.Admin in
  Lwt_result.ok
  @@
  if HttpUtils.Htmx.is_hx_request req
  then
    Queue.list
      context
      queue_table
      (HttpUtils.Url.Admin.experiment_message_history_url experiment_id |> Uri.of_string)
      messages
    |> Lwt.return
  else Experiments.message_history context queue_table experiment messages
;;

module Tags = struct
  let handle action req =
    let experiment_id = experiment_id req in
    let redirect = experiment_path ~id:experiment_id () in
    Admin_experiments_tags.handle_tag action redirect edit req
  ;;

  let assign_tag = handle `Assign
  let remove_tag = handle `Remove
  let assign_experiment_participation_tag = handle `AssignExperimentParticipationTag
  let remove_experiment_participation_tag = handle `RemoveExperimentParticipationTag
end

module Access : sig
  include module type of Helpers.Access
  module Filter : module type of Helpers.Access

  val search : Rock.Middleware.t
  val message_history : Rock.Middleware.t
end = struct
  module ExperimentCommand = Cqrs_command.Experiment_command
  module Guardian = Middleware.Guardian

  let experiment_effects = Guardian.id_effects Experiment.Id.validate Field.Experiment
  let index = Experiment.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  let create = ExperimentCommand.Create.effects |> Guardian.validate_admin_entity
  let read = experiment_effects Experiment.Guard.Access.read
  let update = experiment_effects ExperimentCommand.Update.effects
  let delete = experiment_effects ExperimentCommand.Delete.effects

  module Filter = struct
    include Helpers.Access

    let combined_effects validation_set =
      let open CCResult.Infix in
      let find = HttpUtils.find_id in
      Guardian.validate_generic
      @@ fun req ->
      let* filter_id = find FilterEntity.Id.validate Field.Filter req in
      let* id = find Experiment.Id.validate Field.Experiment req in
      validation_set id filter_id |> CCResult.return
    ;;

    let create = experiment_effects ExperimentCommand.CreateFilter.effects
    let update = combined_effects ExperimentCommand.UpdateFilter.effects
    let delete = combined_effects ExperimentCommand.DeleteFilter.effects
  end

  let search = index

  let message_history =
    experiment_effects (fun id ->
      Pool_queue.Guard.Access.index ~id:(Guard.Uuid.target_of Experiment.Id.value id) ())
  ;;
end
