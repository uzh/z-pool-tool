open CCFun
module Assignment = Admin_experiments_assignments
module Field = Pool_common.Message.Field
module FilterEntity = Filter
module HttpUtils = Http_utils
module Invitations = Admin_experiments_invitations
module Mailings = Admin_experiments_mailing
module Message = HttpUtils.Message
module MessageTemplates = Admin_experiments_message_templates
module Users = Admin_experiments_users
module WaitingList = Admin_experiments_waiting_list

let src = Logs.Src.create "handler.admin.experiments"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let find_entity_in_urlencoded urlencoded field fnc =
  let open Utils.Lwt_result.Infix in
  urlencoded
  |> HttpUtils.find_in_urlencoded_opt field
  |> function
  | None -> Lwt_result.return None
  | Some id -> id |> fnc >|+ CCOption.return
;;

let experiment_boolean_fields =
  Experiment.boolean_fields |> CCList.map Field.show
;;

let contact_person_roles experiment_id =
  let base = [ `RecruiterAll ] in
  match experiment_id with
  | None -> base
  | Some id ->
    id
    |> Guard.Uuid.target_of Experiment.Id.value
    |> fun target_id -> [ `Experimenter target_id; `Recruiter target_id ] @ base
;;

let validation_set =
  let open Guard in
  let open ValidationSet in
  let base = [ SpecificRole `RecruiterAll ] in
  CCOption.map_or ~default:base (fun exp_id ->
    let id = Uuid.target_of Experiment.Id.value exp_id in
    base
    @ [ SpecificRole (`Recruiter id)
      ; SpecificRole (`Experimenter id)
      ; SpecificRole (`Recruiter id)
      ])
  %> or_
;;

let contact_person_from_urlencoded database_label urlencoded experiment_id =
  let open Utils.Lwt_result.Infix in
  let query id =
    let* () =
      id
      |> Guard.Uuid.Actor.of_string
      |> CCOption.to_result Pool_common.Message.(NotFound Field.ContactPerson)
      |> Lwt_result.lift
      >>= (fun id ->
            Guard.Persistence.Actor.find database_label `Admin id
            >|- Pool_common.Message.authorization)
      >>= Guard.Persistence.validate
            database_label
            (validation_set experiment_id)
    in
    id |> Admin.Id.of_string |> Admin.find database_label
  in
  find_entity_in_urlencoded urlencoded Field.ContactPerson query
;;

let organisational_unit_from_urlencoded urlencoded database_label =
  let query id =
    Organisational_unit.(id |> Id.of_string |> find database_label)
  in
  find_entity_in_urlencoded urlencoded Field.OrganisationalUnit query
;;

let smtp_auth_from_urlencoded urlencoded database_label =
  let query auth_id =
    Email.SmtpAuth.(auth_id |> Id.of_string |> find database_label)
  in
  find_entity_in_urlencoded urlencoded Field.Smtp query
;;

let experiment_message_templates database_label experiment_id =
  let open Message_template in
  let open Utils.Lwt_result.Infix in
  let find_templates label =
    find_all_of_entity_by_label
      database_label
      (experiment_id |> Experiment.Id.to_common)
      label
    ||> CCPair.make label
  in
  Label.[ ExperimentInvitation; SessionReminder; AssignmentConfirmation ]
  |> Lwt_list.map_s find_templates
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let find_actor =
      Pool_context.Utils.find_authorizable ~admin_only:true database_label user
    in
    let find_experiments actor =
      let open Experiment in
      let query =
        Query.from_request
          ~searchable_by
          ~sortable_by
          ~default:Experiment.default_query
          req
      in
      find_all ~query ~actor ~action:Guard.Access.index_action database_label
    in
    find_actor
    |>> find_experiments
    >|+ Page.Admin.Experiments.index context
    >>= create_layout ~active_navigation:"/admin/experiments" req context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, error_path
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/experiments" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let%lwt default_email_reminder_lead_time =
      Settings.find_default_reminder_lead_time database_label
    in
    let%lwt default_text_msg_reminder_lead_time =
      Settings.find_default_text_msg_reminder_lead_time database_label
    in
    let%lwt organisational_units = Organisational_unit.all database_label () in
    let%lwt contact_persons =
      contact_person_roles None |> Admin.find_all_with_roles database_label
    in
    let%lwt smtp_auth_list = Email.SmtpAuth.find_all database_label in
    Page.Admin.Experiments.create
      context
      organisational_units
      default_email_reminder_lead_time
      default_text_msg_reminder_lead_time
      contact_persons
      smtp_auth_list
      flash_fetcher
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values experiment_boolean_fields
    ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , "/admin/experiments/create"
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* contact_person =
      contact_person_from_urlencoded database_label urlencoded None
    in
    let* organisational_unit =
      organisational_unit_from_urlencoded urlencoded database_label
    in
    let* smtp_auth = smtp_auth_from_urlencoded urlencoded database_label in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Experiment_command.Create in
      urlencoded
      |> decode
      >>= handle ~tags contact_person organisational_unit smtp_auth
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        "/admin/experiments"
        [ Message.set
            ~success:[ Pool_common.Message.(Created Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/experiments")
    @@
    let* actor = Pool_context.Utils.find_authorizable database_label user in
    let id = experiment_id req in
    let* experiment = Experiment.find database_label id in
    let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let%lwt message_templates =
      experiment_message_templates database_label id
    in
    let%lwt current_tags =
      Tags.(find_all_of_entity database_label Model.Experiment)
        (id |> Experiment.Id.to_common)
    in
    let%lwt current_participation_tags =
      Tags.(
        ParticipationTags.find_all
          database_label
          (ParticipationTags.Experiment (Experiment.Id.to_common id)))
    in
    (match edit with
     | false ->
       let%lwt session_count = Experiment.session_count database_label id in
       let* contact_person =
         experiment.Experiment.contact_person_id
         |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
           Admin.find database_label id >|+ CCOption.return)
       in
       let* smtp_auth =
         experiment.Experiment.smtp_auth_id
         |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
           Email.SmtpAuth.find database_label id >|+ CCOption.return)
       in
       Page.Admin.Experiments.detail
         experiment
         session_count
         message_templates
         sys_languages
         contact_person
         smtp_auth
         current_tags
         current_participation_tags
         context
       |> Lwt_result.ok
     | true ->
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let%lwt default_email_reminder_lead_time =
         Settings.find_default_reminder_lead_time database_label
       in
       let%lwt default_text_msg_reminder_lead_time =
         Settings.find_default_text_msg_reminder_lead_time database_label
       in
       let%lwt organisational_units =
         Organisational_unit.all database_label ()
       in
       let%lwt smtp_auth_list = Email.SmtpAuth.find_all database_label in
       let%lwt contact_persons =
         Some id
         |> contact_person_roles
         |> Admin.find_all_with_roles database_label
       in
       let%lwt allowed_to_assign =
         Guard.Persistence.validate
           database_label
           (id
            |> Experiment.Id.to_common
            |> Cqrs_command.Tags_command.AssignTagToContact.effects)
           actor
         ||> CCResult.is_ok
       in
       let find_tags model =
         let open Tags in
         if allowed_to_assign
         then find_all_validated_with_model database_label model actor
         else Lwt.return []
       in
       let%lwt experiment_tags = find_tags Tags.Model.Experiment in
       let%lwt participation_tags = find_tags Tags.Model.Contact in
       Page.Admin.Experiments.edit
         ~allowed_to_assign
         experiment
         context
         sys_languages
         default_email_reminder_lead_time
         default_text_msg_reminder_lead_time
         contact_persons
         organisational_units
         smtp_auth_list
         message_templates
         (experiment_tags, current_tags)
         (participation_tags, current_participation_tags)
         flash_fetcher
       |> Lwt_result.ok)
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let show = detail false
let edit = detail true

let update req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.database_label; _ } =
    let id = experiment_id req in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values experiment_boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let detail_path =
      Format.asprintf "/admin/experiments/%s" (id |> Experiment.Id.value)
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/edit" detail_path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label id in
    let* organisational_unit =
      organisational_unit_from_urlencoded urlencoded database_label
    in
    let* contact_person =
      contact_person_from_urlencoded database_label urlencoded (Some id)
    in
    let* smtp_auth = smtp_auth_from_urlencoded urlencoded database_label in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Experiment_command.Update in
      urlencoded
      |> decode
      >>= handle ~tags experiment contact_person organisational_unit smtp_auth
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        detail_path
        [ Message.set
            ~success:[ Pool_common.Message.(Updated Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let delete req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.database_label; _ } =
    let experiment_id = experiment_id req in
    let experiments_path = "/admin/experiments" in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf
          "%s/%s"
          experiments_path
          (Experiment.Id.value experiment_id) ))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label experiment_id in
    let%lwt session_count =
      Experiment.session_count database_label experiment_id
    in
    let%lwt mailings =
      Mailing.find_by_experiment database_label experiment_id
    in
    let%lwt assistants =
      Admin.find_all_with_role
        database_label
        (`Assistant (Guard.Uuid.target_of Experiment.Id.value experiment_id))
    in
    let%lwt experimenters =
      Admin.find_all_with_role
        database_label
        (`Experimenter (Guard.Uuid.target_of Experiment.Id.value experiment_id))
    in
    let%lwt templates =
      let open Message_template in
      Label.[ ExperimentInvitation; SessionReminder; AssignmentConfirmation ]
      |> Lwt_list.map_s
           (find_all_of_entity_by_label
              database_label
              (experiment_id |> Experiment.Id.to_common))
      ||> CCList.flatten
    in
    let events =
      let open Cqrs_command.Experiment_command.Delete in
      handle
        ~tags
        { experiment
        ; session_count
        ; mailings
        ; experimenters
        ; assistants
        ; templates
        }
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        experiments_path
        [ Message.set
            ~success:[ Pool_common.Message.(Created Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let search = Helpers.Search.create `Experiment

module Filter = struct
  open HttpUtils.Filter
  open Utils.Lwt_result.Infix

  let handler fnc req =
    let id = experiment_id req in
    req
    |> database_label_from_req
    >>= CCFun.flip Experiment.find id
    |>> (fun e -> fnc (Experiment e) req)
    >|> function
    | Ok res -> Lwt.return res
    | Error err ->
      let path =
        Format.asprintf
          "/admin/experiments/%s/invitations"
          (Experiment.Id.value id)
      in
      Http_utils.redirect_to_with_actions path [ Message.set ~error:[ err ] ]
  ;;

  let toggle_predicate_type = handler Admin_filter.handle_toggle_predicate_type
  let add_predicate = handler Admin_filter.handle_add_predicate
  let toggle_key = handler Admin_filter.handle_toggle_key
  let create = handler Admin_filter.write
  let update = handler Admin_filter.write

  let delete req =
    let result { Pool_context.database_label; _ } =
      let experiment_id =
        HttpUtils.find_id Experiment.Id.of_string Field.Experiment req
      in
      let redirect_path =
        Format.asprintf
          "/admin/experiments/%s/invitations"
          (Experiment.Id.value experiment_id)
      in
      Utils.Lwt_result.map_error (fun err -> err, redirect_path)
      @@
      let tags = Pool_context.Logger.Tags.req req in
      let* experiment = Experiment.find database_label experiment_id in
      let events =
        let open Cqrs_command.Experiment_command.DeleteFilter in
        handle ~tags experiment |> Lwt_result.lift
      in
      let handle events =
        let%lwt () =
          Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
        in
        Http_utils.redirect_to_with_actions
          redirect_path
          [ Message.set ~success:[ Pool_common.Message.(Deleted Field.Filter) ]
          ]
      in
      events |>> handle
    in
    result |> HttpUtils.extract_happy_path ~src req
  ;;
end

module Tags = Admin_experiments_tags

module Access : sig
  include module type of Helpers.Access
  module Filter : module type of Helpers.Access

  val search : Rock.Middleware.t
end = struct
  module Field = Pool_common.Message.Field
  module ExperimentCommand = Cqrs_command.Experiment_command
  module Guardian = Middleware.Guardian

  let experiment_effects =
    Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let index =
    Experiment.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create =
    ExperimentCommand.Create.effects |> Guardian.validate_admin_entity
  ;;

  let read =
    Experiment.Guard.Access.read
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let update =
    ExperimentCommand.Update.effects
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let delete =
    ExperimentCommand.Delete.effects
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  module Filter = struct
    include Helpers.Access

    let combined_effects effects req =
      let open HttpUtils in
      let filter_id = find_id FilterEntity.Id.of_string Field.Filter req in
      let id = find_id Experiment.Id.of_string Field.Experiment req in
      effects id filter_id
    ;;

    let create =
      ExperimentCommand.CreateFilter.effects
      |> experiment_effects
      |> Guardian.validate_generic
    ;;

    let update =
      ExperimentCommand.UpdateFilter.effects
      |> combined_effects
      |> Guardian.validate_generic
    ;;

    let delete =
      ExperimentCommand.DeleteFilter.effects
      |> combined_effects
      |> Guardian.validate_generic
    ;;
  end

  let search = index
end
