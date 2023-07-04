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

let organisational_unit urlencoded database_label =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  urlencoded
  |> CCList.assoc_opt ~eq:CCString.equal Field.(OrganisationalUnit |> show)
  |> function
  | Some [ id ] ->
    let open Organisational_unit in
    id |> Id.of_string |> find database_label >|+ CCOption.return
  | None | Some _ -> Lwt_result.return None
;;

let experiment_boolean_fields =
  Experiment.boolean_fields |> CCList.map Field.show
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
      let query = Query.from_request ~searchable_by ~sortable_by req in
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
    let%lwt default_reminder_lead_time =
      Settings.find_default_reminder_lead_time database_label
    in
    let%lwt organisational_units = Organisational_unit.all database_label () in
    Page.Admin.Experiments.create
      context
      organisational_units
      default_reminder_lead_time
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
    let* organisational_unit = organisational_unit urlencoded database_label in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Experiment_command.Create in
      urlencoded
      |> decode
      >>= handle ~tags organisational_unit
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
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/experiments")
    @@
    let open Message_template in
    let id = experiment_id req in
    let* experiment = Experiment.find database_label id in
    let sys_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let find_templates =
      find_all_of_entity_by_label database_label (id |> Experiment.Id.to_common)
    in
    let%lwt invitation_templates = find_templates Label.ExperimentInvitation in
    let%lwt session_reminder_templates = find_templates Label.SessionReminder in
    (match edit with
     | false ->
       let%lwt session_count = Experiment.session_count database_label id in
       Page.Admin.Experiments.detail
         experiment
         session_count
         invitation_templates
         session_reminder_templates
         sys_languages
         context
       |> Lwt_result.ok
     | true ->
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let%lwt default_reminder_lead_time =
         Settings.find_default_reminder_lead_time database_label
       in
       let%lwt organisational_units =
         Organisational_unit.all database_label ()
       in
       Page.Admin.Experiments.edit
         experiment
         context
         sys_languages
         default_reminder_lead_time
         invitation_templates
         organisational_units
         session_reminder_templates
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
    let* organisational_unit = organisational_unit urlencoded database_label in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Experiment_command.Update in
      urlencoded
      |> decode
      >>= handle ~tags experiment organisational_unit
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
      Label.[ ExperimentInvitation; SessionReminder ]
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
