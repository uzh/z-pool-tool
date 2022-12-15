module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Invitations = Admin_experiments_invitations
module WaitingList = Admin_experiments_waiting_list
module Assignment = Admin_experiments_assignments
module Mailings = Admin_experiments_mailing
module Users = Admin_experiments_users

let create_layout req = General.create_tenant_layout req

let experiment_id =
  HttpUtils.find_id Experiment.Id.of_string Pool_common.Message.Field.Experiment
;;

let experiment_boolean_fields =
  Experiment.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let%lwt expermient_list = Experiment.find_all database_label () in
       Page.Admin.Experiments.index expermient_list context
       |> create_layout ~active_navigation:"/admin/experiments" req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/experiments" in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* sys_languages =
      Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
    in
    Page.Admin.Experiments.create context sys_languages flash_fetcher
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
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
    let tags = Logger.req req in
    let events =
      let open CCResult.Infix in
      urlencoded
      |> Cqrs_command.Experiment_command.Create.decode
      >>= Cqrs_command.Experiment_command.Create.handle ~tags
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
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/experiments")
    @@
    let id = experiment_id req in
    let* experiment = Experiment.find database_label id in
    (match edit with
     | false ->
       let%lwt session_count = Experiment.session_count database_label id in
       Page.Admin.Experiments.detail experiment session_count context
       |> Lwt.return_ok
     | true ->
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let* sys_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       Page.Admin.Experiments.edit
         experiment
         context
         sys_languages
         flash_fetcher
       |> Lwt.return_ok)
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
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
    @@ let* experiment = Experiment.find database_label id in
       let tags = Logger.req req in
       let events =
         let open CCResult.Infix in
         let open Cqrs_command.Experiment_command.Update in
         urlencoded |> decode >>= handle ~tags experiment |> Lwt_result.lift
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
  result |> HttpUtils.extract_happy_path_with_actions req
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
    @@ let%lwt session_count =
         Experiment.session_count database_label experiment_id
       in
       let tags = Logger.req req in
       let events =
         Cqrs_command.Experiment_command.Delete.(
           handle ~tags { experiment_id; session_count })
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
  result |> HttpUtils.extract_happy_path req
;;

module Filter = struct
  let toggle_predicate_type req =
    let id = experiment_id req in
    Admin_filter.handle_toggle_predicate_type ~experiment_id:id req
  ;;

  let add_predicate req =
    let id = experiment_id req in
    Admin_filter.handle_add_predicate ~experiment_id:id req
  ;;

  let toggle_key = Admin_filter.toggle_key
end

module Access : sig
  include Helpers.AccessSig
end = struct
  module Field = Pool_common.Message.Field
  module ExperimentCommand = Cqrs_command.Experiment_command

  let experiment_effects =
    Middleware.Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity
      [ `Read, `TargetEntity `Experiment ]
  ;;

  let create =
    ExperimentCommand.Create.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Experiment.Id.value)
        ; `Read, `TargetEntity `Experiment
        ])
    ]
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    [ ExperimentCommand.Update.effects ]
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let delete =
    [ ExperimentCommand.Delete.effects ]
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
