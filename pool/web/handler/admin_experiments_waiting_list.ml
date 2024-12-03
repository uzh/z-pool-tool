module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_message.Field

let src = Logs.Src.create "handler.admin.experiments_waiting_list"
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let index req =
  let id = experiment_id req in
  HttpUtils.Htmx.handler
    ~error_path:(Format.asprintf "/admin/experiments/%s" (Experiment.Id.value id))
    ~create_layout
    ~query:(module Waiting_list)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* experiment = Experiment.find database_label id in
  let access_contact_profiles = Helpers.Guard.can_access_contact_profile context id in
  let%lwt waiting_list =
    Waiting_list.find_by_experiment ~query database_label experiment.Experiment.id
  in
  let open Page.Admin.WaitingList in
  (if HttpUtils.Htmx.is_hx_request req
   then list ~access_contact_profiles context experiment waiting_list |> Lwt.return
   else index ~access_contact_profiles context experiment waiting_list)
  |> Lwt_result.ok
;;

let detail req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    experiment_id req, HttpUtils.find_id Pool_common.Id.of_string Field.WaitingList req
  in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/waiting-list"
      (Experiment.Id.value experiment_id)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* waiting_list = Waiting_list.find database_label id in
       let%lwt sessions =
         Session.find_all_to_assign_from_waitinglist database_label experiment_id
       in
       let grouped_sessions, chronological =
         let open Session in
         let sessions = group_and_sort sessions in
         let sort_sessions (s1 : t) (s2 : t) = Start.compare s1.start s2.start in
         match Sihl.Web.Request.query Pool_message.Field.(show Chronological) req with
         | Some "true" ->
           let open CCList in
           ( sessions
             |> flat_map (fun (parent, follow_ups) -> parent :: follow_ups)
             |> sort sort_sessions
             |> map (fun s -> s, [])
           , true )
         | None | Some _ -> sessions, false
       in
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       Page.Admin.WaitingList.detail
         waiting_list
         grouped_sessions
         experiment_id
         context
         flash_fetcher
         chronological
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, waiting_list_id =
    experiment_id req, HttpUtils.find_id Pool_common.Id.of_string Field.WaitingList req
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/waiting-list/%s"
      (Experiment.Id.value experiment_id)
      (Pool_common.Id.value waiting_list_id)
  in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* waiting_list = Waiting_list.find database_label waiting_list_id in
    let events =
      let open Cqrs_command.Waiting_list_command in
      let open CCResult in
      urlencoded |> Update.decode >>= Update.handle ~tags waiting_list |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_message.(Success.Updated Field.WaitingList) ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let assign_contact req =
  let open Session in
  let open Utils.Lwt_result.Infix in
  let experiment_id, waiting_list_id =
    experiment_id req, HttpUtils.find_id Pool_common.Id.of_string Field.WaitingList req
  in
  let redirect_path =
    let open Experiment.Id in
    Format.asprintf "/admin/experiments/%s/waiting-list" (experiment_id |> value)
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, Format.asprintf "%s/%s" redirect_path (Pool_common.Id.value waiting_list_id))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* experiment = Experiment.find database_label experiment_id in
    let* waiting_list = Waiting_list.find database_label waiting_list_id in
    let* session =
      let open Pool_message in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> CCList.assoc_opt ~eq:CCString.equal Field.(show Session)
      |> CCFun.flip CCOption.bind CCList.head_opt
      |> CCOption.to_result Error.NoValue
      |> Lwt_result.lift
      >>= fun id -> id |> Session.Id.of_string |> find_open database_label
    in
    let%lwt follow_up_sessions =
      Session.find_follow_ups database_label session.Session.id
    in
    let%lwt already_enrolled =
      let open Utils.Lwt_result.Infix in
      Assignment.Public.find_all_by_experiment
        database_label
        experiment_id
        waiting_list.Waiting_list.contact
      ||> CCList.is_empty
      ||> not
    in
    let%lwt confirmation_email =
      let contact = waiting_list.Waiting_list.contact in
      Message_template.AssignmentConfirmation.prepare
        ~follow_up_sessions
        tenant
        contact
        experiment
        session
    in
    let events =
      let open Cqrs_command.Assignment_command.CreateFromWaitingList in
      (handle ~tags { session; follow_up_sessions; waiting_list; already_enrolled })
        confirmation_email
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set ~success:[ Pool_message.Success.AssignmentCreated ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : sig
  include module type of Helpers.Access

  val assign : Rock.Middleware.t
end = struct
  include Helpers.Access
  module WaitingListCommand = Cqrs_command.Waiting_list_command
  module Guardian = Middleware.Guardian

  let experiment_effects = Guardian.id_effects Experiment.Id.validate Field.Experiment

  let combined_effects validation_set =
    let open CCResult.Infix in
    let find = HttpUtils.find_id in
    Guardian.validate_generic
    @@ fun req ->
    let* experiment_id = find Experiment.Id.validate Field.Experiment req in
    let* id = find Pool_common.Id.validate Field.WaitingList req in
    validation_set experiment_id id |> CCResult.return
  ;;

  let index = experiment_effects Waiting_list.Guard.Access.index
  let create = experiment_effects WaitingListCommand.Create.effects
  let read = combined_effects Waiting_list.Guard.Access.read
  let update = combined_effects WaitingListCommand.Update.effects
  let delete = combined_effects WaitingListCommand.Destroy.effects

  let assign =
    combined_effects Cqrs_command.Assignment_command.CreateFromWaitingList.effects
  ;;
end
