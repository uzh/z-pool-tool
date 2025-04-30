open Pool_message
module HttpUtils = Http_utils
module HttpMessage = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.experiments_invitations"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let index req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* experiment = id |> Experiment.find database_label >|- Response.not_found in
    Response.bad_request_render_error context
    @@
    let common_exp_id = Experiment.(experiment |> id |> Id.to_common) in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list = Filter.find_all_templates database_label () in
    let%lwt query_experiments, query_tags =
      match experiment |> Experiment.filter with
      | None -> Lwt.return ([], [])
      | Some filter ->
        Lwt.both
          (filter
           |> Filter.all_query_experiments
           |> Experiment.search_multiple_by_id database_label)
          (filter |> Filter.all_query_tags |> Tags.find_multiple database_label)
    in
    let* filtered_contacts =
      if Sihl.Configuration.is_production ()
      then Lwt_result.return None
      else
        Filter.(
          find_filtered_contacts
            ~limit:50
            database_label
            (Matcher common_exp_id)
            (experiment |> Experiment.filter))
        >|+ CCOption.pure
    in
    let* statistics =
      let query =
        experiment.Experiment.filter |> CCOption.map (fun f -> f.Filter.query)
      in
      Statistics.ExperimentFilter.create database_label experiment query
    in
    Page.Admin.Experiments.invitations
      experiment
      key_list
      template_list
      query_experiments
      query_tags
      statistics
      filtered_contacts
      context
    >|> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src ~enable_cache:false req result
;;

let sent_invitations req =
  let id = experiment_id req in
  Response.Htmx.index_handler ~create_layout ~query:(module Invitation) req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* experiment = Experiment.find database_label id in
  let experiment_guard = [ Guard.Uuid.target_of Experiment.Id.value id ] in
  let%lwt invitations =
    Invitation.find_by_experiment ~query database_label experiment.Experiment.id
  in
  let open Helpers_guard in
  let view_contact_name = can_read_contact_name context experiment_guard in
  let view_contact_info = can_read_contact_info context experiment_guard in
  let access_contact_profiles = can_access_contact_profile context id in
  let open Page.Admin.Invitations in
  match HttpUtils.Htmx.is_hx_request req with
  | true ->
    Partials.list
      ~view_contact_name
      ~view_contact_info
      ~access_contact_profiles
      context
      experiment
      invitations
    |> Lwt_result.return
  | false ->
    let* statistics = Statistics.ExperimentInvitations.create database_label experiment in
    sent_invitations
      ~access_contact_profiles
      ~view_contact_name
      ~view_contact_info
      context
      experiment
      invitations
      statistics
    |> Lwt_result.ok
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let redirect_path =
    Format.asprintf "/admin/experiments/%s/invitations" (Experiment.Id.value id)
  in
  let result { Pool_context.database_label; user; _ } =
    let* experiment = Experiment.find database_label id |> Response.not_found_on_error in
    Response.bad_request_on_error index
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* contact_ids =
      Sihl.Web.Request.urlencoded_list Field.(Contacts |> array_key) req
      ||> CCList.map Contact.Id.of_string
      ||> fun list ->
      if CCList.is_empty list
      then Error (Error.NoOptionSelected Field.Contact)
      else Ok list
    in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* contacts =
      let find_missing contacts =
        let retrieved_ids = CCList.map Contact.id contacts in
        CCList.fold_left
          (fun missing id ->
             match CCList.mem ~eq:Contact.Id.equal id retrieved_ids with
             | true -> missing
             | false -> CCList.cons id missing)
          []
          contact_ids
      in
      let%lwt contacts = Contact.find_multiple database_label contact_ids in
      Lwt_result.lift
      @@
      match CCList.length contact_ids == CCList.length contacts with
      | true -> Ok contacts
      | false ->
        find_missing contacts
        |> CCList.map Contact.Id.value
        |> fun ids -> Error (Error.NotFoundList (Field.Contacts, ids))
    in
    let%lwt invited_contacts =
      Invitation.find_multiple_by_experiment_and_contacts
        database_label
        (CCList.map Contact.id contacts)
        experiment
    in
    let%lwt create_message =
      Message_template.ExperimentInvitation.prepare tenant experiment
    in
    let%lwt events =
      let open Cqrs_command.Invitation_command.Create in
      handle
        ~tags
        { experiment; contacts; invited_contacts; create_message; mailing = None }
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpMessage.set ~success:[ Success.SentList Field.Invitations ] ]
    in
    events |> Lwt_result.lift |>> handle
  in
  Response.handle ~src req result
;;

let resend req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id, id =
    experiment_id req, HttpUtils.find_id Pool_common.Id.of_string Field.Invitation req
  in
  let redirect_path =
    HttpUtils.Url.Admin.experiment_path ~id:experiment_id ~suffix:"invitations" ()
  in
  let result { Pool_context.database_label; user; _ } =
    let* invitation = Invitation.find database_label id |> Response.not_found_on_error in
    let* experiment =
      Experiment.find database_label experiment_id |> Response.not_found_on_error
    in
    Response.bad_request_on_error index
    @@
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let%lwt create_email =
      Message_template.ExperimentInvitation.prepare tenant experiment
    in
    let events =
      let open Cqrs_command.Invitation_command.Resend in
      handle ~tags create_email invitation |> Lwt.return
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpMessage.set ~success:[ Success.SentList Field.Invitations ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

module Access : sig
  include module type of Helpers.Access

  val resend : Rock.Middleware.t
end = struct
  include Helpers.Access
  module InvitationCommand = Cqrs_command.Invitation_command
  module Guardian = Middleware.Guardian

  let experiment_effects = Guardian.id_effects Experiment.Id.validate Field.Experiment

  let combined_effects validation_set =
    let open CCResult.Infix in
    let find = HttpUtils.find_id in
    Guardian.validate_generic
    @@ fun req ->
    let* experiment_id = find Experiment.Id.validate Field.Experiment req in
    let* invitation_id = find Pool_common.Id.validate Field.Invitation req in
    validation_set experiment_id invitation_id |> CCResult.return
  ;;

  let index = experiment_effects Invitation.Guard.Access.index
  let create = experiment_effects InvitationCommand.Create.effects
  let read = combined_effects Invitation.Guard.Access.read
  let resend = combined_effects InvitationCommand.Resend.effects
end
