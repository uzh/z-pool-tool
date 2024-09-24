open Pool_message
module HttpUtils = Http_utils
module HttpMessage = HttpUtils.Message

let src = Logs.Src.create "handler.admin.experiments_invitations"
let extract_happy_path = HttpUtils.extract_happy_path ~src
let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let index req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Experiment.Id.value id)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = id |> Experiment.find database_label in
       let open Experiment in
       let common_exp_id = experiment |> id |> Id.to_common in
       let%lwt key_list = Filter.all_keys database_label in
       let%lwt template_list = Filter.find_all_templates database_label () in
       let%lwt query_experiments, query_tags =
         match experiment |> filter with
         | None -> Lwt.return ([], [])
         | Some filter ->
           Lwt.both
             (filter
              |> Filter.all_query_experiments
              |> search_multiple_by_id database_label)
             (filter
              |> Filter.all_query_tags
              |> Tags.find_multiple database_label)
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
               (experiment |> filter))
           >|+ CCOption.pure
       in
       let* matching_filter_count =
         let open Filter in
         count_filtered_contacts
           database_label
           (Matcher common_exp_id)
           (experiment
            |> filter
            |> CCOption.map (fun { Filter.query; _ } -> query))
       in
       let%lwt invitation_count =
         experiment |> id |> Experiment.invitation_count database_label
       in
       Page.Admin.Experiments.invitations
         experiment
         key_list
         template_list
         query_experiments
         query_tags
         matching_filter_count
         invitation_count
         filtered_contacts
         context
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let sent_invitations req =
  let id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s/invitations" (Experiment.Id.value id)
  in
  HttpUtils.Htmx.handler
    ~error_path
    ~create_layout
    ~query:(module Invitation)
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let open Utils.Lwt_result.Infix in
  let* experiment = Experiment.find database_label id in
  let%lwt invitations =
    Invitation.find_by_experiment ~query database_label experiment.Experiment.id
  in
  let open Page.Admin.Invitations in
  match HttpUtils.Htmx.is_hx_request req with
  | true -> Partials.list context experiment invitations |> Lwt_result.return
  | false ->
    let* statistics =
      Experiment.Statistics.SentInvitations.create database_label experiment
    in
    sent_invitations context experiment invitations statistics |> Lwt_result.ok
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let redirect_path =
    Format.asprintf "/admin/experiments/%s/invitations" (Experiment.Id.value id)
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
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
    let* experiment = Experiment.find database_label id in
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
        { experiment
        ; contacts
        ; invited_contacts
        ; create_message
        ; mailing = None
        }
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
  result |> extract_happy_path req
;;

let resend req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id, id =
    ( experiment_id req
    , HttpUtils.find_id Pool_common.Id.of_string Field.Invitation req )
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Experiment.Id.value experiment_id)
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* invitation = Invitation.find database_label id in
    let* experiment = Experiment.find database_label experiment_id in
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
  result |> extract_happy_path req
;;

let reset req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let experiment_id = experiment_id req in
  let redirect_path =
    Format.asprintf "/admin/experiments/%s" (Experiment.Id.value experiment_id)
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let* experiment = Experiment.find database_label experiment_id in
    let events =
      let open Cqrs_command.Experiment_command.ResetInvitations in
      handle ~tags experiment |> Lwt.return
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpMessage.set ~success:[ Success.ResetInvitations ] ]
    in
    events |>> handle
  in
  result |> extract_happy_path req
;;

module Access : sig
  include module type of Helpers.Access

  val resend : Rock.Middleware.t
end = struct
  include Helpers.Access
  module InvitationCommand = Cqrs_command.Invitation_command
  module Guardian = Middleware.Guardian

  let experiment_effects =
    Guardian.id_effects Experiment.Id.validate Field.Experiment
  ;;

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
