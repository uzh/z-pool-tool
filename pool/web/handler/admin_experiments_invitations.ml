module HttpUtils = Http_utils
module HttpMessage = HttpUtils.Message
module Field = Pool_common.Message.Field

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
    @@ let* ({ Experiment.id; filter; _ } as experiment) =
         Experiment.find database_label id
       in
       let%lwt key_list = Filter.all_keys database_label in
       let%lwt template_list = Filter.find_all_templates database_label () in
       let%lwt query_experiments, query_tags =
         match filter with
         | None -> Lwt.return ([], [])
         | Some filter ->
           Lwt.both
             (filter
              |> Filter.all_query_experiments
              |> Experiment.search_multiple_by_id database_label)
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
               (Matcher (id |> Experiment.Id.to_common))
               filter)
           >|+ CCOption.pure
       in
       Page.Admin.Experiments.invitations
         experiment
         key_list
         template_list
         query_experiments
         query_tags
         filtered_contacts
         context
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let sent_invitations req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s/invitations" (Experiment.Id.value id)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = Experiment.find database_label id in
       let query =
         let open Invitation in
         Query.from_request ~searchable_by ~sortable_by req
       in
       let%lwt statistics =
         Invitation.Statistics.by_experiment database_label id
       in
       let* invitations =
         Invitation.find_by_experiment
           ~query
           database_label
           experiment.Experiment.id
       in
       Page.Admin.Experiments.sent_invitations
         context
         experiment
         invitations
         statistics
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let redirect_path =
    Format.asprintf "/admin/experiments/%s/invitations" (Experiment.Id.value id)
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let open Pool_common in
    let tags = Pool_context.Logger.Tags.req req in
    let* contact_ids =
      Sihl.Web.Request.urlencoded_list Field.(Contacts |> array_key) req
      ||> CCList.map Id.of_string
      ||> fun list ->
      if CCList.is_empty list
      then Error Message.(NoOptionSelected Field.Contact)
      else Ok list
    in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* experiment = Experiment.find database_label id in
    let* contacts =
      let find_missing contacts =
        let retrieved_ids = CCList.map Contact.id contacts in
        CCList.fold_left
          (fun missing id ->
            match CCList.mem ~eq:Id.equal id retrieved_ids with
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
        |> CCList.map Id.value
        |> fun ids -> Error Message.(NotFoundList (Field.Contacts, ids))
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
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpMessage.set ~success:[ Message.(SentList Field.Invitations) ] ]
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
  let result { Pool_context.database_label; _ } =
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
      handle ~tags create_email { invitation; experiment } |> Lwt.return
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpMessage.set
            ~success:[ Pool_common.Message.(SentList Field.Invitations) ]
        ]
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
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let* experiment = Experiment.find database_label experiment_id in
    let events =
      let open Cqrs_command.Experiment_command.ResetInvitations in
      handle ~tags experiment |> Lwt.return
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpMessage.set ~success:[ Pool_common.Message.ResetInvitations ] ]
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
    Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let combined_effects fcn req =
    let open HttpUtils in
    let experiment_id = find_id Experiment.Id.of_string Field.Experiment req in
    let invitation_id = find_id Pool_common.Id.of_string Field.Invitation req in
    fcn experiment_id invitation_id
  ;;

  let index =
    Invitation.Guard.Access.index
    |> experiment_effects
    |> Guardian.validate_generic ~any_id:true
  ;;

  let create =
    InvitationCommand.Create.effects
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let read =
    Invitation.Guard.Access.read
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let resend =
    InvitationCommand.Resend.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;
end
