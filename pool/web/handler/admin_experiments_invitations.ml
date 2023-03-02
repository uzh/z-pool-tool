module HttpUtils = Http_utils
module HttpMessage = HttpUtils.Message
module Field = Pool_common.Message.Field

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
    @@ let* experiment = Experiment.find database_label id in
       let%lwt key_list = Filter.all_keys database_label in
       let%lwt template_list = Filter.find_all_templates database_label () in
       let%lwt query_experiments =
         match experiment.Experiment.filter with
         | None -> Lwt.return []
         | Some filter ->
           filter
           |> Filter.all_query_experiments
           |> Experiment.search_multiple_by_id database_label
       in
       let* filtered_contacts =
         if Sihl.Configuration.is_production ()
         then Lwt_result.return None
         else
           Filter.find_filtered_contacts
             database_label
             (experiment.Experiment.id |> Experiment.Id.to_common)
             experiment.Experiment.filter
           >|+ CCOption.pure
       in
       Page.Admin.Experiments.invitations
         experiment
         key_list
         template_list
         query_experiments
         filtered_contacts
         context
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
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
       let* invitations =
         Invitation.find_by_experiment
           ~query
           database_label
           experiment.Experiment.id
       in
       Page.Admin.Experiments.sent_invitations context experiment invitations
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
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
    let* { Pool_context.Tenant.tenant; _ } =
      Pool_context.Tenant.find req |> Lwt_result.lift
    in
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
    let* create_message =
      Message_template.ExperimentInvitation.prepare tenant experiment
    in
    let%lwt events =
      let open Cqrs_command.Invitation_command.Create in
      handle ~tags { experiment; contacts; invited_contacts; create_message }
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
  result |> HttpUtils.extract_happy_path req
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
    @@ let* { Pool_context.Tenant.tenant; _ } =
         Pool_context.Tenant.find req |> Lwt_result.lift
       in
       let* invitation = Invitation.find database_label id in
       let* experiment = Experiment.find database_label experiment_id in
       let* invitation_mail =
         Message_template.ExperimentInvitation.create
           tenant
           experiment
           invitation.Invitation.contact
       in
       let events =
         let open Cqrs_command.Invitation_command.Resend in
         handle ~tags invitation_mail { invitation; experiment } |> Lwt.return
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
  result |> HttpUtils.extract_happy_path req
;;

module Access : sig
  include Helpers.AccessSig

  val resend : Rock.Middleware.t
end = struct
  module InvitationCommand = Cqrs_command.Invitation_command
  module Field = Pool_common.Message.Field

  let invitation_effects =
    Middleware.Guardian.id_effects Pool_common.Id.of_string Field.Invitation
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity
      [ `Read, `TargetEntity `Invitation ]
  ;;

  let create =
    InvitationCommand.Create.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Pool_common.Id.value)
        ; `Read, `TargetEntity `Invitation
        ])
    ]
    |> invitation_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update = Middleware.Guardian.denied
  let delete = Middleware.Guardian.denied

  let resend =
    [ InvitationCommand.Resend.effects ]
    |> invitation_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
