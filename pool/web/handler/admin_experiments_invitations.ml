module HttpUtils = Http_utils
module Message = HttpUtils.Message

let invitation_template_data tenant_db system_languages =
  let open Lwt_result.Syntax in
  let%lwt res =
    Lwt_list.map_s
      (fun lang ->
        let find = CCFun.flip (I18n.find_by_key tenant_db) lang in
        let* subject = find I18n.Key.InvitationSubject in
        let* text = find I18n.Key.InvitationText in
        Lwt_result.return (lang, (subject, text)))
      system_languages
  in
  CCList.all_ok res |> Lwt.return
;;

let create_layout req = General.create_tenant_layout req

let id_from_req req =
  Pool_common.Message.Field.(Experiment |> show)
  |> Sihl.Web.Router.param req
  |> Pool_common.Id.of_string
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let id = id_from_req req in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Pool_common.Id.value id)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = Experiment.find tenant_db id in
       let%lwt key_list = Filter.all_keys tenant_db in
       let* filtered_contacts =
         Contact.find_filtered
           tenant_db
           experiment.Experiment.id
           (experiment |> Experiment.filter_predicate)
       in
       Page.Admin.Experiments.invitations
         experiment
         key_list
         filtered_contacts
         context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let sent_invitations req =
  let open Utils.Lwt_result.Infix in
  let id = id_from_req req in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value id)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, error_path)
    @@ let* experiment = Experiment.find tenant_db id in
       let* invitations =
         Invitation.find_by_experiment tenant_db experiment.Experiment.id
       in
       Page.Admin.Experiments.sent_invitations context experiment invitations
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id_from_req req in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* contact_ids =
         let open Lwt.Infix in
         Sihl.Web.Request.urlencoded_list
           Pool_common.Message.Field.(Contacts |> array_key)
           req
         >|= CCList.map Pool_common.Id.of_string
         >|= fun list ->
         if CCList.is_empty list
         then Error Pool_common.Message.(NoOptionSelected Field.Contact)
         else Ok list
       in
       let* experiment = Experiment.find tenant_db experiment_id in
       let* contacts =
         let find_missing contacts =
           let retrieved_ids = CCList.map Contact.id contacts in
           CCList.fold_left
             (fun missing id ->
               match CCList.mem ~eq:Pool_common.Id.equal id retrieved_ids with
               | true -> missing
               | false -> CCList.cons id missing)
             []
             contact_ids
         in
         let%lwt contacts = Contact.find_multiple tenant_db contact_ids in
         Lwt_result.lift
         @@
         match CCList.length contact_ids == CCList.length contacts with
         | true -> Ok contacts
         | false ->
           find_missing contacts
           |> CCList.map Pool_common.Id.value
           |> fun ids ->
           Error Pool_common.Message.(NotFoundList (Field.Contacts, ids))
       in
       let* system_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       let* i18n_texts = invitation_template_data tenant_db system_languages in
       let%lwt invited_contacts =
         Invitation.find_multiple_by_experiment_and_contacts
           tenant_db
           (CCList.map Contact.id contacts)
           experiment
       in
       let%lwt events =
         Cqrs_command.Invitation_command.Create.(
           handle
             { experiment; contacts; invited_contacts }
             system_languages
             i18n_texts
           |> Lwt_result.lift)
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(SentList Field.Invitations) ]
           ]
       in
       events |> Lwt_result.lift |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let resend req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    let open Pool_common.Message.Field in
    let open HttpUtils in
    ( get_field_router_param req Experiment |> Pool_common.Id.of_string
    , get_field_router_param req Invitation |> Pool_common.Id.of_string )
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* invitation = Invitation.find tenant_db id in
       let* experiment = Experiment.find tenant_db experiment_id in
       let* system_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       let* i18n_texts = invitation_template_data tenant_db system_languages in
       let events =
         let open Cqrs_command.Invitation_command.Resend in
         handle { invitation; experiment } system_languages i18n_texts
         |> Lwt.return
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(SentList Field.Invitations) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
