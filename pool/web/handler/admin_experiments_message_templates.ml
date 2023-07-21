module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.experiments_mailing"
let create_layout req = General.create_tenant_layout req

let experiment_id =
  HttpUtils.find_id Experiment.Id.of_string Pool_common.Message.Field.Experiment
;;

let template_id =
  HttpUtils.find_id
    Message_template.Id.of_string
    Pool_common.Message.Field.MessageTemplate
;;

let form_redirects experiment_id error_path =
  let open Admin_message_templates in
  let base =
    experiment_id
    |> Pool_common.Id.value
    |> Format.asprintf "/admin/experiments/%s"
  in
  { success = base; error = Format.asprintf "%s/%s" base error_path }
;;

let form ?template_id label req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , experiment_id
        |> Experiment.Id.value
        |> Format.asprintf "/admin/experiments/%s" ))
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* experiment = Experiment.find database_label experiment_id in
    let* template =
      template_id
      |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
        Message_template.find database_label id >|+ CCOption.pure)
    in
    let%lwt available_languages =
      match template_id with
      | None ->
        Pool_context.Tenant.get_tenant_languages_exn req
        |> Message_template.find_available_languages
             database_label
             (experiment_id |> Experiment.Id.to_common)
             label
        ||> CCOption.return
      | Some _ -> Lwt.return_none
    in
    Page.Admin.Experiments.message_template_form
      context
      tenant
      experiment
      available_languages
      label
      template
      flash_fetcher
    >|> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let new_invitation = form Message_template.Label.ExperimentInvitation

let new_invitation_post req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req |> Experiment.Id.to_common in
  let label = Message_template.Label.ExperimentInvitation in
  let redirect =
    form_redirects
      experiment_id
      (Message_template.Label.prefixed_human_url label)
  in
  (write (Create (experiment_id, label, redirect))) req
;;

let new_session_reminder = form Message_template.Label.SessionReminder

let new_session_reminder_post req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req |> Experiment.Id.to_common in
  let label = Message_template.Label.SessionReminder in
  let redirect =
    form_redirects
      experiment_id
      (Message_template.Label.prefixed_human_url label)
  in
  (write (Create (experiment_id, label, redirect))) req
;;

let update_template req =
  let open Utils.Lwt_result.Infix in
  let open Admin_message_templates in
  let open Message_template in
  let experiment_id = experiment_id req |> Experiment.Id.to_common in
  let template_id = template_id req in
  let%lwt template =
    req
    |> database_label_of_req
    |> Lwt_result.lift
    >>= CCFun.flip find template_id
  in
  match template with
  | Ok template ->
    let redirect =
      form_redirects experiment_id (prefixed_template_url template)
    in
    (write (Update (template_id, redirect))) req
  | Error err ->
    HttpUtils.redirect_to_with_actions
      (Format.asprintf
         "/admin/experiments/%s"
         (Pool_common.Id.value experiment_id))
      [ HttpUtils.Message.set ~error:[ err ] ]
;;

let edit_template req =
  let template_id = template_id req in
  form ~template_id Message_template.Label.ExperimentInvitation req
;;

module Access : sig
  include module type of Helpers.Access

  val invitation : Rock.Middleware.t
  val session_reminder : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Field = Pool_common.Message.Field
  module Guardian = Middleware.Guardian

  let experiment_effects =
    Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let invitation =
    Experiment.Guard.Access.update
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let session_reminder =
    Experiment.Guard.Access.update
    |> experiment_effects
    |> Guardian.validate_generic
  ;;
end
