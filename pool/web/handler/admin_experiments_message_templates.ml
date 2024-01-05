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

let template_label = Admin_message_templates.template_label

let experiment_path experiment_id =
  Format.asprintf "/admin/experiments/%s" (Experiment.Id.value experiment_id)
;;

let form_redirects experiment_id error_path =
  let open Admin_message_templates in
  let base = experiment_path experiment_id in
  { success = base; error = Format.asprintf "%s/%s" base error_path }
;;

type form_context =
  | New of Message_template.Label.t
  | Edit of Message_template.Id.t

let form form_context req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, experiment_path experiment_id)
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let* experiment = Experiment.find database_label experiment_id in
    let* template, available_languages, label =
      match form_context with
      | New label ->
        let exclude =
          CCOption.map CCList.return experiment.Experiment.language
        in
        let%lwt available_languages =
          Pool_context.Tenant.get_tenant_languages_exn req
          |> Message_template.missing_template_languages
               database_label
               (experiment_id |> Experiment.Id.to_common)
               label
               ?exclude
          ||> CCOption.return
        in
        Lwt_result.return (None, available_languages, label)
      | Edit id ->
        let* template = Message_template.find database_label id in
        Lwt_result.return (Some template, None, template.Message_template.label)
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

let new_post label req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req in
  let redirect =
    form_redirects
      experiment_id
      (Message_template.Label.prefixed_human_url label)
  in
  (write (Create (experiment_id |> Experiment.Id.to_common, label, redirect)))
    req
;;

let new_message_template req =
  let label = template_label req in
  form (New label) req
;;

let new_message_template_post req =
  let label = template_label req in
  new_post label req
;;

let update_template req =
  let open Utils.Lwt_result.Infix in
  let open Admin_message_templates in
  let open Message_template in
  let experiment_id = experiment_id req in
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
      (experiment_path experiment_id)
      [ HttpUtils.Message.set ~error:[ err ] ]
;;

let edit_template req =
  let template_id = template_id req in
  form (Edit template_id) req
;;

let delete req =
  let result { Pool_context.database_label; _ } =
    let experiment_id = experiment_id req in
    let template_id = template_id req in
    let redirect = experiment_path experiment_id in
    Helpers.MessageTemplates.delete database_label template_id redirect
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

module Access : sig
  include module type of Helpers.Access

  val message_template : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Field = Pool_common.Message.Field
  module Guardian = Middleware.Guardian

  let experiment_effects =
    Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let message_template =
    Experiment.Guard.Access.update
    |> experiment_effects
    |> Guardian.validate_generic
  ;;
end
