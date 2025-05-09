module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.experiments_mailing"
let create_layout req = General.create_tenant_layout req

let experiment_id =
  HttpUtils.find_id Experiment.Id.of_string Pool_message.Field.Experiment
;;

let template_id =
  HttpUtils.find_id Message_template.Id.of_string Pool_message.Field.MessageTemplate
;;

let template_label = Admin_message_templates.template_label

let experiment_path experiment_id =
  Format.asprintf "/admin/experiments/%s" (Experiment.Id.value experiment_id)
;;

type form_context =
  | New of Message_template.Label.t
  | Edit of Message_template.Id.t

let form form_context req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* experiment =
      Experiment.find database_label experiment_id >|- Response.not_found
    in
    Response.bad_request_render_error context
    @@
    let open Message_template in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    let* form_context, available_languages, label =
      let experiment_id = experiment_id |> Experiment.Id.to_common in
      match form_context with
      | New label ->
        let%lwt languages =
          Pool_context.Tenant.get_tenant_languages_exn req
          |> missing_template_languages database_label experiment_id label
        in
        let%lwt template =
          find_entity_defaults_by_label
            database_label
            ~entity_uuids:[ experiment_id ]
            languages
            label
          ||> CCList.hd
        in
        Lwt_result.return (`Create template, Some languages, label)
      | Edit template_id ->
        let* template = Message_template.find database_label template_id in
        Lwt_result.return (`Update template, None, template.label)
    in
    Page.Admin.Experiments.message_template_form
      ~text_messages_enabled
      context
      tenant
      experiment
      available_languages
      label
      form_context
    >|> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let new_post label req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req in
  let redirect = { success = experiment_path experiment_id; error = form (New label) } in
  (write (Create (experiment_id |> Experiment.Id.to_common, label, redirect))) req
;;

let redirect_back req err =
  let open HttpUtils in
  redirect_back
    ~fallback:(Url.Admin.experiment_path ())
    req
    [ Message.set ~error:[ err ] ]
;;

let new_message_template req =
  match template_label req with
  | Ok label -> form (New label) req
  | Error err -> redirect_back req err
;;

let new_message_template_post req =
  match template_label req with
  | Ok label -> new_post label req
  | Error err -> redirect_back req err
;;

let edit_template req =
  let template_id = template_id req in
  form (Edit template_id) req
;;

let update_template req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req in
  let template_id = template_id req in
  let redirect = { success = experiment_path experiment_id; error = edit_template } in
  (write (Update (template_id, redirect))) req
;;

let delete req =
  let result ({ Pool_context.database_label; user; _ } as context) =
    Response.bad_request_render_error context
    @@
    let experiment_id = experiment_id req in
    let template_id = template_id req in
    let redirect = experiment_path experiment_id in
    Helpers.MessageTemplates.delete database_label user template_id redirect
  in
  Response.handle ~src req result
;;

let changelog req =
  let open Utils.Lwt_result.Infix in
  let open Message_template in
  let result (_ : Pool_context.t) =
    let experiment_id = experiment_id req in
    let* template_label = template_label req |> Lwt_result.lift in
    let id = template_id req in
    let url =
      HttpUtils.Url.Admin.experiment_message_template_path
        experiment_id
        template_label
        ~suffix:"changelog"
        ~id
        ()
    in
    Helpers.Changelog.htmx_handler ~url (Id.to_common id) req |> Lwt_result.ok
  in
  Response.Htmx.handle req result
;;

module Access : sig
  include module type of Helpers.Access

  val message_template : Rock.Middleware.t
end = struct
  include Helpers.Access
  module Field = Pool_message.Field
  module Guardian = Middleware.Guardian

  let experiment_effects = Guardian.id_effects Experiment.Id.validate Field.Experiment
  let message_template = experiment_effects Experiment.Guard.Access.update
end
