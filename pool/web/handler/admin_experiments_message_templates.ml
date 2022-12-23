module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout req

let experiment_id =
  HttpUtils.find_id Experiment.Id.of_string Pool_common.Message.Field.Experiment
;;

let template_id =
  HttpUtils.find_id
    Message_template.Id.of_string
    Pool_common.Message.Field.MessageTemplate
;;

let form ?template_id label req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , experiment_id
        |> Experiment.Id.value
        |> Format.asprintf "/admin/experiments/%s/edit" ))
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* experiment = Experiment.find database_label experiment_id in
    let* template =
      template_id
      |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
           Message_template.find database_label id >|+ CCOption.pure)
    in
    let* available_languages =
      match template_id with
      | None ->
        Pool_context.Tenant.get_tenant_languages req
        |> Lwt_result.lift
        |>> Message_template.find_available_languages
              database_label
              (experiment_id |> Experiment.Id.to_common)
              label
        >|+ CCOption.pure
      | Some _ -> Lwt_result.return None
    in
    Page.Admin.Experiments.message_template_form
      context
      experiment
      available_languages
      label
      template
      flash_fetcher
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_invitation req = form Message_template.Label.ExperimentInvitation req

let edit_invitation req =
  let template_id = template_id req in
  form ~template_id Message_template.Label.ExperimentInvitation req
;;

let new_invitation_post req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req |> Experiment.Id.to_common in
  let label = Message_template.Label.ExperimentInvitation in
  let redirect =
    let base =
      experiment_id
      |> Pool_common.Id.value
      |> Format.asprintf "/admin/experiments/%s/%s"
    in
    { success = base "edit"
    ; error = base (Message_template.Label.prefixed_human_url label)
    }
  in
  (write (Create (experiment_id, label, redirect))) req
;;

let update_invitation req =
  let open Admin_message_templates in
  let experiment_id = experiment_id req |> Experiment.Id.to_common in
  let template_id = template_id req in
  let redirect =
    let open Pool_common in
    let base =
      experiment_id |> Id.value |> Format.asprintf "/admin/experiments/%s/%s"
    in
    { success = base "edit"
    ; error =
        base
          (Format.asprintf
             "%s/%s/edit"
             Message.Field.(human_url MessageTemplate)
             (Message_template.Id.value template_id))
    }
  in
  (write (Update (template_id, redirect))) req
;;

module Access : sig
  val invitation : Rock.Middleware.t
end = struct
  module Field = Pool_common.Message.Field

  let experiment_effects =
    Middleware.Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let invitation =
    [ (fun id ->
        [ `Update, `Target (id |> Guard.Uuid.target_of Experiment.Id.value)
        ; `Update, `TargetEntity `Experiment
        ])
    ]
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;
end
