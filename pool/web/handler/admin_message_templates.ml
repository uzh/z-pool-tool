open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.admin.message_templates"
let create_layout req = General.create_tenant_layout req
let template_id = HttpUtils.find_id Message_template.Id.of_string Field.MessageTemplate

let template_label req =
  let open Message_template.Label in
  try
    Ok
      (HttpUtils.find_id read_from_url Field.Label req
       |> fun label -> CCList.find (equal label) customizable_by_experiment)
  with
  | _ -> Error Pool_message.(Error.Invalid Field.Label)
;;

let database_label_of_req req =
  let open CCResult in
  Pool_context.(req |> find >|= fun { database_label; _ } -> database_label)
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Response.bad_request_render_error context
    @@
    let%lwt template_list = Message_template.all_default database_label () in
    Page.Admin.MessageTemplate.index context template_list
    |> create_layout ~active_navigation:"/admin/message-template" req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let edit req =
  let open Utils.Lwt_result.Infix in
  let id = template_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let* template = Message_template.find database_label id >|- Response.not_found in
    Response.bad_request_render_error context
    @@
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    Page.Admin.MessageTemplate.edit ~text_messages_enabled context template tenant
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

type redirect =
  { success : string
  ; error : Rock.Request.t -> Rock.Response.t Lwt.t
  }

type action =
  | Create of Pool_common.Id.t * Message_template.Label.t * redirect
  | Update of Message_template.Id.t * redirect

let write action req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
  in
  let redirect, success =
    match action with
    | Create (_, _, redirect) -> redirect, Success.Created Field.MessageTemplate
    | Update (_, redirect) -> redirect, Success.Updated Field.MessageTemplate
  in
  let result { Pool_context.database_label; user; _ } =
    Response.bad_request_on_error ~urlencoded redirect.error
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let events =
      let open Cqrs_command.Message_template_command in
      match action with
      | Create (entity_id, label, _) ->
        let%lwt available_languages =
          Pool_context.Tenant.get_tenant_languages_exn req
          |> Message_template.missing_template_languages database_label entity_id label
        in
        Create.(
          urlencoded
          |> decode
          |> Lwt_result.lift
          >== handle ~tags label entity_id available_languages)
      | Update (id, _) ->
        let* template = Message_template.find database_label id in
        Update.(urlencoded |> decode |> Lwt_result.lift >== handle template)
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        redirect.success
        [ HttpUtils.Message.set ~success:[ success ] ]
    in
    events |>> handle
  in
  Response.handle ~src req result
;;

let update req =
  let id = template_id req in
  let redirect_path =
    id |> Message_template.Id.value |> Format.asprintf "/admin/message-template/%s/edit"
  in
  let redirect = { success = redirect_path; error = edit } in
  write (Update (id, redirect)) req
;;

let default_templates_from_request req ?languages database_label params =
  let open Utils.Lwt_result.Infix in
  let open Page.Admin.MessageTemplate in
  let* label = template_label req |> Lwt_result.lift in
  let languages =
    languages
    |> CCOption.value ~default:(Pool_context.Tenant.get_tenant_languages_exn req)
  in
  let find_param field = HttpUtils.find_in_urlencoded_opt field params in
  let experiment_entity experiment_id =
    let open Experiment in
    let experiment_id = Id.of_string experiment_id in
    let* experiment = find database_label experiment_id in
    let entity = Experiment experiment_id in
    Lwt_result.return ([ Id.to_common experiment.id ], entity)
  in
  let session_entity session_id =
    let open Session in
    let session_id = Id.of_string session_id in
    let* session = find database_label session_id in
    let* experiment =
      Experiment.find_of_session database_label (Id.to_common session.id)
    in
    let entity = Session session_id in
    Lwt_result.return
      ( [ Experiment.Id.to_common experiment.Experiment.id; Id.to_common session.id ]
      , entity )
  in
  let entities = [ Field.Session, session_entity; Field.Experiment, experiment_entity ] in
  let* entity_uuids, entity =
    entities
    |> CCList.find_map (fun (key, entity_fnc) ->
      find_param key |> CCOption.map entity_fnc)
    |> CCOption.value ~default:(Lwt_result.fail (Error.NotFound Field.Context))
  in
  let%lwt templates =
    Message_template.find_entity_defaults_by_label
      database_label
      ~entity_uuids
      languages
      label
  in
  Lwt_result.return (templates, entity)
;;

let preview_default req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.database_label; language; _ } =
    let query_params = Sihl.Web.Request.query_list req in
    let* label = template_label req |> Lwt_result.lift in
    let* message_templates, _ =
      default_templates_from_request req database_label query_params
    in
    Page.Admin.MessageTemplate.preview_template_modal language (label, message_templates)
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  result |> Response.Htmx.handle ~src req
;;

let reset_to_default_htmx req =
  let open Utils.Lwt_result.Infix in
  let open Message_template in
  let result ({ Pool_context.database_label; _ } as context) =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* current_template =
      let open Message_template in
      let open CCOption in
      HttpUtils.find_in_urlencoded_opt Field.MessageTemplate urlencoded
      >|= (fun id -> id |> Id.of_string |> find database_label >|+ CCOption.return)
      |> CCOption.value ~default:(Lwt_result.return None)
    in
    let* template_language =
      match current_template with
      | Some ({ language; _ } : t) -> Lwt_result.return language
      | None ->
        HttpUtils.find_in_urlencoded Field.Language urlencoded
        |> Lwt_result.lift
        >== Pool_common.Language.create
    in
    let* languages =
      let open CCList in
      Lwt_result.lift
      @@ (HttpUtils.find_in_urlencoded_opt Field.AvailableLanguages urlencoded
          |> function
          | Some languages ->
            languages
            |> CCString.split_on_char ','
            >|= CCString.trim
            |> fun language_strings ->
            language_strings
            >|= Pool_common.Language.create
            |> all_ok
            |> CCResult.map CCOption.return
          | None -> Ok None)
    in
    let* message_templates, entity =
      default_templates_from_request
        req
        ~languages:[ template_language ]
        database_label
        urlencoded
    in
    let* template =
      message_templates
      |> CCList.head_opt
      |> CCOption.to_result (Error.NotFound Field.MessageTemplate)
      |> Lwt_result.lift
    in
    let%lwt text_messages_enabled = Pool_context.Tenant.text_messages_enabled req in
    let form_context =
      if CCOption.is_some current_template then `Update template else `Create template
    in
    Page.Admin.MessageTemplate.template_inputs
      ~entity
      ?languages
      ~text_messages_enabled
      context
      form_context
      template.label
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  result |> Response.Htmx.handle ~src req
;;

let changelog req =
  let id = template_id req in
  let url = HttpUtils.Url.Admin.message_template_path ~suffix:"changelog" ~id () in
  Helpers.Changelog.htmx_handler ~url (Message_template.Id.to_common id) req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Command = Cqrs_command.Message_template_command
  module Guardian = Middleware.Guardian

  let template_effects =
    Guardian.id_effects Message_template.Id.validate Field.MessageTemplate
  ;;

  let index = Message_template.Guard.Access.index |> Guardian.validate_admin_entity
  let update = template_effects Command.Update.effects
end
