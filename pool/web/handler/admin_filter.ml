open CCFun
open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response
open HttpUtils.Filter

let src = Logs.Src.create "handler.admin.filter"
let filter_id = HttpUtils.find_id Filter.Id.of_string Field.Filter

let templates_disabled urlencoded =
  let open CCOption in
  let open Component.Utils in
  CCList.assoc_opt ~eq:CCString.equal templates_disabled_key urlencoded
  >>= CCList.head_opt
  |> map_or ~default:false (CCString.equal "true")
;;

let find_all_templates database_label templates_disabled =
  if templates_disabled
  then Lwt.return []
  else Filter.find_all_templates database_label ()
;;

let create_layout req = General.create_tenant_layout req

let find_identifier urlencoded =
  let open CCResult in
  HttpUtils.find_in_urlencoded Field.Id urlencoded
  >>= fun str ->
  str
  |> CCString.split ~by:"-"
  |> fun str ->
  let open CCList in
  str
  |> map CCInt.of_string
  |> map (CCOption.to_result (Error.Invalid Field.Id))
  |> all_ok
;;

let get_id req field encode = Sihl.Web.Router.param req @@ Field.show field |> encode

let index req =
  Response.Htmx.index_handler
    ~active_navigation:"/admin/filter"
    ~query:(module Filter)
    ~create_layout
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt filter_list, query = Filter.find_templates_by query database_label in
  let open Page.Admin.Filter in
  (if HttpUtils.Htmx.is_hx_request req then list else index) context filter_list query
  |> Lwt_result.return
;;

let form is_edit req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    let* filter =
      if is_edit
      then
        get_id req Field.Filter Pool_common.Id.of_string
        |> Filter.find_template database_label
        >|+ CCOption.pure
        >|- Response.not_found
      else Lwt.return_none |> Lwt_result.ok
    in
    Response.bad_request_render_error context
    @@
    let%lwt query_experiments, query_tags =
      match filter with
      | None -> Lwt.return ([], [])
      | Some filter ->
        Lwt.both
          (filter
           |> Filter.all_query_experiments
           |> Experiment.search_multiple_by_id database_label)
          (filter |> Filter.all_query_tags |> Tags.find_multiple database_label)
    in
    let%lwt key_list = Filter.all_keys database_label in
    Page.Admin.Filter.edit context filter key_list query_experiments query_tags
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let edit = form true
let new_form = form false

let write action req =
  let open Utils.Lwt_result.Infix in
  let open Cqrs_command in
  let result { Pool_context.database_label; user; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* query =
      let open CCResult in
      HttpUtils.find_in_urlencoded Field.Query urlencoded
      >>= Filter.query_of_string
      |> Lwt_result.lift
    in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list = Filter.find_templates_of_query database_label query in
    let events =
      let lift = Lwt_result.lift in
      match action with
      | Experiment exp ->
        let open Experiment_command in
        let* admin = Pool_context.get_admin_user user |> Lwt_result.lift in
        let matcher_events filter =
          Assignment_job.update_matches_filter
            ~current_user:admin
            database_label
            (`Experiment (exp, Some filter))
        in
        (match exp.Experiment.filter with
         | None ->
           let open CreateFilter in
           let* filter = create_filter key_list template_list query |> lift in
           let* matcher_events = matcher_events filter in
           handle ~tags exp matcher_events filter |> lift
         | Some filter ->
           let open UpdateFilter in
           let* updated = create_filter key_list template_list filter query |> lift in
           let* matcher_events = matcher_events updated in
           handle ~tags exp matcher_events filter updated |> lift)
      | Template filter ->
        let open Cqrs_command.Filter_command in
        let* decoded = urlencoded |> default_decode |> lift in
        (match filter with
         | None -> Create.handle ~tags key_list template_list query decoded |> lift
         | Some filter ->
           Update.handle ~tags key_list template_list filter query decoded |> lift)
    in
    let handle events = Pool_event.handle_events ~tags database_label user events in
    let success () =
      let open Success in
      let field = Field.Filter in
      let redirect path msg =
        Response.Htmx.redirect path ~actions:[ Message.set ~success:[ msg ] ]
      in
      match action with
      | Template None -> redirect "/admin/filter" (Created field)
      | Template (Some filter) ->
        redirect
          (Format.asprintf "/admin/filter/%s/edit" Filter.(Id.value filter.id))
          (Updated field)
      | Experiment exp ->
        let msg =
          if CCOption.is_some exp.Experiment.filter then Updated field else Created field
        in
        redirect
          (Format.asprintf
             "/admin/experiments/%s/invitations"
             Experiment.(Id.value exp.id))
          msg
    in
    events |>> handle |>> success
  in
  Response.Htmx.handle ~src ~error_as_notification:true req result
;;

let handle_toggle_predicate_type action req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.language; database_label; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let templates_disabled = templates_disabled urlencoded in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list = find_all_templates database_label templates_disabled in
    let* query =
      let open CCResult in
      Lwt_result.lift
      @@ let* current =
           HttpUtils.find_in_urlencoded Field.Query urlencoded
           >|= Yojson.Safe.from_string
           >>= Filter.Human.of_yojson key_list
         in
         let* predicate_type = HttpUtils.find_in_urlencoded Field.Predicate urlencoded in
         Filter.toggle_predicate_type current predicate_type
    in
    let* identifier = find_identifier urlencoded |> Lwt_result.lift in
    let%lwt quey_experiments, query_tags =
      Lwt.both
        (query
         |> Filter.Human.all_query_experiments
         |> Experiment.search_multiple_by_id database_label)
        (query |> Filter.Human.all_query_tags |> Tags.find_multiple database_label)
    in
    Component.Filter.(
      predicate_form
        language
        action
        key_list
        template_list
        templates_disabled
        quey_experiments
        query_tags
        (Some query)
        ~identifier
        ())
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  Response.Htmx.handle ~src ~error_as_notification:true req result
;;

let handle_toggle_key _ req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.language; database_label; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* key =
      HttpUtils.find_in_urlencoded Field.Key urlencoded
      |> Lwt_result.lift
      >>= Filter.key_of_string database_label
    in
    Component.Filter.predicate_value_form language [] [] ~key ()
    |> Response.Htmx.of_html
    |> Lwt.return_ok
  in
  Response.Htmx.handle ~src req result
;;

let handle_add_predicate action req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.language; database_label; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let templates_disabled = templates_disabled urlencoded in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list = find_all_templates database_label templates_disabled in
    let* identifier = find_identifier urlencoded |> Lwt_result.lift in
    let rec increment_identifier identifier =
      match identifier with
      | [] -> []
      | [ tl ] -> [ tl + 1 ]
      | hd :: tl -> hd :: increment_identifier tl
    in
    let query = Filter.Human.init () |> CCOption.pure in
    let filter_form =
      Component.Filter.predicate_form
        language
        action
        key_list
        template_list
        templates_disabled
        []
        []
        query
        ~identifier
        ()
    in
    let add_button =
      Component.Filter.add_predicate_btn
        action
        (increment_identifier identifier)
        templates_disabled
    in
    [ filter_form; add_button ] |> Response.Htmx.of_html_list |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;

let filter_statistics req =
  let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment req in
  let result { Pool_context.database_label; language; _ } =
    let open Utils.Lwt_result.Infix in
    let* experiment = Experiment.find database_label experiment_id in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* query =
      let open CCResult in
      HttpUtils.find_in_urlencoded Field.Query urlencoded
      |> CCOption.of_result
      |> CCOption.map_or ~default:(Ok None) (fun str ->
        str |> Filter.query_of_string >|= CCOption.pure)
      |> Lwt_result.lift
    in
    let* statistics =
      Statistics.ExperimentFilter.create database_label experiment query
    in
    Component.Statistics.ExperimentFilter.create language statistics
    |> Response.Htmx.of_html
    |> Lwt_result.return
  in
  Response.Htmx.handle ~src req result
;;

module Create = struct
  let action = Template None
  let create_template = write action
  let add_predicate = handle_add_predicate action
  let toggle_predicate_type = handle_toggle_predicate_type action
  let toggle_key = handle_toggle_key ()
end

module Update = struct
  let handler fnc req =
    let open Utils.Lwt_result.Infix in
    let id = filter_id req in
    req
    |> database_label_from_req
    >>= flip Filter.find id
    |>> (fun e -> fnc (Template (Some e)) req)
    >|> function
    | Ok res -> Lwt.return res
    | Error err ->
      Http_utils.redirect_to_with_actions "/admin/filter" [ Message.set ~error:[ err ] ]
  ;;

  let update_template = handler write
  let add_predicate = handler handle_add_predicate
  let toggle_predicate_type = handler handle_toggle_predicate_type
  let toggle_key = handler handle_toggle_key
end

let changelog req =
  let open Filter in
  let id = filter_id req in
  let url = HttpUtils.Url.Admin.filter_path ~suffix:"changelog" ~id () in
  let to_human { Pool_context.database_label; language; _ } =
    Custom_field.changelog_to_human database_label language
  in
  Helpers.Changelog.htmx_handler ~to_human ~url (Id.to_common id) req
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Command = Cqrs_command.Filter_command
  module Guardian = Middleware.Guardian

  let filter_effects = Guardian.id_effects Filter.Id.validate Field.Filter
  let index = Filter.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  let create = Command.Create.effects () |> Guardian.validate_admin_entity
  let update = filter_effects Command.Update.effects
end
