open CCFun
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field
open HttpUtils.Filter

let src = Logs.Src.create "handler.admin.filter"
let template_id = HttpUtils.find_id Filter.Id.of_string Field.Filter

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
  |> map (CCOption.to_result Pool_common.Message.(Invalid Field.Id))
  |> all_ok
;;

let get_id req field encode =
  Sihl.Web.Router.param req @@ Field.show field |> encode
;;

let index req =
  Http_utils.Htmx.handler
    ~active_navigation:"/admin/filter"
    ~error_path:(Format.asprintf "/admin/filter")
    ~query:(module Filter)
    ~create_layout
    req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let%lwt filter_list, query =
    Filter.find_all_templates_query query database_label
  in
  let open Page.Admin.Filter in
  (if HttpUtils.Htmx.is_hx_request req then list else index)
    context
    filter_list
    query
  |> Lwt_result.return
;;

let form is_edit req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, Format.asprintf "/admin/filter")
    @@ let* filter =
         if is_edit
         then
           get_id req Field.Filter Pool_common.Id.of_string
           |> Filter.find_template database_label
           >|+ CCOption.pure
         else Lwt.return_none |> Lwt_result.ok
       in
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
       let%lwt key_list = Filter.all_keys database_label in
       Page.Admin.Filter.edit
         context
         filter
         key_list
         query_experiments
         query_tags
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let edit = form true
let new_form = form false

let write action req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.database_label; _ } =
    let tags = Pool_context.Logger.Tags.req req in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* query =
      let open CCResult in
      HttpUtils.find_in_urlencoded Field.Query urlencoded
      >>= Filter.query_of_string
      |> Lwt_result.lift
    in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list =
      Filter.find_templates_of_query database_label query
    in
    let events =
      let lift = Lwt_result.lift in
      match action with
      | Experiment exp ->
        let open Cqrs_command.Experiment_command in
        (match exp.Experiment.filter with
         | None ->
           CreateFilter.handle ~tags exp key_list template_list query |> lift
         | Some filter ->
           UpdateFilter.handle ~tags key_list template_list filter query |> lift)
      | Template filter ->
        let open Cqrs_command.Filter_command in
        let* decoded = urlencoded |> default_decode |> lift in
        (match filter with
         | None ->
           Create.handle ~tags key_list template_list query decoded |> lift
         | Some filter ->
           Update.handle ~tags key_list template_list filter query decoded
           |> lift)
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
    in
    let success () =
      let open Pool_common.Message in
      let field = Field.Filter in
      let redirect path msg =
        HttpUtils.Htmx.htmx_redirect
          path
          ~actions:[ Message.set ~success:[ msg ] ]
          ()
      in
      match action with
      | Template None -> redirect "/admin/filter" (Created field)
      | Template (Some filter) ->
        redirect
          (Format.asprintf "/admin/filter/%s/edit" Filter.(Id.value filter.id))
          (Updated field)
      | Experiment exp ->
        let msg =
          if CCOption.is_some exp.Experiment.filter
          then Updated field
          else Created field
        in
        redirect
          (Format.asprintf
             "/admin/experiments/%s/invitations"
             Experiment.(Id.value exp.id))
          msg
    in
    events |>> handle |>> success
  in
  result
  |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let handle_toggle_predicate_type action req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.language; database_label; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let templates_disabled = templates_disabled urlencoded in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list =
      find_all_templates database_label templates_disabled
    in
    let* query =
      let open CCResult in
      Lwt_result.lift
      @@ let* current =
           HttpUtils.find_in_urlencoded Field.Query urlencoded
           >|= Yojson.Safe.from_string
           >>= Filter.Human.of_yojson key_list
         in
         let* predicate_type =
           HttpUtils.find_in_urlencoded Field.Predicate urlencoded
         in
         Filter.toggle_predicate_type current predicate_type
    in
    let* identifier = find_identifier urlencoded |> Lwt_result.lift in
    let%lwt quey_experiments, query_tags =
      Lwt.both
        (query
         |> Filter.Human.all_query_experiments
         |> Experiment.search_multiple_by_id database_label)
        (query
         |> Filter.Human.all_query_tags
         |> Tags.find_multiple database_label)
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
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt_result.return
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
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
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt.return_ok
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let handle_add_predicate action req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.language; database_label; _ } =
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let templates_disabled = templates_disabled urlencoded in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list =
      find_all_templates database_label templates_disabled
    in
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
    [ filter_form; add_button ]
    |> HttpUtils.Htmx.multi_html_to_plain_text_response
    |> Lwt_result.return
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let count_contacts req =
  let experiment_id =
    HttpUtils.find_id Experiment.Id.of_string Field.Experiment req
  in
  let result { Pool_context.database_label; _ } =
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
    Filter.(
      count_filtered_contacts
        database_label
        (Matcher (experiment.Experiment.id |> Experiment.Id.to_common))
        query)
    >|+ fun count -> `Assoc [ "count", `Int count ]
  in
  result |> HttpUtils.Json.handle_yojson_response ~src req
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
    let id = template_id req in
    req
    |> database_label_from_req
    >>= flip Filter.find id
    |>> (fun e -> fnc (Template (Some e)) req)
    >|> function
    | Ok res -> Lwt.return res
    | Error err ->
      Http_utils.redirect_to_with_actions
        "/admin/filter"
        [ Message.set ~error:[ err ] ]
  ;;

  let update_template = handler write
  let add_predicate = handler handle_add_predicate
  let toggle_predicate_type = handler handle_toggle_predicate_type
  let toggle_key = handler handle_toggle_key
end

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Command = Cqrs_command.Filter_command
  module Guardian = Middleware.Guardian

  let filter_effects = Guardian.id_effects Filter.Id.of_string Field.Filter
  let index = Filter.Guard.Access.index |> Guardian.validate_admin_entity
  let create = Command.Create.effects |> Guardian.validate_admin_entity

  let update =
    Command.Update.effects |> filter_effects |> Guardian.validate_generic
  ;;
end
