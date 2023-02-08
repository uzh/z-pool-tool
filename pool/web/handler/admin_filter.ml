module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field
open HttpUtils.Filter

let templates_disabled_key = Component.Filter.templates_disabled_key

let template_id =
  HttpUtils.find_id Filter.Id.of_string Pool_common.Message.Field.Filter
;;

let error_to_html ?(language = Pool_common.Language.En) err =
  err
  |> Pool_common.(Utils.error_to_string language)
  |> Tyxml.Html.txt
  |> CCList.pure
  |> Tyxml.Html.div
;;

let templates_disabled urlencoded =
  let open CCOption in
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

let find_in_params urlencoded field =
  CCList.assoc_opt
    ~eq:CCString.equal
    Pool_common.Message.Field.(show field)
    urlencoded
  |> CCFun.flip CCOption.bind CCList.head_opt
  |> CCOption.to_result Pool_common.Message.(Invalid field)
;;

let find_identifier urlencoded =
  let open CCResult in
  find_in_params urlencoded Field.Id
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
  let open Utils.Lwt_result.Infix in
  let error_path = Format.asprintf "/admin/filter" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let%lwt filter_list = Filter.find_all_templates database_label () in
       Page.Admin.Filter.index context filter_list
       |> create_layout ~active_navigation:"/admin/filter" req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
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
       let%lwt query_experiments =
         match filter with
         | None -> Lwt.return []
         | Some filter ->
           filter
           |> Filter.all_query_experiments
           |> Experiment.multiple_search_results_by_id database_label
       in
       let%lwt key_list = Filter.all_keys database_label in
       Page.Admin.Filter.edit context filter key_list query_experiments
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let edit = form true
let new_form = form false

let write action req =
  let open Utils.Lwt_result.Infix in
  let language =
    let open CCResult in
    Pool_context.find req
    >|= (fun { Pool_context.language; _ } -> language)
    |> get_or ~default:Pool_common.Language.En
  in
  let%lwt result =
    let { Pool_context.database_label; _ } =
      Pool_context.find req |> CCResult.get_exn
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* query =
      let open CCResult in
      find_in_params urlencoded Pool_common.Message.Field.Query
      >>= Filter.query_of_string
      |> Lwt_result.lift
    in
    let%lwt key_list = Filter.all_keys database_label in
    let%lwt template_list =
      Filter.find_templates_of_query database_label query
    in
    let tags = Logger.req req in
    let events =
      let lift = Lwt_result.lift in
      let open CCFun.Infix in
      match action with
      | Experiment exp ->
        let open Cqrs_command.Experiment_command in
        (match exp.Experiment.filter with
         | None ->
           CreateFilter.handle ~tags exp key_list template_list query
           |> Lwt_result.lift
         | Some filter ->
           UpdateFilter.handle ~tags key_list template_list filter query
           |> Lwt_result.lift)
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
    events |>> handle
  in
  let open Pool_common.Message in
  let htmx_notification (status, message) =
    Page.Message.create
      ~attributes:
        Tyxml.Html.
          [ a_user_data "hx-swap-oob" "true"
          ; a_id Component.Filter.notification_id
          ]
      (CCOption.pure message)
      language
      ()
    |> CCList.pure
    |> HttpUtils.multi_html_to_plain_text_response ~status
    |> Lwt.return
  in
  match result with
  | Ok () ->
    let created path =
      HttpUtils.htmx_redirect
        path
        ~actions:[ Message.set ~success:[ Created Field.Filter ] ]
        ()
    in
    (match action with
     | Template None -> created "/admin/filter"
     | Experiment exp when CCOption.is_none exp.Experiment.filter ->
       created
         (Format.asprintf
            "/admin/experiments/%s/invitations"
            Experiment.(Id.value exp.id))
     | Experiment _ | Template _ ->
       (200, Collection.(set_success [ Updated Field.Filter ] empty))
       |> htmx_notification)
  | Error err ->
    (* HTMX will only swap the content if respose is 200 *)
    (200, Collection.(set_error [ err ] empty)) |> htmx_notification
;;

let handle_toggle_predicate_type action req =
  let open Utils.Lwt_result.Infix in
  let%lwt result =
    let* { Pool_context.language; database_label; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
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
           find_in_params urlencoded Pool_common.Message.Field.Query
           >|= Yojson.Safe.from_string
           >>= Filter.Human.of_yojson key_list
         in
         let* predicate_type =
           find_in_params urlencoded Pool_common.Message.Field.Predicate
         in
         Filter.toggle_predicate_type current predicate_type
    in
    let* identifier = find_identifier urlencoded |> Lwt_result.lift in
    let%lwt quey_experiments =
      query
      |> Filter.Human.all_query_experiments
      |> Experiment.multiple_search_results_by_id database_label
    in
    Component.Filter.(
      predicate_form
        language
        action
        key_list
        template_list
        templates_disabled
        quey_experiments
        (Some query)
        ~identifier
        ())
    |> Lwt_result.return
  in
  (match result with
   | Ok html -> html
   | Error err -> error_to_html err)
  |> CCList.pure
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;

let handle_toggle_key action req =
  let open Utils.Lwt_result.Infix in
  let%lwt result =
    let* { Pool_context.language; database_label; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* key =
      find_in_params urlencoded Pool_common.Message.Field.Key
      |> Lwt_result.lift
      >>= Filter.key_of_string database_label
    in
    Component.Filter.predicate_value_form language action [] ~key ()
    |> Lwt.return_ok
  in
  (match result with
   | Ok html -> html
   | Error err -> err |> error_to_html)
  |> CCList.pure
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;

let handle_add_predicate action req =
  let open Utils.Lwt_result.Infix in
  let%lwt result =
    let* { Pool_context.language; database_label; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
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
    Lwt_result.return [ filter_form; add_button ]
  in
  (match result with
   | Ok html -> html
   | Error err -> error_to_html err |> CCList.pure)
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;

let search_experiments action req =
  let open Utils.Lwt_result.Infix in
  let%lwt result =
    let* { Pool_context.database_label; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let query =
      find_in_params urlencoded Pool_common.Message.Field.Title
      |> CCResult.to_opt
    in
    let%lwt exclude =
      let current_experiment =
        match action with
        | Template _ -> None
        | Experiment experiment -> Some experiment.Experiment.id
      in
      Sihl.Web.Request.urlencoded_list "exclude[]" req
      ||> CCList.map Experiment.Id.of_string
      ||> CCList.cons_maybe current_experiment
    in
    let%lwt results =
      query
      |> CCOption.map_or
           ~default:(Lwt.return [])
           (Experiment.search database_label exclude)
    in
    Component.Filter.search_experiments_input ?value:query ~results action
    |> Lwt.return_ok
  in
  (match result with
   | Ok html -> html
   | Error err -> error_to_html err)
  |> CCList.pure
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;

let count_contacts req =
  let experiment_id =
    HttpUtils.find_id Experiment.Id.of_string Field.Experiment req
  in
  let%lwt result =
    let open Utils.Lwt_result.Infix in
    let* { Pool_context.language; database_label; _ } =
      req
      |> Pool_context.find
      |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
      |> Lwt_result.lift
    in
    Utils.Lwt_result.map_error Pool_common.(Utils.error_to_string language)
    @@ let* experiment = Experiment.find database_label experiment_id in
       let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
       let* query =
         let open CCResult in
         find_in_params urlencoded Pool_common.Message.Field.Query
         |> CCOption.of_result
         |> CCOption.map_or
              ~default:
                (Ok
                   (experiment.Experiment.filter
                   |> CCOption.map (fun filter -> filter.Filter.query)))
              (fun str -> str |> Filter.query_of_string >|= CCOption.pure)
         |> Lwt_result.lift
       in
       Filter.count_filtered_contacts
         database_label
         (experiment.Experiment.id |> Experiment.Id.to_common)
         query
  in
  let status, (json : Yojson.Safe.t) =
    match result with
    | Error str -> 400, `Assoc [ "message", `String str ]
    | Ok int -> 200, `Assoc [ "count", `Int int ]
  in
  HttpUtils.yojson_response ~status:(status |> Opium.Status.of_code) json
  |> Lwt.return
;;

module Create = struct
  let action = Template None
  let create_template = write action
  let add_predicate = handle_add_predicate action
  let toggle_predicate_type = handle_toggle_predicate_type action
  let toggle_key = handle_toggle_key action
  let search_experiments = search_experiments action
end

module Update = struct
  let handler fnc req =
    let open Utils.Lwt_result.Infix in
    let id = template_id req in
    req
    |> database_label_from_req
    >>= CCFun.flip Filter.find id
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
  let search_experiments = handler search_experiments
end

module Access : Helpers.AccessSig = struct
  module FilterCommand = Cqrs_command.Filter_command

  let filter_effects =
    Middleware.Guardian.id_effects Filter.Id.of_string Field.Filter
  ;;

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Filter.Id.value)
        ; `Read, `TargetEntity `Contact
        ])
    ]
    |> filter_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    [ FilterCommand.Update.effects ]
    |> filter_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let create =
    [ `Create, `TargetEntity `Filter ]
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let index =
    [ `Read, `TargetEntity `Filter ]
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let delete = Middleware.Guardian.denied
end
