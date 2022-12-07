module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let templates_disabled_key = Component.Filter.templates_disabled_key

let templates_disabled urlencoded =
  let open CCOption in
  CCList.assoc_opt ~eq:CCString.equal templates_disabled_key urlencoded
  >>= CCList.head_opt
  |> map_or ~default:false (CCString.equal "true")
;;

let find_all_templates tenant_db templates_disabled =
  if templates_disabled
  then Lwt.return []
  else Filter.find_all_templates tenant_db ()
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
  let result ({ Pool_context.tenant_db; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let%lwt filter_list = Filter.find_all_templates tenant_db () in
       Page.Admin.Filter.index context filter_list
       |> create_layout ~active_navigation:"/admin/filter" req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let form is_edit req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, Format.asprintf "/admin/filter")
    @@ let* filter =
         if is_edit
         then
           get_id req Field.Filter Pool_common.Id.of_string
           |> Filter.find_template tenant_db
           >|+ CCOption.pure
         else Lwt.return_none |> Lwt_result.ok
       in
       let%lwt key_list = Filter.all_keys tenant_db in
       Page.Admin.Filter.edit context filter key_list
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let edit = form true
let new_form = form false

let create ?model req =
  let open Utils.Lwt_result.Infix in
  let language =
    let open CCResult in
    Pool_context.find req
    >|= (fun { Pool_context.language; _ } -> language)
    |> get_or ~default:Pool_common.Language.En
  in
  let%lwt result =
    let { Pool_context.tenant_db; _ } =
      Pool_context.find req |> CCResult.get_exn
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* query =
      let open CCResult in
      find_in_params urlencoded Pool_common.Message.Field.Query
      >>= Filter.query_of_string
      |> Lwt_result.lift
    in
    let%lwt key_list = Filter.all_keys tenant_db in
    let%lwt template_list = Filter.find_templates_of_query tenant_db query in
    let tags = Logger.req req in
    let events =
      let open Pool_common.Message in
      let open HttpUtils in
      let lift = Lwt_result.lift in
      let open CCFun.Infix in
      match model with
      | Some `Experiment ->
        let experiment_id =
          get_field_router_param req Field.Experiment
          |> Pool_common.Id.of_string
        in
        let* experiment = Experiment.find tenant_db experiment_id in
        let open Cqrs_command.Experiment_command.UpdateFilter in
        handle ~tags experiment key_list template_list query |> Lwt_result.lift
      | Some `Filter ->
        let filter_id =
          get_field_router_param req Field.Filter |> Pool_common.Id.of_string
        in
        let* filter = Filter.find_template tenant_db filter_id in
        let open Cqrs_command.Filter_command.Update in
        urlencoded
        |> decode
        |> lift
        >>= handle ~tags key_list template_list filter query %> lift
      | None ->
        let open Cqrs_command.Filter_command.Create in
        urlencoded
        |> decode
        |> lift
        >>= handle ~tags key_list template_list query %> lift
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event ~tags tenant_db) events
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
    (match model with
     | None ->
       HttpUtils.htmx_redirect
         "/admin/filter"
         ~actions:[ Message.set ~success:[ Created Field.Filter ] ]
         ()
     | Some _ ->
       (200, Collection.(set_success [ Updated Field.Filter ] empty))
       |> htmx_notification)
  | Error err ->
    (400, Collection.(set_error [ err ] empty)) |> htmx_notification
;;

let create_for_experiment req = create ~model:`Experiment req
let create_template req = create req
let update_template req = create ~model:`Filter req

let toggle_predicate_type req =
  let open Utils.Lwt_result.Infix in
  let%lwt result =
    let* { Pool_context.language; tenant_db; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let templates_disabled = templates_disabled urlencoded in
    let%lwt key_list = Filter.all_keys tenant_db in
    let%lwt template_list = find_all_templates tenant_db templates_disabled in
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
    Component.Filter.(
      predicate_form
        language
        key_list
        template_list
        templates_disabled
        (Some query)
        ~identifier
        ())
    |> Lwt_result.return
  in
  (match result with
   | Ok html -> html
   | Error err ->
     err
     |> Pool_common.(Utils.error_to_string Pool_common.Language.En)
     |> Tyxml.Html.txt
     |> CCList.pure
     |> Tyxml.Html.div)
  |> CCList.pure
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;

let toggle_key req =
  let open Utils.Lwt_result.Infix in
  let%lwt result =
    let* { Pool_context.language; tenant_db; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* key =
      find_in_params urlencoded Pool_common.Message.Field.Key
      |> Lwt_result.lift
      >>= Filter.key_of_string tenant_db
    in
    Component.Filter.predicate_value_form language ~key () |> Lwt.return_ok
  in
  (match result with
   | Ok html -> html
   | Error err ->
     err
     |> Pool_common.(Utils.error_to_string Pool_common.Language.En)
     |> Tyxml.Html.txt
     |> CCList.pure
     |> Tyxml.Html.div)
  |> CCList.pure
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;

let add_predicate req =
  let open Utils.Lwt_result.Infix in
  let%lwt result =
    let* { Pool_context.language; tenant_db; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let templates_disabled = templates_disabled urlencoded in
    let%lwt key_list = Filter.all_keys tenant_db in
    let%lwt template_list = find_all_templates tenant_db templates_disabled in
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
        key_list
        template_list
        templates_disabled
        query
        ~identifier
        ()
    in
    let add_button =
      Component.Filter.add_predicate_btn
        (increment_identifier identifier)
        templates_disabled
    in
    Lwt_result.return [ filter_form; add_button ]
  in
  (match result with
   | Ok html -> html
   | Error err ->
     err
     |> Pool_common.(Utils.error_to_string Pool_common.Language.En)
     |> Tyxml.Html.txt
     |> CCList.pure
     |> Tyxml.Html.div
     |> CCList.pure)
  |> HttpUtils.multi_html_to_plain_text_response
  |> Lwt.return
;;

let count_contacts req =
  let experiment_id =
    let open Pool_common.Message.Field in
    let open HttpUtils in
    get_field_router_param req Experiment |> Pool_common.Id.of_string
  in
  let%lwt result =
    let open Utils.Lwt_result.Infix in
    let* { Pool_context.language; tenant_db; _ } =
      req
      |> Pool_context.find
      |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
      |> Lwt_result.lift
    in
    Utils.Lwt_result.map_error Pool_common.(Utils.error_to_string language)
    @@ let* experiment = Experiment.find tenant_db experiment_id in
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
       Contact.count_filtered tenant_db experiment.Experiment.id query
  in
  let status, (json : Yojson.Safe.t) =
    match result with
    | Error str -> 400, `Assoc [ "message", `String str ]
    | Ok int -> 200, `Assoc [ "count", `Int int ]
  in
  HttpUtils.yojson_response ~status:(status |> Opium.Status.of_code) json
  |> Lwt.return
;;
