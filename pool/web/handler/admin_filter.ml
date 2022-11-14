module HttpUtils = Http_utils
module Message = HttpUtils.Message

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
  find_in_params urlencoded Pool_common.Message.Field.Id
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

let create req =
  let open Lwt_result.Syntax in
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    let open Pool_common.Message.Field in
    let open HttpUtils in
    get_field_router_param req Experiment |> Pool_common.Id.of_string
  in
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
    let* experiment = Experiment.find tenant_db experiment_id in
    let* filter =
      let open CCResult in
      find_in_params urlencoded Pool_common.Message.Field.Filter
      >>= Filter.filter_of_string
      |> Lwt_result.lift
    in
    let%lwt key_list = Filter.all_keys tenant_db in
    let events =
      let open Cqrs_command.Experiment_command.UpdateFilter in
      handle experiment key_list filter |> Lwt_result.lift
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
    in
    events |>> handle
  in
  let open Pool_common.Message in
  (match result with
   | Ok () -> 200, Collection.(set_success [ Created Field.Filter ] empty)
   | Error err -> 400, Collection.(set_error [ err ] empty))
  |> fun (status, message) ->
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
;;

let toggle_predicate_type req =
  let open Lwt_result.Syntax in
  let%lwt result =
    let* { Pool_context.language; tenant_db; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let%lwt key_list = Filter.all_keys tenant_db in
    let* filter =
      let open CCResult in
      Lwt_result.lift
      @@ let* current =
           find_in_params urlencoded Pool_common.Message.Field.Filter
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
      predicate_form language (Some filter) key_list ~identifier ())
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
  let open Lwt_result.Syntax in
  let%lwt result =
    let* { Pool_context.language; tenant_db; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* key =
      let open Lwt_result.Infix in
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
  let open Lwt_result.Syntax in
  let%lwt result =
    let* { Pool_context.language; tenant_db; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let%lwt key_list = Filter.all_keys tenant_db in
    let* identifier = find_identifier urlencoded |> Lwt_result.lift in
    let rec increment_identifier identifier =
      match identifier with
      | [] -> []
      | [ tl ] -> [ tl + 1 ]
      | hd :: tl -> hd :: increment_identifier tl
    in
    let predicate = Filter.Human.init () |> CCOption.pure in
    let filter_form =
      Component.Filter.predicate_form language predicate key_list ~identifier ()
    in
    let add_button =
      Component.Filter.add_predicate_btn (increment_identifier identifier)
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
    let open Lwt_result.Syntax in
    let* { Pool_context.language; tenant_db; _ } =
      req
      |> Pool_context.find
      |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
      |> Lwt_result.lift
    in
    Lwt_result.map_error Pool_common.(Utils.error_to_string language)
    @@ let* experiment = Experiment.find tenant_db experiment_id in
       let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
       let* filter =
         let open CCResult in
         find_in_params urlencoded Pool_common.Message.Field.Filter
         |> CCOption.of_result
         |> CCOption.map_or
              ~default:
                (Ok
                   (experiment.Experiment.filter
                   |> CCOption.map (fun filter -> filter.Filter.filter)))
              (fun str -> str |> Filter.filter_of_string >|= CCOption.pure)
         |> Lwt_result.lift
       in
       Contact.count_filtered tenant_db experiment.Experiment.id filter
  in
  let status, (json : Yojson.Safe.t) =
    match result with
    | Error str -> 400, `Assoc [ "message", `String str ]
    | Ok int -> 200, `Assoc [ "count", `Int int ]
  in
  HttpUtils.yojson_response ~status:(status |> Opium.Status.of_code) json
  |> Lwt.return
;;
