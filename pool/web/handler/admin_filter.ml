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
    let events =
      let open Cqrs_command.Experiment_command.UpdateFilter in
      handle experiment filter |> Lwt_result.lift
    in
    let handle events =
      Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
    in
    events |>> handle
  in
  let open HttpUtils in
  (match result with
   | Ok () ->
     { message =
         Pool_common.(
           Utils.success_to_string language Message.(Created Field.Filter))
     ; success = true
     }
   | Error err ->
     { message = Pool_common.(Utils.error_to_string language err)
     ; success = false
     })
  |> yojson_of_json_response
  |> yojson_to_json_response
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
           >>= Filter.filter_of_string
           >|= Filter.t_to_human key_list
         in
         let* predicate_type =
           find_in_params urlencoded Pool_common.Message.Field.Predicate
         in
         Filter.toggle_predicate_type current predicate_type
    in
    let* identifier =
      let open CCResult in
      find_in_params urlencoded Pool_common.Message.Field.Id
      >>= (fun str ->
            str
            |> CCString.split ~by:"-"
            |> fun str ->
            try Ok (CCList.map CCInt.of_string_exn str) with
            | _ -> Error Pool_common.Message.(Invalid Field.Id))
      |> Lwt_result.lift
    in
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
    Component.Filter.key_select language ~key () |> Lwt.return_ok
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
