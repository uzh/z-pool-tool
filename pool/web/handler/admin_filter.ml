module HttpUtils = Http_utils
module Message = HttpUtils.Message

let print_urlencoded urlencoded =
  CCList.map
    (fun (k, v) -> Logs.info (fun m -> m "%s:%s" k (CCString.concat ", " v)))
    urlencoded
;;

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
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* _ =
         Sihl.Web.Request.to_json req
         ||> CCOption.to_result Pool_common.Message.(Invalid Field.Filter)
       in
       let* experiment = Experiment.find tenant_db experiment_id in
       let* filter = Filter.json_to_filter () |> Lwt_result.lift in
       let events =
         let open Cqrs_command.Experiment_command.AddFilter in
         handle experiment filter |> Lwt_result.lift
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         let headers =
           Opium.Headers.of_list [ "Content-Type", "text/html; charset=utf-8" ]
         in
         "Filter saved"
         |> Sihl.Web.Response.of_plain_text ~headers
         |> Lwt.return
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let toggle_predicate_type req =
  let open Lwt_result.Syntax in
  let%lwt result =
    let* { Pool_context.language; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* predicate_type =
      let open CCResult in
      find_in_params urlencoded Pool_common.Message.Field.Predicate
      >>= Filter.Utils.label_of_string
      |> Lwt_result.lift
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
    Component.Filter.(filter_form language (New predicate_type) ~identifier ())
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
    let* { Pool_context.language; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* key =
      let open CCResult in
      let open Filter in
      find_in_params urlencoded Pool_common.Message.Field.Key
      >>= Key.read
      |> Lwt_result.lift
    in
    Component.Filter.predicate_toggled language key () |> Lwt.return_ok
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
