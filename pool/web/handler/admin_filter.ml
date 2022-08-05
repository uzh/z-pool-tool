module HttpUtils = Http_utils
module Message = HttpUtils.Message

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
       (* Logs.info (fun m -> m "Filter: %s" (Yojson.Safe.pretty_to_string
          json_filter)); *)
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
