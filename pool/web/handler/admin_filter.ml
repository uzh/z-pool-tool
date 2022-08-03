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
  let result _ =
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* _ =
         Sihl.Web.Request.to_json req
         ||> CCOption.to_result Pool_common.Message.(Invalid Field.Filter)
       in
       (* Logs.info (fun m -> m "Filter: %s" (Yojson.Safe.pretty_to_string
          json_filter)); *)
       let () = Filter.json_to_filter () in
       let%lwt response =
         Http_utils.redirect_to_with_actions redirect_path []
       in
       Ok response |> Lwt_result.lift
  in
  result |> HttpUtils.extract_happy_path req
;;
