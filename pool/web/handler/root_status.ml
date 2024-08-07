open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.admin.settings_schedule"
let active_navigation = "/admin/settings/schedules"

let yojson_response ?status json =
  let headers = Opium.Headers.of_list [ "Content-Type", "application/json" ] in
  json |> Sihl.Web.Response.of_json ?status ~headers |> Lwt.return
;;

let show _ =
  let open CCFun in
  let%lwt result =
    let open Schedule in
    let%lwt databases =
      Pool_tenant.find_all ()
      ||> CCList.map (fun { Pool_tenant.database_label; _ } -> database_label)
      ||> CCList.cons Database.root
    in
    let* job_count =
      Lwt_list.fold_left_s
        (fun init label ->
          let* count =
            Lwt_list.map_s
              (flip Pool_queue.count_workable label)
              Pool_queue.JobName.all
            ||> CCResult.flatten_l
          in
          init |> Lwt_result.lift >|+ CCList.fold_right ( + ) count)
        (Ok 0)
        databases
    in
    let%lwt schedules = find_all () in
    let is_ok = CCList.(map is_ok %> for_all id) in
    (schedules
     |> CCList.map (fun ({ label; last_run; _ } : public) ->
       ( Label.value label
       , CCOption.map_or ~default:"unknown" LastRunAt.show last_run )))
    @ [ "job_count", CCInt.to_string job_count
      ; ("ok", if is_ok schedules then "true" else "false")
      ]
    |> CCList.map (fun (k, v) -> k, `String v)
    |> Lwt.return_ok
  in
  match result with
  | Ok result -> yojson_response (`Assoc result)
  | Error _ -> Rock.Response.make ~status:`No_content () |> Lwt.return
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index = Guardian.require_user_type_of Pool_context.UserType.[ Admin ]
end
