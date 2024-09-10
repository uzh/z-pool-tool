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
    let create_entry ?database_label label value =
      ( CCOption.map_or
          ~default:label
          (Database.Label.value %> Format.asprintf "%s [%s]" label)
          database_label
      , value )
    in
    let%lwt databases = Pool_tenant.find_all () in
    let default = "" in
    let%lwt root_job_count =
      let label = Database.root in
      Pool_queue.count_all_workable label
      ||> CCResult.map_or CCInt.to_string ~default
      ||> fun count -> label, count
    in
    let%lwt tenant_job_count =
      Lwt_list.map_s
        (fun { Pool_tenant.database_label; status; _ } ->
          let open Database.Status in
          let%lwt count =
            match status with
            | Active ->
              Pool_queue.count_all_workable database_label
              >|+ CCInt.to_string
              ||> CCResult.to_opt
            | ConnectionIssue
            | Disabled
            | Maintenance
            | MigrationsFailed
            | MigrationsPending -> Lwt.return_none
          in
          Lwt.return (database_label, CCOption.get_or ~default count))
        databases
    in
    let%lwt schedules = find_all () in
    let schedules_of_database_label database_label =
      schedules
      |> CCList.filter_map (fun ({ label; _ } as schedule : public) ->
        let contains_label =
          let sub =
            database_label |> Database.Label.value |> Format.asprintf "[%s]"
          in
          CCString.mem ~sub (Schedule.Label.value label)
        in
        if contains_label then Some (is_ok schedule) else None)
      |> CCList.for_all id
    in
    let is_ok = CCList.(map is_ok %> for_all id) in
    let list_schedules =
      schedules
      |> CCList.map (fun ({ label; last_run; _ } : public) ->
        ( Label.value label
        , CCOption.map_or ~default:"unknown" LastRunAt.show last_run ))
    in
    let list_database_status =
      databases
      |> CCList.map (fun { Pool_tenant.database_label; status; _ } ->
        create_entry
          ~database_label
          Pool_message.Field.(Status |> show)
          (Database.Status.show status))
    in
    let list_job_counts =
      root_job_count :: tenant_job_count
      |> CCList.map (fun (database_label, count) ->
        create_entry ~database_label "job_count" count)
    in
    let list_ok =
      (databases
       |> CCList.map (fun { Pool_tenant.database_label; _ } ->
         create_entry
           ~database_label
           "ok"
           (schedules_of_database_label database_label |> Utils.Bool.to_string))
      )
      @ [ "ok", is_ok schedules |> Utils.Bool.to_string ]
    in
    list_schedules @ list_database_status @ list_job_counts @ list_ok
    |> CCList.map (fun (k, v) -> k, `String v)
    |> Lwt.return_ok
  in
  let sort = CCList.stable_sort (fun a b -> CCString.compare (fst a) (fst b)) in
  match result with
  | Ok result -> yojson_response (`Assoc (sort result))
  | Error _ -> Rock.Response.make ~status:`No_content () |> Lwt.return
;;

module Access : module type of Helpers.Access = struct
  include Helpers.Access
  module Guardian = Middleware.Guardian

  let index = Guardian.require_user_type_of Pool_context.UserType.[ Admin ]
end
