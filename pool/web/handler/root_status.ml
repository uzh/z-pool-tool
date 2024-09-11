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
  let%lwt result =
    let open Schedule in
    let create_entry ?database_label label value =
      ( CCOption.map_or
          ~default:label
          CCFun.(Database.Label.value %> Format.asprintf "%s [%s]" label)
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
    let is_ok =
      let open CCFun in
      CCList.(map is_ok %> for_all id)
    in
    let schedules_of database_label =
      Schedule.find_by_db_label database_label (Query.empty ())
      ||> fst
      ||> is_ok
    in
    let%lwt list_schedules =
      let%lwt schedules = find_all () in
      create_entry
        "schedules"
        (`List (schedules |> CCList.map yojson_of_public))
      |> CCList.return
      |> Lwt.return
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
    let%lwt ok_tenants =
      databases
      |> Lwt_list.map_s (fun { Pool_tenant.database_label; _ } ->
        schedules_of database_label
        ||> Utils.Bool.to_string
        ||> create_entry ~database_label "ok")
    in
    let%lwt ok_root =
      schedules_of Database.root ||> Utils.Bool.to_string ||> create_entry "ok"
    in
    (ok_root :: ok_tenants) @ list_database_status @ list_job_counts
    |> CCList.map (fun (k, v) -> k, `String v)
    |> CCList.append list_schedules
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
