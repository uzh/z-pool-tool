open CCFun
open Migrations
open Utils.Lwt_result.Infix

let src = Logs.Src.create "Database.Pool.Tenant"

module Logs = (val Logs.src_log src : Logs.LOG)

let sort = CCList.stable_sort (fun a b -> CCString.compare (fst a) (fst b))

let steps =
  let sorted =
    Migration_authorization.migration ()
    @ [ Migration_202301010000.migration ()
      ; Migration_202303211734.migration ()
      ; Migration_202303230956.migration ()
      ; Migration_202303291025.migration ()
      ; Migration_202305151556.migration ()
      ; Migration_202306021512.migration ()
      ; Migration_202306071352.migration ()
      ; Migration_202306081615.migration ()
      ; Migration_202306261642.migration ()
      ; Migration_202307081414.migration ()
      ; Migration_202307121006.migration ()
      ; Migration_202307121330.migration ()
      ; Migration_202307121722.migration ()
      ; Migration_202307131619.migration ()
      ; Migration_202307250839.migration ()
      ; Migration_202307281144.migration ()
      ; Migration_202308021016.migration ()
      ; Migration_202308030850.migration ()
      ; Migration_202308041330.migration ()
      ; Migration_202308210946.migration ()
      ; Migration_202309081700.migration ()
      ; Migration_202309121645.migration ()
      ; Migration_202309180820.migration ()
      ; Migration_202309190858.migration ()
      ; Migration_202309211305.migration ()
      ; Migration_202309251059.migration ()
      ; Migration_202309261013.migration ()
      ; Migration_202309281435.migration ()
      ; Migration_202310110929.migration ()
      ; Migration_202310191345.migration ()
      ; Migration_202311011626.migration ()
      ; Migration_202312040852.migration ()
      ; Migration_202312121047.migration ()
      ; Migration_202312150920.migration ()
      ; Migration_202312200952.migration ()
      ; Migration_202312201325.migration ()
      ; Migration_202401081554.migration ()
      ; Migration_202401120946.migration ()
      ; Migration_202401191048.migration ()
      ; Migration_202401221730.migration ()
      ; Migration_202402081615.migration ()
      ; Migration_202402091434.migration ()
      ; Migration_202402150942.migration ()
      ; Migration_202402161523.migration ()
      ; Migration_202402201330.migration ()
      ; Migration_202403131128.migration ()
      ; Migration_202403181624.migration ()
      ; Migration_202403221034.migration ()
      ; Migration_202403251311.migration ()
      ; Migration_202403281032.migration ()
      ; Migration_202403281435.migration ()
      ; Migration_202404101112.migration ()
      ; Migration_202406051700.migration ()
      ; Migration_202406241055.migration ()
      ; Migration_202407081355.migration ()
      ; Migration_202407151050.migration ()
      ; Migration_202407171415.migration ()
      ; Migration_202408081359.migration ()
      ; Migration_202410071409.migration ()
      ; Migration_202410161017.migration ()
      ; Migration_202411011201.migration ()
      ; Migration_202412131612.migration ()
      ; Migration_202412170838.migration ()
      ; Migration_202501091612.migration ()
      ; Migration_202502041812.migration ()
      ; Migration_202502100919.migration ()
      ; Migration_202502101715.migration ()
      ; Migration_202502271318.migration ()
      ]
    |> sort
  in
  [ Migration_person.migration ()
  ; Migration_contact.migration ()
  ; Migration_email_address.migration ()
  ; Migration_settings.migration ()
  ; Migration_i18n.migration ()
  ; Migration_assignment.migration ()
  ; Migration_session.migration ()
  ; Migration_invitation.migration ()
  ; Migration_experiment.migration ()
  ; Migration_waiting_list.migration ()
  ; Migration_location.migration ()
  ; Migration_location_file_mapping.migration ()
  ; Migration_mailing.migration ()
  ; Migration_filter.migration ()
  ; Migration_custom_fields.migration ()
  ; Migration_custom_field_answers.migration ()
  ; Migration_custom_field_options.migration ()
  ; Migration_custom_field_groups.migration ()
  ; Migration_custom_field_answer_versions.migration ()
  ; Migration_message_templates.migration ()
  ; Migration_tenant.migration_tenant ()
  ]
  @ sorted
  |> Database.Migration.extend_migrations
;;

let check_migration_status pool () =
  let open Utils.Lwt_result.Infix in
  let open Database in
  let migrations = steps () in
  let%lwt () =
    let%lwt up_to_date =
      Migration.pending_migrations pool ~migrations () ||> CCList.is_empty
    in
    Pool.Tenant.update_status
      pool
      Status.(if up_to_date then Active else MigrationsPending)
  in
  Migration.check_migrations_status pool ~migrations ()
;;

let report err =
  let open Database in
  let find_database_label text =
    let open Re in
    let regex = seq [ char '<'; group (rep1 any); str ">:" ] |> compile in
    let substring =
      try Some (exec regex text) with
      | Not_found -> None
    in
    CCOption.bind substring (CCFun.flip Group.get_opt 1)
  in
  let%lwt (_ : (unit, Pool_message.Error.t) result) =
    Lwt_result.map_error Pool_common.Utils.with_log_error
    @@
    let printed_error = Printexc.to_string err in
    match find_database_label printed_error with
    | Some database_url ->
      let* database_label =
        Database.Url.create database_url
        |> Lwt_result.lift
        >== Pool.Tenant.find_label_by_url
      in
      let tags = Database.Logger.Tags.create database_label in
      Logs.err (fun m -> m ~tags "%s" printed_error);
      Pool.Tenant.update_status database_label Status.ConnectionIssue >|> Lwt.return_ok
    | None ->
      Logs.err (fun m -> m "%s" printed_error);
      Lwt.return_ok ()
  in
  Lwt.return_unit
;;

let start () =
  let open Database in
  let db_pools = Pool.Tenant.all () in
  Lwt_list.iter_p (check_migration_status %> flip Lwt.catch report) db_pools
;;

let stop () = Lwt.return_unit
