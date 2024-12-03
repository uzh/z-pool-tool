open Entity
module Dynparam = Database.Dynparam

module Label = struct
  include Label

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module LastRunAt = struct
  include LastRunAt

  let t = Caqti_type.ptime
end

module ScheduledTime = struct
  include ScheduledTime

  let t = Caqti_type.ptime
end

module ScheduledTimeSpan = struct
  include ScheduledTimeSpan

  let t = Caqti_type.ptime_span
end

module Status = Pool_common.Repo.Model.SelectorType (Entity.Status)

let encode_time_span = function
  | Every span -> Some span, None
  | At time -> None, Some time
;;

let t =
  let encode (m : t) =
    let span, time = encode_time_span m.scheduled_time in
    Ok (m.label, (m.database_label, (time, (span, (m.status, m.last_run)))))
  in
  let decode _ =
    Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Label.t
         (t2
            (option Database.Repo.Label.t)
            (t2
               (option ScheduledTime.t)
               (t2
                  (option ScheduledTimeSpan.t)
                  (t2 Status.t (option LastRunAt.t)))))))
;;

let public =
  let encode (m : public) =
    let span, time = encode_time_span m.scheduled_time in
    Ok (m.label, (time, (span, (m.status, m.last_run))))
  in
  let decode (label, (time, (span, (status, last_run)))) =
    let open CCResult in
    let* scheduled_time =
      match time, span with
      | Some time, None -> At time |> return
      | None, Some span -> Every span |> return
      | _ ->
        Error
          (Pool_message.(Error.Decode Field.ScheduledTime)
           |> Pool_common.(Utils.error_to_string Language.En))
    in
    Ok { label; scheduled_time; status; last_run }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Label.t
         (t2
            (option ScheduledTime.t)
            (t2 (option ScheduledTimeSpan.t) (t2 Status.t (option LastRunAt.t))))))
;;

module Sql = struct
  let sql_select_public_columns =
    [ "pool_schedules.label"
    ; "pool_schedules.scheduled_time"
    ; "pool_schedules.scheduled_time_span"
    ; "pool_schedules.status"
    ; "pool_schedules.last_run_at"
    ]
  ;;

  let select_public_fragment ?(count = false) where_fragment =
    let columns =
      if count
      then "COUNT(*)"
      else CCString.concat ", " sql_select_public_columns
    in
    Format.sprintf
      {sql|
        SELECT %s FROM pool_schedules %s
      |sql}
      columns
      where_fragment
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    let order_by =
      {sql|
      ORDER BY last_run_at DESC
    |sql}
    in
    select_public_fragment order_by |> Caqti_type.unit ->* public
  ;;

  let find_all pool = Database.collect pool find_all_request

  let upsert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_schedules (
        label,
        database_label,
        scheduled_time,
        scheduled_time_span,
        status,
        last_run_at
      ) VALUES (
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      ) ON DUPLICATE KEY UPDATE
        database_label = VALUES(database_label),
        scheduled_time = VALUES(scheduled_time),
        scheduled_time_span = VALUES(scheduled_time_span),
        status = VALUES(status),
        last_run_at = VALUES(last_run_at)
    |sql}
    |> t ->. Caqti_type.unit
  ;;

  let upsert pool = Database.exec pool upsert_request

  let change_all_status_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_schedules
      SET
        status = $2
      WHERE
        status = $1
    |sql}
    |> Caqti_type.(t2 Status.t Status.t ->. unit)
  ;;

  let stop_all_active pool =
    Database.exec pool change_all_status_request Entity.Status.(Active, Stopped)
  ;;

  let find_by_db_label pool database_label query =
    let where =
      ( "database_label = ? OR database_label IS NULL"
      , Dynparam.(empty |> add Database.Repo.Label.t database_label) )
    in
    Query.collect_and_count
      pool
      (Some query)
      ~where
      ~select:select_public_fragment
      public
  ;;
end

let find_all = Sql.find_all Database.Pool.Root.label
let find_by_db_label = Sql.find_by_db_label Database.Pool.Root.label
let upsert = Sql.upsert Database.Pool.Root.label
let stop_all_active () = Sql.stop_all_active Database.Pool.Root.label
