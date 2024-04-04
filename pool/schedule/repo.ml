open Entity

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
    Ok (m.label, (time, (span, (m.status, m.last_run))))
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
            (option ScheduledTime.t)
            (t2 (option ScheduledTimeSpan.t) (t2 Status.t (option LastRunAt.t))))))
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
  let select_fragment fragment =
    Format.sprintf
      {sql|
      SELECT
        label,
        scheduled_time,
        scheduled_time_span,
        status,
        last_run_at
      FROM pool_schedules
      %s
    |sql}
      fragment
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    let order_by = {sql|
      ORDER BY last_run_at DESC
    |sql} in
    select_fragment order_by |> Caqti_type.unit ->* public
  ;;

  let find_all pool = Database.collect pool find_all_request

  let upsert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_schedules (
        label,
        scheduled_time,
        scheduled_time_span,
        status,
        last_run_at
      ) VALUES (
        ?,
        ?,
        ?,
        ?,
        ?
      ) ON DUPLICATE KEY UPDATE
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

  let select_count where_fragment =
    Format.asprintf
      {sql|
        SELECT COUNT(*)
        FROM pool_schedules
        %s
      |sql}
      where_fragment
  ;;

  let find_by pool query =
    Query.collect_and_count
      pool
      (Some query)
      ~select:(fun ?(count = false) fragment ->
        if count then select_count fragment else select_fragment fragment)
      public
  ;;
end

let find_all = Sql.find_all Database.root
let find_by = Sql.find_by Database.root
let upsert = Sql.upsert Database.root
let stop_all_active () = Sql.stop_all_active Database.root
