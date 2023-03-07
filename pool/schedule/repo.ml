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
    failwith
      Pool_common.(Message.WriteOnlyModel |> Utils.error_to_string Language.En)
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Label.t
         (tup2
            (option ScheduledTime.t)
            (tup2
               (option ScheduledTimeSpan.t)
               (tup2 Status.t (option LastRunAt.t))))))
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
        let open Pool_common in
        Error
          (Message.(Decode Field.ScheduledTime)
           |> Utils.error_to_string Language.En)
    in
    Ok { label; scheduled_time; status; last_run }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Label.t
         (tup2
            (option ScheduledTime.t)
            (tup2
               (option ScheduledTimeSpan.t)
               (tup2 Status.t (option LastRunAt.t))))))
;;

module Sql = struct
  let find_all_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        label,
        scheduled_time,
        scheduled_time_span,
        status,
        last_run_at
      FROM pool_schedules
      ORDER BY last_run_at DESC
    |sql}
    |> Caqti_type.unit ->* public
  ;;

  let find_all pool =
    Utils.Database.collect (Pool_database.Label.value pool) find_all_request
  ;;

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

  let upsert pool =
    Utils.Database.exec (Pool_database.Label.value pool) upsert_request
  ;;

  let change_all_status_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_schedules
      SET
        status = $2
      WHERE
        status = $1
    |sql}
    |> Caqti_type.(tup2 Status.t Status.t ->. unit)
  ;;

  let stop_all_active pool =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      change_all_status_request
      Entity.Status.(Active, Stopped)
  ;;
end

let find_all = Sql.find_all Pool_database.root
let upsert = Sql.upsert Pool_database.root
let stop_all_active () = Sql.stop_all_active Pool_database.root
