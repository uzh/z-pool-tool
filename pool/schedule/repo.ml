open Entity

module Label = struct
  include Label

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module LastRun = struct
  include LastRun

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

let t =
  let encode (m : t) =
    let span, time =
      match m.scheduled_time with
      | Every span -> Some span, None
      | At time -> None, Some time
    in
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
               (tup2 Status.t (option LastRun.t))))))
;;

module Sql = struct
  let upsert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_schedule (
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
        scheduled_time_span = VALUES(scheduled_time_span)
        status = VALUES(status),
    |sql}
    |> t ->. Caqti_type.unit
  ;;

  let upsert pool =
    Utils.Database.exec (Pool_database.Label.value pool) upsert_request
  ;;
end

let upsert = Sql.upsert Pool_database.root
