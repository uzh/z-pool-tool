module RepoEntity = Repo_entity
module Database = Database
open Caqti_request.Infix

let sql_select_columns =
  [ Pool_queue.Id.sql_select_fragment
      ~field:"pool_text_message_dlr.queue_job_uuid"
  ; "pool_text_message_dlr.raw"
  ; "pool_text_message_dlr.from"
  ; "pool_text_message_dlr.to"
  ; "pool_text_message_dlr.message_id"
  ; "pool_text_message_dlr.dlr_mask"
  ; "pool_text_message_dlr.error_code"
  ; "pool_text_message_dlr.error_message"
  ; "pool_text_message_dlr.submit_date"
  ; "pool_text_message_dlr.done_date"
  ; "pool_text_message_dlr.plmn"
  ; "pool_text_message_dlr.country"
  ; "pool_text_message_dlr.sms_cost"
  ]
;;

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "COUNT(*)" else CCString.concat ", " sql_select_columns
  in
  Format.asprintf
    {sql|SELECT %s FROM pool_text_message_dlr %s|sql}
    columns
    where_fragment
;;

let find_report_by_queue_id_request =
  let open Caqti_request.Infix in
  {sql|
      WHERE pool_text_message_dlr.queue_job_uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> find_request_sql
  |> Pool_queue.Repo.Id.t ->! RepoEntity.delivery_report
;;

let find_report_by_queue_id pool =
  Database.find_opt pool find_report_by_queue_id_request
;;

let insert_request =
  [%string
    {sql|
        INSERT INTO pool_text_message_dlr (
          queue_job_uuid,
          raw,
          `from`,
          `to`,
          message_id,
          dlr_mask,
          error_code,
          error_message,
          submit_date,
          done_date,
          plmn,
          country,
          sms_cost
        ) VALUES (
          %{Pool_model.Base.Id.sql_value_fragment "$1"},
          $2,
          $3,
          $4,
          $5,
          $6,
          $7,
          $8,
          $9,
          $10,
          $11,
          $12,
          $13
        )
      |sql}]
  |> RepoEntity.delivery_report ->. Caqti_type.unit
;;

let insert_report pool = Database.exec pool insert_request
