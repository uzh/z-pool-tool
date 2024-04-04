module Dynparam = Database.Dynparam

let job = Repo_entity.sihl_queue_job_caqti

let sql_select_columns =
  [ Pool_common.Id.sql_select_fragment ~field:"queue_jobs.uuid"
  ; "queue_jobs.name"
  ; "queue_jobs.input"
  ; "queue_jobs.tries"
  ; "queue_jobs.next_run_at"
  ; "queue_jobs.max_tries"
  ; "queue_jobs.status"
  ; "queue_jobs.last_error"
  ; "queue_jobs.last_error_at"
  ; "queue_jobs.tag"
  ; "queue_jobs.ctx"
  ]
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE queue_jobs
      SET
        name = $2,
        input = $3,
        tries = $4,
        next_run_at = $5,
        max_tries = $6,
        status = $7,
        last_error = $8,
        last_error_at = $9,
        tag = $10,
        ctx = $11
      WHERE
        queue_jobs.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> job ->. Caqti_type.unit
;;

let update label job_instance = Database.exec label update_request job_instance

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf {sql|SELECT %s FROM queue_jobs %s|sql} columns where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql| SELECT %s FROM queue_jobs WHERE queue_jobs.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    (sql_select_columns |> CCString.concat ", ")
  |> Pool_common.Repo.Id.t ->? job
;;

let find label id =
  let open Utils.Lwt_result.Infix in
  print_endline "";
  Database.find_opt label find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Queue)
;;

let find_by ?query pool =
  Query.collect_and_count pool query ~select:find_request_sql job
;;

let find_workable_query ?(count = false) () =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ","
  in
  Format.asprintf
    {sql|
      SELECT %s FROM queue_jobs
      WHERE status = "pending"
        AND next_run_at <= NOW()
        AND tries < max_tries
      ORDER BY id ASC
    |sql}
    columns
;;

let find_workable_request =
  let open Caqti_request.Infix in
  find_workable_query () |> Caqti_type.unit ->* job
;;

let find_workable label = Database.collect label find_workable_request ()

let count_workable_request =
  let open Caqti_request.Infix in
  find_workable_query ~count:true () |> Caqti_type.(unit ->? int)
;;

let count_workable label =
  let open Utils.Lwt_result.Infix in
  Database.find_opt label count_workable_request ()
  ||> CCOption.to_result Pool_message.Error.NoValue
;;
