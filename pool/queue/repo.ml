open CCFun
open Ppx_yojson_conv_lib.Yojson_conv

let status =
  let open Sihl.Contract.Queue in
  let to_string = function
    | Pending -> "pending"
    | Succeeded -> "succeeded"
    | Failed -> "failed"
    | Cancelled -> "cancelled"
  in
  let of_string = function
    | "pending" -> Ok Pending
    | "succeeded" -> Ok Succeeded
    | "failed" -> Ok Failed
    | "cancelled" -> Ok Cancelled
    | str -> Error (Format.asprintf "Unexpected job status %s found" str)
  in
  let encode m = Ok (to_string m) in
  let decode = of_string in
  Caqti_type.(custom ~encode ~decode string)
;;

type ctx = (string * string) list [@@deriving yojson]

let ctx =
  let encode = yojson_of_ctx %> Yojson.Safe.to_string %> CCResult.return in
  let decode m =
    try Yojson.Safe.from_string m |> ctx_of_yojson |> CCResult.return with
    | _ -> Error (Format.sprintf "failed to decode ctx %s" m)
  in
  Caqti_type.(custom ~encode ~decode string)
;;

let job =
  let open Sihl.Contract.Queue in
  let encode m =
    Ok
      ( m.id
      , ( m.name
        , ( m.input
          , ( m.tries
            , ( m.next_run_at
              , ( m.max_tries
                , ( m.status
                  , (m.last_error, (m.last_error_at, (m.tag, Some m.ctx))) ) )
              ) ) ) ) )
  in
  let decode
    ( id
    , ( name
      , ( input
        , ( tries
          , ( next_run_at
            , (max_tries, (status, (last_error, (last_error_at, (tag, ctx)))))
            ) ) ) ) )
    =
    Ok
      { id
      ; name
      ; input
      ; tries
      ; next_run_at
      ; max_tries
      ; status
      ; last_error
      ; last_error_at
      ; tag
      ; ctx = Option.value ~default:[] ctx
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         string
         (t2
            string
            (t2
               string
               (t2
                  int
                  (t2
                     ptime
                     (t2
                        int
                        (t2
                           status
                           (t2
                              (option string)
                              (t2
                                 (option ptime)
                                 (t2 (option string) (option ctx))))))))))))
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

let update label job_instance =
  Utils.Database.exec
    (Pool_database.Label.value label)
    update_request
    job_instance
;;

let select_from_fragment =
  {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(uuid), 1, 8), '-',
        SUBSTR(HEX(uuid), 9, 4), '-',
        SUBSTR(HEX(uuid), 13, 4), '-',
        SUBSTR(HEX(uuid), 17, 4), '-',
        SUBSTR(HEX(uuid), 21)
        )),
      name,
      input,
      tries,
      next_run_at,
      max_tries,
      status,
      last_error,
      last_error_at,
      tag,
      ctx
    FROM queue_jobs
  |sql}
;;

let find_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql| %s WHERE queue_jobs.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
    select_from_fragment
  |> Pool_common.Repo.Id.t ->? job
;;

let find label id =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt (Pool_database.Label.value label) find_request id
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Queue)
;;

let find_workable_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      %s
      WHERE
        status = "pending"
        AND next_run_at <= NOW()
        AND tries < max_tries
      ORDER BY id DESC
    |sql}
    select_from_fragment
  |> Caqti_type.unit ->* job
;;

let find_workable label =
  Utils.Database.collect
    (Pool_database.Label.value label)
    find_workable_request
    ()
;;
