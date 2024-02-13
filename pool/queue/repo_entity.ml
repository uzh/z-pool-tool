open Entity_history
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

let sihl_queue_job_caqti =
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

let write_caqti_type =
  let open Pool_common.Repo in
  let encode m =
    Ok
      ( Pool_common.Id.of_string m.job.Sihl_queue.id
      , m.entity_uuid
      , m.message_template )
  in
  let decode _ = failwith "Write only model" in
  Caqti_type.(custom ~encode ~decode (t3 Id.t Id.t (option string)))
;;

let t =
  let open Pool_common.Repo in
  let encode (_ : t) = failwith "Read only model" in
  let decode (entity_uuid, message_template, job) =
    Ok { entity_uuid; message_template; job }
  in
  Caqti_type.(
    custom ~encode ~decode (t3 Id.t (option string) sihl_queue_job_caqti))
;;
