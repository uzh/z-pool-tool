open CCFun
open Ppx_yojson_conv_lib.Yojson_conv

module Status = struct
  include Entity.Status

  let t =
    Caqti_type.(
      custom
        ~encode:(yojson_of_t %> Yojson.Safe.to_string %> CCResult.return)
        ~decode:(Yojson.Safe.from_string %> t_of_yojson %> CCResult.return)
        string)
  ;;
end

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
  let encode m =
    let open Entity in
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
      { Entity.id
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
                           Status.t
                           (t2
                              (option string)
                              (t2
                                 (option ptime)
                                 (t2 (option string) (option ctx))))))))))))
;;

let write_caqti_type =
  let open Entity_history in
  let open Pool_common.Repo in
  let encode m =
    Ok
      ( Pool_common.Id.of_string m.job.Entity.id
      , m.entity_uuid
      , m.message_template )
  in
  let decode _ = failwith "Write only model" in
  Caqti_type.(custom ~encode ~decode (t3 Id.t Id.t (option string)))
;;

let t =
  let open Pool_common.Repo in
  let encode (_ : Entity_history.t) = failwith "Read only model" in
  let decode (entity_uuid, message_template, job) =
    Ok { Entity_history.entity_uuid; message_template; job }
  in
  Caqti_type.(custom ~encode ~decode (t3 Id.t (option string) job))
;;
