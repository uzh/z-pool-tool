open Entity

let t =
  let open Database.Caqti_encoders in
  let decode
        ( id
        , ( code
          , ( signup_count
            , ( verification_count
              , (signed_up_webhook_url, (email_verified_webhook_url, ())) ) ) ) )
    =
    Ok
      { id
      ; code
      ; signup_count
      ; verification_count
      ; signed_up_webhook_url
      ; email_verified_webhook_url
      }
  in
  let encode m : ('a Data.t, string) result =
    Ok
      Data.
        [ m.id
        ; m.code
        ; m.signup_count
        ; m.verification_count
        ; m.signed_up_webhook_url
        ; m.email_verified_webhook_url
        ]
  in
  let open Schema in
  custom
    ~encode
    ~decode
    Caqti_type.[ string; string; int; int; option string; option string ]
;;

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_signup_codes.uuid"
  ; "pool_signup_codes.code"
  ; "pool_signup_codes.signup_count"
  ; "pool_signup_codes.verification_count"
  ; "pool_signup_codes.signed_up_webhook_url"
  ; "pool_signup_codes.email_verified_webhook_url"
  ]
;;

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count
    then "COUNT( pool_signup_codes.uuid )"
    else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf {sql|SELECT %s FROM pool_signup_codes %s |sql} columns where_fragment
;;

let find_request =
  let open Caqti_request.Infix in
  find_request_sql {sql| WHERE pool_signup_codes.uuid = UNHEX(REPLACE($1, '-', '')) |sql}
  |> Caqti_type.string ->! t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request (Id.value id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.SignUpCode)
;;

let find_by_code_request =
  let open Caqti_request.Infix in
  find_request_sql {sql| WHERE pool_signup_codes.code = $1 |sql}
  |> Caqti_type.string ->? t
;;

let find_by_code pool code = Database.find_opt pool find_by_code_request (Code.value code)

let find_webhook pool trigger ?payload code =
  let open Utils.Lwt_result.Infix in
  find_by_code pool code ||> CCFun.flip CCOption.bind (webhook trigger ?payload)
;;

let all ?query pool = Query.collect_and_count pool query ~select:find_request_sql t

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_signup_codes (
      uuid,
      code,
      signup_count,
      verification_count,
      signed_up_webhook_url,
      email_verified_webhook_url
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3,
      $4,
      $5,
      $6
    )
  |sql}
  |> t ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_signup_codes
    SET
      signed_up_webhook_url = $2,
      email_verified_webhook_url = $3
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Caqti_type.(t3 string (option string) (option string) ->. unit)
;;

let update pool { id; signed_up_webhook_url; email_verified_webhook_url; _ } =
  Database.exec
    pool
    update_request
    (Id.value id, signed_up_webhook_url, email_verified_webhook_url)
;;

let increment_request column =
  let open Caqti_request.Infix in
  let column =
    match column with
    | `Signup -> "signup_count"
    | `Verification -> "verification_count"
  in
  [%string
    {sql|
      INSERT INTO pool_signup_codes (
        uuid,
        code,
        %{column}
      ) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        $1,
        1
      ) ON DUPLICATE KEY UPDATE
          %{column} = %{column} + 1
    |sql}]
  |> Caqti_type.string ->. Caqti_type.unit
;;

let increment pool column = Database.exec pool (increment_request column)
