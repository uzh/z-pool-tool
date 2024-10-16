open Entity

let t =
  let open Database.Caqti_encoders in
  let decode (id, (code, (count, ()))) = Ok { id; code; count } in
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom ~encode ~decode Caqti_type.[ string; string; int ]
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_signup_codes (
      uuid,
      code,
      count
    ) VALUES (
      UNHEX(REPLACE(UUID(), '-', '')),
      $1,
      1
    ) ON DUPLICATE KEY UPDATE
        count = count + 1
  |sql}
  |> Caqti_type.string ->. Caqti_type.unit
;;

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_signup_codes.uuid"
  ; "pool_signup_codes.code"
  ; "pool_signup_codes.count"
  ]
;;

let insert pool = Database.exec pool insert_request

let find_request_sql ?(count = false) where_fragment =
  let columns =
    if count
    then "COUNT( pool_signup_codes.uuid )"
    else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf
    {sql|SELECT %s FROM pool_signup_codes %s |sql}
    columns
    where_fragment
;;

let all ?query pool =
  Query.collect_and_count pool query ~select:find_request_sql t
;;
