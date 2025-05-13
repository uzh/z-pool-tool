open Caqti_request.Infix
open Entity

module Repo_entity = struct
  open Database.Caqti_encoders
  open Pool_common.Repo
  module Channel = Model.SelectorType (Channel)

  let t =
    let decode (id, (user_uuid, (channel, (token, ())))) =
      Ok { id; user_uuid; channel; token }
    in
    let encode m : ('a Data.t, string) result =
      Ok Data.[ m.id; m.user_uuid; m.channel; m.token ]
    in
    let open Schema in
    custom ~encode ~decode Caqti_type.[ Id.t; Pool_user.Repo.Id.t; Channel.t; string ]
  ;;
end

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_authentication.uuid"
  ; Entity.Id.sql_select_fragment ~field:"pool_authentication.user_uuid"
  ; "pool_authentication.channel"
  ; "pool_authentication.token"
  ]
;;

let insert_request =
  {sql|
    INSERT INTO pool_authentication (
      uuid,
      user_uuid,
      channel,
      token,
      valid_until
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', '')),
      $3,
      $4,
      NOW() + INTERVAL 5 MINUTE
    ) ON DUPLICATE KEY UPDATE 
      uuid = UNHEX(REPLACE($1, '-', '')),
      channel = $3,
      token = $4,
      valid_until = NOW() + INTERVAL 5 MINUTE
  |sql}
  |> Repo_entity.t ->. Caqti_type.unit
;;

let insert pool = Database.exec pool insert_request

let find_valid_by_id_request =
  Format.asprintf
    {sql|
      SELECT
        %s
      FROM pool_authentication
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
      AND valid_until > NOW()
    |sql}
    (CCString.concat ", " sql_select_columns)
  |> Pool_common.Repo.Id.t ->? Repo_entity.t
;;

let find_valid_by_id pool id =
  let open Utils.Lwt_result.Infix in
  let* auth =
    Database.find_opt pool find_valid_by_id_request id
    ||> CCOption.to_result Pool_message.(Error.Invalid Field.Token)
  in
  let* user = Pool_user.find pool auth.user_uuid in
  Lwt_result.return (auth, user)
;;

let delete_request =
  {sql|
    DELETE FROM pool_authentication
    WHERE uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> Pool_common.Repo.Id.t ->. Caqti_type.unit
;;

let delete pool { id; _ } = Database.exec pool delete_request id
