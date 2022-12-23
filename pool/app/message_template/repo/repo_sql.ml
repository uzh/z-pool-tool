module RepoEntity = Repo_entity
module Database = Pool_database
open Entity

let select_sql =
  {sql|
    SELECT
      LOWER(CONCAT(
        SUBSTR(HEX(pool_message_templates.uuid), 1, 8), '-',
        SUBSTR(HEX(pool_message_templates.uuid), 9, 4), '-',
        SUBSTR(HEX(pool_message_templates.uuid), 13, 4), '-',
        SUBSTR(HEX(pool_message_templates.uuid), 17, 4), '-',
        SUBSTR(HEX(pool_message_templates.uuid), 21)
      )),
      pool_message_templates.label,
      LOWER(CONCAT(
        SUBSTR(HEX(pool_message_templates.entity_uuid), 1, 8), '-',
        SUBSTR(HEX(pool_message_templates.entity_uuid), 9, 4), '-',
        SUBSTR(HEX(pool_message_templates.entity_uuid), 13, 4), '-',
        SUBSTR(HEX(pool_message_templates.entity_uuid), 17, 4), '-',
        SUBSTR(HEX(pool_message_templates.entity_uuid), 21)
      )),
      pool_message_templates.language,
      pool_message_templates.email_subject,
      pool_message_templates.email_text_html,
      pool_message_templates.sms_text
    FROM
      pool_message_templates
  |sql}
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO pool_message_templates (
        uuid,
        label,
        entity_uuid,
        language,
        email_subject,
        email_text_html,
        sms_text
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        UNHEX(REPLACE($3, '-', '')),
        $4,
        $5,
        $6,
        $7
      )
    |sql}
  |> RepoEntity.t ->. Caqti_type.unit
;;

let insert pool = Utils.Database.exec (Database.Label.value pool) insert_request

let update_request =
  let open Caqti_request.Infix in
  {sql|
      UPDATE
        pool_message_templates
      SET
        email_subject = $5,
        email_text_html = $6,
        sms_text = $7
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> RepoEntity.t ->. Caqti_type.unit
;;

let update pool t =
  Utils.Database.exec (Database.Label.value pool) update_request t
;;

let find_default_by_label_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {|
  %s
  WHERE pool_message_templates.label = ?
  AND pool_message_templates.language = ?
  AND pool_message_templates.entity_uuid IS NULL
  |}
    select_sql
  |> Caqti_type.(tup2 string string) ->! RepoEntity.t
;;

let find_by_label_and_entity_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {|
  %s
  WHERE pool_message_templates.label = ?
  AND pool_message_templates.language = ?
  AND pool_message_templates.entity_uuid = UNHEX(REPLACE(?, '-', ''))
    |}
    select_sql
  |> Caqti_type.(tup3 string string string) ->! RepoEntity.t
;;

let find_by_label pool ?entity_uuid language label =
  let open Utils.Lwt_result.Infix in
  let find_opt = Utils.Database.find_opt in
  let db_label = Database.Label.value pool in
  let map_err =
    CCOption.to_result Pool_common.Message.(NotFound Field.Template)
  in
  let find_default () =
    find_opt
      db_label
      find_default_by_label_request
      (Label.show label, Pool_common.Language.show language)
  in
  match entity_uuid with
  | None -> find_default () ||> map_err
  | Some entity_uuid ->
    (* TODO: Merge queries, ev COALESCE *)
    find_opt
      db_label
      find_by_label_and_entity_request
      ( Label.show label
      , Pool_common.Language.show language
      , Pool_common.Id.value entity_uuid )
    >|> CCOption.map_or ~default:(find_default ()) Lwt.return_some
    ||> map_err
;;

let find_all_default_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {|
  %s
  WHERE pool_message_templates.entity_uuid IS NULL
  ORDER BY pool_message_templates.label
    |}
    select_sql
  |> Caqti_type.unit ->* RepoEntity.t
;;

let all_default pool =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_all_default_request
;;

let find_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {|
  %s
  WHERE pool_message_templates.uuid = UNHEX(REPLACE(?, '-', ''))
|}
    select_sql
  |> Caqti_type.string ->! RepoEntity.t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_request
    (Pool_common.Id.value id)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.MessageTemplate)
;;
