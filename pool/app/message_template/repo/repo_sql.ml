module Dynparam = Utils.Database.Dynparam
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
      pool_message_templates.email_text_plain,
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
        email_text_plain,
        sms_text
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?
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
        email_text_plain = $7,
        sms_text = $8
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> RepoEntity.t ->. Caqti_type.unit
;;

let update pool t =
  Utils.Database.exec (Database.Label.value pool) update_request t
;;

(* The template are prioritised according to the entity_uuids list, from left to
   right. If none are found, the default template will be returned. *)
let find_by_label pool ?entity_uuids language label =
  let open Utils.Lwt_result.Infix in
  let open Caqti_request.Infix in
  let where =
    Format.asprintf
      {sql|
    pool_message_templates.label = $1
    AND pool_message_templates.language = $2
    %s
    |sql}
  in
  let dyn =
    Dynparam.(
      empty
      |> add Caqti_type.string (Label.show label)
      |> add Caqti_type.string (Pool_common.Language.show language))
  in
  let where, dyn =
    match entity_uuids with
    | None | Some [] ->
      where {sql| AND pool_message_templates.entity_uuid IS NULL |sql}, dyn
    | Some entity_uuids ->
      let id_list, dyn =
        CCList.foldi
          (fun (id_list, dyn) i entity_uuid ->
            let index = i + 3 in
            ( Format.asprintf "UNHEX(REPLACE($%i, '-', ''))" index :: id_list
            , dyn
              |> Dynparam.add
                   Caqti_type.string
                   (Pool_common.Id.value entity_uuid) ))
          ([], dyn)
          entity_uuids
      in
      id_list
      |> CCString.concat ","
      |> fun ids ->
      ( Format.asprintf
          {sql| AND(
                  pool_message_templates.entity_uuid IS NULL
                  OR
                  pool_message_templates.entity_uuid IN (%s))
                ORDER BY
                FIELD(entity_uuid, %s) DESC
                LIMIT 1
              |sql}
          ids
          ids
        |> where
      , dyn )
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    Format.asprintf "%s WHERE %s" select_sql where |> pt ->! RepoEntity.t
  in
  Utils.Database.find_opt (pool |> Database.Label.value) request pv
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Template)
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

let find_all_of_entity_by_label_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {|
  %s
  WHERE pool_message_templates.entity_uuid = UNHEX(REPLACE($1, '-', ''))
  AND pool_message_templates.label = $2
    |}
    select_sql
  |> Caqti_type.(tup2 string string) ->* RepoEntity.t
;;

let find_all_of_entity_by_label pool entity_uuid label =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_all_of_entity_by_label_request
    (Pool_common.Id.value entity_uuid, Label.show label)
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

let insert_default_if_not_exists pool t =
  let open Utils.Lwt_result.Infix in
  find_by_label pool t.Entity.language t.Entity.label
  ||> CCResult.to_opt
  >|> function
  | None -> insert pool t
  | Some _ -> Lwt.return ()
;;

let delete_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_message_templates
    WHERE uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> RepoEntity.Id.t ->. Caqti_type.unit
;;

let delete pool id =
  Utils.Database.exec (Pool_database.Label.value pool) delete_request id
;;
