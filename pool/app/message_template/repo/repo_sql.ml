module Dynparam = Database.Dynparam
module RepoEntity = Repo_entity
module Database = Database
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

let insert pool = Database.exec pool insert_request

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

let update pool t = Database.exec pool update_request t

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

let all_default pool = Database.collect pool find_all_default_request

let find_all_of_entity_by_label_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {|
  %s
  WHERE pool_message_templates.entity_uuid = UNHEX(REPLACE($1, '-', ''))
  AND pool_message_templates.label = $2
    |}
    select_sql
  |> Caqti_type.(t2 string string) ->* RepoEntity.t
;;

let find_all_of_entity_by_label pool entity_uuid label =
  Database.collect
    pool
    find_all_of_entity_by_label_request
    (Pool_common.Id.value entity_uuid, Label.show label)
;;

let find_default_by_label_and_language_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    %s
    WHERE
      pool_message_templates.label = ?
    AND
      pool_message_templates.language = ?
    AND
      pool_message_templates.entity_uuid IS NULL
    |sql}
    select_sql
  |> Caqti_type.(t2 string string) ->! RepoEntity.t
;;

let find_default_by_label_and_language pool language label =
  Database.find_opt
    pool
    find_default_by_label_and_language_request
    (Entity.Label.show label, Pool_common.Language.show language)
;;

let find_default_by_label_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    %s
    WHERE
      pool_message_templates.label = ?
    AND
      pool_message_templates.entity_uuid IS NULL
    |sql}
    select_sql
  |> Caqti_type.string ->! RepoEntity.t
;;

let find_default_by_label pool label =
  Database.collect pool find_default_by_label_request (Entity.Label.show label)
;;

let find_by_label_and_language_to_send pool ?entity_uuids label language =
  let open Caqti_request.Infix in
  let dyn =
    Dynparam.(
      empty
      |> add Caqti_type.string (Label.show label)
      |> add Caqti_type.string (Pool_common.Language.show language))
  in
  let where =
    {sql|
    WHERE
    pool_message_templates.label = $1
    AND
    pool_message_templates.language = $2
  |sql}
  in
  let where, dyn =
    match entity_uuids with
    | None | Some [] ->
      ( Format.asprintf "%s AND pool_message_templates.entity_uuid IS NULL" where
      , dyn )
    | Some ids ->
      let dyn, ids =
        ids
        |> CCList.foldi
             (fun (dyn, ids) i entity_uuid ->
               let dyn =
                 Dynparam.(
                   dyn
                   |> add Caqti_type.string (Pool_common.Id.value entity_uuid))
               in
               let ids =
                 ids
                 @ [ Format.asprintf "UNHEX(REPLACE($%i, '-', ''))" (i + 3) ]
               in
               dyn, ids)
             (dyn, [])
      in
      let where =
        let ids = ids |> CCString.concat ", " in
        Format.asprintf
          {sql|
                %s
                AND(entity_uuid IS NULL
                    OR entity_uuid IN(%s))
                ORDER BY
                  ISNULL(pool_message_templates.entity_uuid),
                  FIELD(pool_message_templates.entity_uuid, %s)
                LIMIT 1
              |sql}
          where
          ids
          ids
      in
      where, dyn
  in
  let sql = Format.asprintf "%s\n%s" select_sql where in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request = sql |> pt ->! RepoEntity.t in
  Database.find pool request pv
;;

(* The template are prioritised according to the entity_uuids list, from left to
   right. If none are found, the default template will be returned. *)
let find_all_by_label_to_send pool ?entity_uuids languages label =
  let open Utils.Lwt_result.Infix in
  if CCList.is_empty languages
  then Lwt.return []
  else (
    match entity_uuids with
    | None | Some [] ->
      languages
      |> Lwt_list.map_s (fun lang ->
        find_default_by_label_and_language pool lang label
        ||> CCOption.get_exn_or
              (Format.asprintf
                 "Default message template %s (%s) is missing"
                 (Label.show label)
                 (Pool_common.Language.show lang)))
    | Some entity_uuids ->
      languages
      |> Lwt_list.map_s
           (find_by_label_and_language_to_send pool label ~entity_uuids))
;;

let find_entity_defaults_by_label pool ?entity_uuids languages label =
  (* Removing the last uuid from the entity_uuids to so the entity default is
     returned *)
  let entity_uuids =
    match entity_uuids with
    | None | Some [] | Some (_ :: []) -> []
    | Some list -> CCList.(list |> rev |> tl |> rev)
  in
  find_all_by_label_to_send pool ~entity_uuids languages label
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
  Database.find_opt pool find_request (Pool_common.Id.value id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.MessageTemplate)
;;

let insert_default_if_not_exists pool t =
  let open Utils.Lwt_result.Infix in
  find_default_by_label_and_language pool t.Entity.language t.Entity.label
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

let delete pool id = Database.exec pool delete_request id
