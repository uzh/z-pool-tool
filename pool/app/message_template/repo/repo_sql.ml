module RepoEntity = Repo_entity
module Database = Pool_database

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
