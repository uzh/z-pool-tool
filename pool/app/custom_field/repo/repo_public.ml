module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

module Sql = struct
  let answers_left_join =
    {sql|
      LEFT JOIN pool_custom_field_answers
      ON pool_custom_field_answers.custom_field_uuid = pool_custom_fields.uuid
      AND pool_custom_field_answers.entity_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  ;;

  let select_sql =
    Format.asprintf
      {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_custom_fields.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_custom_fields.uuid), 21)
        )),
        pool_custom_fields.name,
        pool_custom_fields.hint,
        pool_custom_fields.validation,
        pool_custom_fields.field_type,
        pool_custom_fields.required,
        LOWER(CONCAT(
          SUBSTR(HEX(pool_custom_field_answers.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 21)
        )),
        pool_custom_field_answers.value,
        pool_custom_field_answers.version
      FROM pool_custom_fields
      %s
    |sql}
      answers_left_join
  ;;

  let find_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
        %s
        WHERE pool_custom_fields.uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
      select_sql
    |> Caqti_type.string ->! Repo_entity.Public.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (id |> Entity.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.CustomField)
    >|= Repo_entity.Public.to_entity
  ;;

  let find_all_by_contact_request required =
    let open Caqti_request.Infix in
    let where =
      Format.asprintf
        {sql|
        WHERE pool_custom_fields.model = $2
        AND pool_custom_fields.disabled = 0
        %s
      |sql}
        (if required then "AND pool_custom_fields.required = 1" else "")
    in
    Format.asprintf "%s \n %s" select_sql where
    |> Caqti_type.(tup2 string string ->* Repo_entity.Public.t)
  ;;

  let find_all_by_contact ?(required = false) pool id =
    let open Lwt.Infix in
    Utils.Database.collect
      (Pool_database.Label.value pool)
      (find_all_by_contact_request required)
      (Pool_common.Id.value id, Entity.Model.(show Contact))
    >|= CCList.map Repo_entity.Public.to_entity
  ;;

  let find_multiple_by_contact_request ids =
    let where =
      Format.asprintf
        {sql|
        WHERE pool_custom_fields.model = $2
        AND pool_custom_fields.disabled = 0
        AND pool_custom_fields.uuid in ( %s )
      |sql}
        (CCList.mapi
           (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 3))
           ids
        |> CCString.concat ",")
    in
    Format.asprintf "%s \n %s" select_sql where
  ;;

  let find_multiple_by_contact pool contact_id ids =
    let open Lwt.Infix in
    let open Caqti_request.Infix in
    let dyn =
      let base =
        Dynparam.(
          empty
          |> add Caqti_type.string (contact_id |> Pool_common.Id.value)
          |> add Caqti_type.string Entity.Model.(show Contact))
      in
      CCList.fold_left
        (fun dyn id ->
          dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
        base
        ids
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      find_multiple_by_contact_request ids |> pt ->* Repo_entity.Public.t
    in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
    >|= CCList.map Repo_entity.Public.to_entity
  ;;

  let find_by_contact_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|%s
      WHERE pool_custom_fields.model = $2
      AND pool_custom_fields.disabled = 0
      AND pool_custom_fields.uuid = UNHEX(REPLACE($3, '-', ''))
    |sql}
      select_sql
    |> Caqti_type.(tup3 string string string ->! Repo_entity.Public.t)
  ;;

  let find_by_contact pool contact_id field_id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_by_contact_request
      ( Pool_common.Id.value contact_id
      , Entity.Model.(show Contact)
      , Entity.Id.value field_id )
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.CustomField)
    >|= Repo_entity.Public.to_entity
  ;;

  let all_required_answered_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
      SELECT count(*) questions FROM pool_custom_fields
      %s
      WHERE pool_custom_fields.model = $2
      AND pool_custom_fields.disabled = 0
      AND pool_custom_fields.required = 1
      AND pool_custom_field_answers.value IS NULL
      |sql}
      answers_left_join
    |> Caqti_type.(tup2 string string ->! int)
  ;;

  let all_required_answered pool contact_id =
    let open Lwt.Infix in
    Utils.Database.find
      (Pool_database.Label.value pool)
      all_required_answered_request
      (Pool_common.Id.value contact_id, Entity.Model.(show Contact))
    >|= CCInt.equal 0
  ;;

  let upsert_answer_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_custom_field_answers (
        uuid,
        custom_field_uuid,
        entity_uuid,
        value,
        version
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($3, '-', '')),
        $4,
        $5
      )
      ON DUPLICATE KEY UPDATE
      value = VALUES(value),
      version = VALUES(version) + 1
      |sql}
    |> Repo_entity_answer.Write.t ->. Caqti_type.unit
  ;;

  let upsert_answer pool entity_uuid t =
    let exec =
      Utils.Database.exec (Database.Label.value pool) upsert_answer_request
    in
    let open Entity.Public in
    match t with
    | Number { id; answer; _ } ->
      let field_id = id in
      answer
      |> CCOption.map_or
           ~default:Lwt.return_unit
           (fun { Entity.Answer.id; value; version } ->
           Repo_entity_answer.Write.of_entity
             id
             field_id
             entity_uuid
             (CCInt.to_string value)
             version
           |> exec)
    | Text { id; answer; _ } ->
      let field_id = id in
      answer
      |> CCOption.map_or
           ~default:Lwt.return_unit
           (fun { Entity.Answer.id; value; version } ->
           Repo_entity_answer.Write.of_entity
             id
             field_id
             entity_uuid
             value
             version
           |> exec)
  ;;
end

let find_all_by_contact = Sql.find_all_by_contact
let find_all_required_by_contact = Sql.find_all_by_contact ~required:true
let find_multiple_by_contact = Sql.find_multiple_by_contact
let find_by_contact = Sql.find_by_contact
let upsert_answer = Sql.upsert_answer
let find = Sql.find
let all_required_answered = Sql.all_required_answered
