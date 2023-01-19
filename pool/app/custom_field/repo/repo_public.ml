module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

let get_field_type m = m.Repo_entity.Public.field_type
let id m = m.Repo_entity.Public.id

let has_options m =
  Entity.FieldType.(
    equal Select (get_field_type m) || equal MultiSelect (get_field_type m))
;;

let get_options pool m =
  if has_options m
  then Repo_option.Public.find_by_field pool (id m)
  else Lwt.return []
;;

let get_options_of_multiple pool fields =
  fields
  |> CCList.filter_map (fun m ->
       if has_options m then Some m.Repo_entity.Public.id else None)
  |> Repo_option.Public.find_by_multiple_fields pool
;;

let to_grouped_public pool model fields =
  let%lwt groups = Repo_group.find_by_model pool model in
  let%lwt options = get_options_of_multiple pool fields in
  Repo_entity.Public.to_grouped_entities options groups fields |> Lwt.return
;;

let base_filter_conditions is_admin =
  let base =
    {sql|
    AND pool_custom_fields.disabled = 0
    AND pool_custom_fields.published_at IS NOT NULL
  |sql}
  in
  if is_admin
  then base
  else Format.asprintf "%s AND pool_custom_fields.admin_view_only = 0 " base
;;

module Sql = struct
  let answers_left_join =
    {sql|
      LEFT JOIN pool_custom_field_answers
        ON pool_custom_field_answers.custom_field_uuid = pool_custom_fields.uuid
        AND pool_custom_field_answers.entity_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_custom_field_answers.value IS NOT NULL
    |sql}
  ;;

  let version_left_join =
    {sql|
      LEFT JOIN pool_custom_field_answer_versions
        ON pool_custom_field_answer_versions.custom_field_uuid = pool_custom_fields.uuid
        AND pool_custom_field_answer_versions.entity_uuid = UNHEX(REPLACE($1, '-', ''))
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
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 1, 8), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 9, 4), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 13, 4), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 17, 4), '-',
          SUBSTR(HEX(pool_custom_fields.custom_field_group_uuid), 21)
        )),
        pool_custom_fields.admin_overwrite,
        pool_custom_fields.admin_input_only,
        LOWER(CONCAT(
          SUBSTR(HEX(pool_custom_field_answers.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 21)
        )),
        pool_custom_field_answers.value,
        pool_custom_field_answer_versions.version
      FROM pool_custom_fields
      %s
      %s
    |sql}
      answers_left_join
      version_left_join
  ;;

  let find_all_by_model_request required is_admin =
    let open Caqti_request.Infix in
    let where =
      Format.asprintf
        {sql|
        WHERE pool_custom_fields.model = $2
        %s
        %s
      |sql}
        (base_filter_conditions is_admin)
        (if required then "AND pool_custom_fields.required = 1" else "")
    in
    let order = {sql| ORDER BY pool_custom_fields.position ASC |sql} in
    Format.asprintf "%s \n %s \n %s" select_sql where order
    |> Caqti_type.(tup2 string string ->* Repo_entity.Public.t)
  ;;

  let find_all_by_model model ?(required = false) ?(is_admin = false) pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.collect
      (Database.Label.value pool)
      (find_all_by_model_request required is_admin)
      (Pool_common.Id.value id, Entity.Model.show model)
    >|> to_grouped_public pool model
  ;;

  let find_multiple_by_contact_request is_admin ids =
    let where =
      Format.asprintf
        {sql|
        WHERE pool_custom_fields.model = $2
        %s
        AND pool_custom_fields.uuid in ( %s )
      |sql}
        (base_filter_conditions is_admin)
        (CCList.mapi
           (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 3))
           ids
        |> CCString.concat ",")
    in
    Format.asprintf "%s \n %s" select_sql where
  ;;

  let find_multiple_by_contact ?(is_admin = false) pool contact_id ids =
    if CCList.is_empty ids
    then Lwt.return []
    else
      let open Utils.Lwt_result.Infix in
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
        find_multiple_by_contact_request is_admin ids
        |> pt ->* Repo_entity.Public.t
      in
      Utils.Database.collect (pool |> Database.Label.value) request pv
      >|> fun fields ->
      let%lwt options = get_options_of_multiple pool fields in
      fields |> Repo_entity.Public.to_ungrouped_entities options |> Lwt.return
  ;;

  let find_by_contact_request is_admin =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|%s
      WHERE pool_custom_fields.model = $2
      %s
      AND pool_custom_fields.uuid = UNHEX(REPLACE($3, '-', ''))
    |sql}
      select_sql
      (base_filter_conditions is_admin)
    |> Caqti_type.(tup3 string string string) ->* Repo_entity.Public.t
  ;;

  let find_by_contact ?(is_admin = false) pool contact_id field_id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.collect
      (Database.Label.value pool)
      (find_by_contact_request is_admin)
      ( Pool_common.Id.value contact_id
      , Entity.Model.(show Contact)
      , Entity.Id.value field_id )
    >|> fun field_list ->
    let%lwt options =
      field_list
      |> CCList.head_opt
      |> CCOption.map_or ~default:(Lwt.return []) (get_options pool)
    in
    Repo_entity.Public.to_ungrouped_entities options field_list
    |> CCList.head_opt
    |> CCOption.to_result Pool_common.Message.(NotFound Field.CustomField)
    |> Lwt_result.lift
  ;;

  let all_required_answered_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
      SELECT count(*) questions FROM pool_custom_fields
      %s
      WHERE pool_custom_fields.model = $2
      %s
      AND pool_custom_fields.required = 1
      AND pool_custom_field_answers.value IS NULL
      |sql}
      answers_left_join
      (base_filter_conditions false)
    |> Caqti_type.(tup2 string string ->! int)
  ;;

  let all_required_answered pool contact_id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find
      (Database.Label.value pool)
      all_required_answered_request
      (Pool_common.Id.value contact_id, Entity.Model.(show Contact))
    ||> CCInt.equal 0
  ;;

  let upsert_answer_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_custom_field_answers (
        uuid,
        custom_field_uuid,
        entity_uuid,
        value
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($3, '-', '')),
        $4
      )
      ON DUPLICATE KEY UPDATE
      value = VALUES(value)
      |sql}
    |> Repo_entity_answer.Write.t ->. Caqti_type.unit
  ;;

  let upsert_version_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_custom_field_answer_versions (
        custom_field_uuid,
        entity_uuid,
        version
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        $3
      )
      ON DUPLICATE KEY UPDATE
      version = VALUES(version)
      |sql}
    |> Repo_entity_answer.Version.t ->. Caqti_type.unit
  ;;

  let delete_all_answers_of_multiselect_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_custom_field_answers
      WHERE custom_field_uuid = UNHEX(REPLACE($1, '-', ''))
      AND entity_uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
    |> Caqti_type.(tup2 string string ->. unit)
  ;;

  let delete_all_answers_of_multiselect pool field_id entity_id =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      delete_all_answers_of_multiselect_request
      Pool_common.Id.(field_id |> value, entity_id |> value)
  ;;

  let clear_answer_request =
    let open Caqti_request.Infix in
    let open Pool_common.Repo in
    {sql|
      UPDATE pool_custom_field_answers
        SET value = NULL
      WHERE
        custom_field_uuid = UNHEX(REPLACE($1, '-', ''))
      AND
        entity_uuid = UNHEX(REPLACE($2, '-', ''))
      |sql}
    |> Caqti_type.(tup2 Id.t Id.t) ->. Caqti_type.unit
  ;;

  let clear_answer_version_increment_request =
    let open Caqti_request.Infix in
    let open Pool_common.Repo in
    {sql|
      UPDATE pool_custom_field_answer_versions
        SET version = version + 1
      WHERE
        custom_field_uuid = UNHEX(REPLACE($1, '-', ''))
      AND
        entity_uuid = UNHEX(REPLACE($2, '-', ''))
      |sql}
    |> Caqti_type.(tup2 Id.t Id.t) ->. Caqti_type.unit
  ;;

  let clear_answer pool field_id entity_uuid () =
    let exec = Utils.Database.exec (Database.Label.value pool) in
    let%lwt () = exec clear_answer_request (field_id, entity_uuid) in
    exec clear_answer_version_increment_request (field_id, entity_uuid)
  ;;

  let map_or ~clear fnc t = CCOption.map_or ~default:(clear ()) fnc t

  let upsert_answer pool entity_uuid t =
    let upsert =
      Utils.Database.exec (Database.Label.value pool) upsert_answer_request
    in
    let option_id = Entity.SelectOption.Public.show_id in
    let open Entity.Public in
    let field_id = id t in
    let clear = clear_answer pool field_id entity_uuid in
    let update_answer id value =
      Repo_entity_answer.Write.of_entity id field_id entity_uuid value |> upsert
    in
    let update_version () =
      let version = version t in
      Repo_entity_answer.Version.create field_id entity_uuid version
      |> Utils.Database.exec (Database.Label.value pool) upsert_version_request
    in
    let%lwt () =
      let open Entity.Answer in
      match t with
      | Boolean (_, answer) ->
        answer
        |> map_or ~clear (fun { id; value; _ } ->
             update_answer id (Utils.Bool.to_string value))
      | MultiSelect (_, _, answers) ->
        let%lwt () =
          delete_all_answers_of_multiselect pool field_id entity_uuid
        in
        answers
        |> Lwt_list.iter_s (fun { Entity_answer.id; value } ->
             update_answer id (value |> option_id))
      | Number (_, answer) ->
        answer
        |> map_or ~clear (fun { id; value; _ } ->
             update_answer id (CCInt.to_string value))
      | Select (_, _, answer) ->
        answer
        |> map_or ~clear (fun { id; value; _ } ->
             update_answer id (value |> option_id))
      | Text (_, answer) ->
        answer |> map_or ~clear (fun { id; value; _ } -> update_answer id value)
    in
    update_version ()
  ;;
end

let find_all_by_contact = Sql.find_all_by_model Entity.Model.Contact
let find_all_required_by_contact = find_all_by_contact ~required:true
let find_multiple_by_contact = Sql.find_multiple_by_contact
let find_by_contact = Sql.find_by_contact
let upsert_answer = Sql.upsert_answer
let all_required_answered = Sql.all_required_answered
