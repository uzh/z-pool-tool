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

let to_grouped_public is_admin pool model fields =
  let%lwt groups = Repo_group.find_by_model pool model in
  let%lwt options = get_options_of_multiple pool fields in
  Repo_entity.Public.to_grouped_entities is_admin options groups fields
  |> Lwt.return
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
        pool_custom_fields.admin_override,
        pool_custom_fields.admin_input_only,
        LOWER(CONCAT(
          SUBSTR(HEX(pool_custom_field_answers.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_custom_field_answers.uuid), 21)
        )),
        pool_custom_field_answers.value,
        pool_custom_field_answers.admin_value,
        pool_custom_field_answers.version,
        pool_custom_field_answers.admin_version
      FROM pool_custom_fields
      %s
    |sql}
      answers_left_join
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

  let find_all_by_model model ~required ~is_admin pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.collect
      (Database.Label.value pool)
      (find_all_by_model_request required is_admin)
      (Pool_common.Id.value id, Entity.Model.show model)
    >|> to_grouped_public is_admin pool model
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
      fields
      |> Repo_entity.Public.to_ungrouped_entities is_admin options
      |> Lwt.return
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
    Repo_entity.Public.to_ungrouped_entities is_admin options field_list
    |> CCList.head_opt
    |> CCOption.to_result Pool_common.Message.(NotFound Field.CustomField)
    |> Lwt_result.lift
  ;;

  let all_answered_request required =
    let open Caqti_request.Infix in
    let base =
      Format.asprintf
        {sql|
      SELECT count(*) questions FROM pool_custom_fields
      %s
      WHERE pool_custom_fields.model = $2
      %s
      AND pool_custom_field_answers.value IS NULL
      |sql}
        answers_left_join
        (base_filter_conditions false)
    in
    (match required with
     | false -> base
     | true -> Format.asprintf "%s AND pool_custom_fields.required = 1" base)
    |> Caqti_type.(tup2 string string ->! int)
  ;;

  let all_answered required pool contact_id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find
      (Database.Label.value pool)
      (all_answered_request required)
      (Pool_common.Id.value contact_id, Entity.Model.(show Contact))
    ||> CCInt.equal 0
  ;;
end

let find_all_by_contact = Sql.find_all_by_model Entity.Model.Contact
let find_multiple_by_contact = Sql.find_multiple_by_contact
let find_by_contact = Sql.find_by_contact
let all_required_answered = Sql.all_answered true
let all_answered = Sql.all_answered false
