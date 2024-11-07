module Database = Database
module Dynparam = Database.Dynparam

let answer_sql_select_columns =
  [ "pool_custom_fields.field_type"
  ; Entity.Id.sql_select_fragment ~field:"pool_custom_fields.uuid"
  ; "pool_custom_field_answers.value"
  ; "pool_custom_field_answers.admin_value"
  ]
;;

let select_names_of_custom_fields_and_options ids =
  let ids =
    CCList.mapi
      (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
      ids
    |> CCString.concat ","
  in
  let columns =
    [ Entity.Id.sql_select_fragment ~field:"uuid"; "name" ]
    |> CCString.concat ","
  in
  [%string
    {sql| 
        SELECT %{columns} FROM pool_custom_fields WHERE uuid IN (%{ids}) 
          UNION
        SELECT %{columns} FROM pool_custom_field_options WHERE uuid IN (%{ids})
          UNION
        SELECT %{columns} FROM pool_custom_field_groups WHERE uuid IN (%{ids})
    |sql}]
;;

let find_names pool =
  let open Caqti_request.Infix in
  let open Dynparam in
  function
  | [] -> Lwt.return []
  | ids ->
    let (Pack (pt, pv)) =
      (CCList.fold_left (fun dyn id -> dyn |> add Pool_common.Repo.Id.t id))
        empty
        ids
    in
    let request =
      select_names_of_custom_fields_and_options ids
      |> pt ->* Caqti_type.(t2 Pool_common.Repo.Id.t Repo_entity.Name.t)
    in
    Database.collect pool request pv
;;

let find_answer_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      SELECT
        %s
      FROM pool_custom_field_answers
      INNER JOIN pool_custom_fields 
        ON pool_custom_field_answers.custom_field_uuid = pool_custom_fields.uuid
      WHERE 
        pool_custom_fields.uuid = UNHEX(REPLACE($1, '-', ''))
      AND
        pool_custom_field_answers.entity_uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
    (answer_sql_select_columns |> CCString.concat ", ")
  |> Caqti_type.(
       t2 Repo_entity_base.Id.t Contact.Repo.Id.t
       ->! Repo_entity_answer.VersionHistory.t)
;;

let find_answer_opt pool contact_id field_id =
  Database.find_opt pool find_answer_request (field_id, contact_id)
;;
