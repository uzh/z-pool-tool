module Dynparam = Database.Dynparam
open Repo_entity

let id_select_fragment = Pool_common.Id.sql_select_fragment
let id_value_fragment = Pool_common.Id.sql_value_fragment

let sql_select_columns =
  let open Contact.Repo in
  [ id_select_fragment ~field:"pool_contacts_possible_duplicates.uuid" ]
  @ make_sql_select_columns ~user_table:"user_a" ~contact_table:"contact_a"
  @ make_sql_select_columns ~user_table:"user_b" ~contact_table:"contact_b"
  @ [ "pool_contacts_possible_duplicates.score"
    ; "pool_contacts_possible_duplicates.ignore"
    ]
;;

let joins =
  {sql|
    INNER JOIN pool_contacts AS contact_a ON contact_a.user_uuid = pool_contacts_possible_duplicates.contact_a
    INNER JOIN user_users AS user_a ON contact_a.user_uuid = user_a.uuid

    INNER JOIN pool_contacts AS contact_b ON contact_b.user_uuid = pool_contacts_possible_duplicates.contact_b
    INNER JOIN user_users AS user_b ON contact_b.user_uuid = user_b.uuid
  |sql}
;;

let find_request_sql ?(count = false) where =
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
  in
  [%string
    {sql| SELECT %{columns} FROM pool_contacts_possible_duplicates %{joins} %{where} |sql}]
;;

let similarity_request user_columns custom_field_columns similarities average =
  let columns = user_columns @ custom_field_columns |> CCString.concat "," in
  let similarities = similarities |> CCString.concat "," in
  let contact_joins =
    {sql| 
      INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
      LEFT JOIN pool_custom_field_answers ON pool_contacts.user_uuid = pool_custom_field_answers.entity_uuid
    |sql}
  in
  [%string
    {sql|
      WITH filtered_contacts AS (
        SELECT
          user_users.uuid,
          %{columns}
        FROM
          pool_contacts
        %{contact_joins}
        WHERE 
          pool_contacts.email_verified IS NOT NULL
          AND pool_contacts.disabled = 0
        GROUP BY user_users.uuid
      ),
      similarity_scores AS (
        SELECT
          t.uuid as target_uuid,
          contacts.uuid,
          %{similarities}
        FROM
        filtered_contacts AS t
        CROSS JOIN filtered_contacts AS contacts
        WHERE
          t.uuid = UNHEX(REPLACE($1, '-', ''))
          AND contacts.uuid <> t.uuid
      ),
      average_similarity AS (
        SELECT
          target_uuid,
          uuid,
          %{average} AS similarity_score
        FROM similarity_scores
      )
      SELECT
        %{id_select_fragment ~field:"target_uuid"},
        %{id_select_fragment ~field:"uuid"},
        CAST(similarity_score AS FLOAT)
      FROM
        average_similarity
      WHERE 
        similarity_score >= $2
      ORDER BY
        similarity_score DESC;
    |sql}]
;;

let find_similars database_label ~user_uuid custom_fields =
  let open CCList in
  let open Entity in
  let open Format in
  let open Caqti_request.Infix in
  let concat_sql ?table { Column.sql_table; sql_column; _ } =
    asprintf "%s.%s" (CCOption.value ~default:sql_table table) sql_column
  in
  let make_similarity_name sql_column = asprintf "%s_similarity" sql_column in
  let make_comparison (left_column, right_column) =
    let open SimilarityCriteria in
    function
    | Fuzzy -> asprintf "SOUNDEX(%s) = SOUNDEX(%s)" left_column right_column
    | Exact -> asprintf "%s = %s" left_column right_column
  in
  let user_similarities column =
    let open Column in
    let with_name comp =
      asprintf "%s as %s" comp (make_similarity_name column.sql_column)
    in
    let target_col = asprintf "t.%s" column.sql_column in
    let user_col = concat_sql ~table:"contacts" column in
    make_comparison (user_col, target_col) column.criteria |> with_name
  in
  let field_similarities field =
    let id = Custom_field.(id field |> Id.value) in
    let target_col = asprintf "t.`%s`" id in
    let user_col = asprintf "contacts.`%s`" id in
    asprintf
      "%s as %s"
      (make_comparison (target_col, user_col) SimilarityCriteria.Exact)
      (make_similarity_name id |> asprintf "`%s`")
  in
  let user_columns =
    columns >|= fun col -> asprintf "%s as %s" (concat_sql col) col.Column.sql_column
  in
  let dyn =
    Dynparam.(
      empty
      |> add Pool_common.Repo.Id.t user_uuid
      |> add Caqti_type.float Entity.alert_threshold)
  in
  let custom_field_columns =
    let open Custom_field in
    (* Using placeholders like $2 or ? is not supported in colum names *)
    custom_fields
    >|= fun field ->
    let id = id field |> Id.value in
    asprintf
      {sql| MAX(
        CASE WHEN pool_custom_field_answers.custom_field_uuid = %s 
        THEN COALESCE(pool_custom_field_answers.admin_value, pool_custom_field_answers.value) 
        END) AS %s 
      |sql}
      (id |> asprintf "\"%s\"" |> id_value_fragment)
      (asprintf "`%s`" id)
  in
  let similarities =
    map user_similarities columns @ map field_similarities custom_fields
  in
  let average_similarity =
    let not_null = CCFun.uncurry (asprintf "(%s IS NOT NULL) * %d") in
    let coalesce = CCFun.uncurry (asprintf "COALESCE(%s * %d, 0)") in
    let user_similarities =
      columns
      >|= fun { Column.sql_column; weight; _ } -> make_similarity_name sql_column, weight
    in
    let custom_field_similarities =
      custom_fields
      >|= fun field ->
      let open Custom_field in
      let weight =
        CCOption.(
          duplicate_weighting field >|= DuplicateWeighting.value |> value ~default:0)
      in
      id field |> Id.value |> make_similarity_name |> asprintf "`%s`", weight
    in
    let similarities = user_similarities @ custom_field_similarities in
    let division =
      similarities >|= not_null |> CCString.concat " + " |> asprintf "NULLIF(%s, 0)"
    in
    similarities
    >|= coalesce
    |> CCString.concat " + "
    |> fun average -> Format.asprintf "(%s) / %s" average division
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    similarity_request user_columns custom_field_columns similarities average_similarity
    |> pt ->* raw
  in
  Database.collect database_label request pv
;;

let insert_request =
  Format.asprintf
    {sql|
    INSERT INTO pool_contacts_possible_duplicates (
      uuid, 
      contact_a, 
      contact_b,
      score
    ) VALUES 
      %s
    ON DUPLICATE KEY UPDATE
      score = VALUES(score),
      updated_at = NOW();
  |sql}
;;

let insert pool = function
  | [] -> Lwt.return_unit
  | rows ->
    let open Dynparam in
    let open Caqti_request.Infix in
    let id = Pool_common.Repo.Id.t in
    let id_sql = "UNHEX(REPLACE(?, '-', ''))" in
    let dyn, sql =
      rows
      |> CCList.fold_left
           (fun (dyn, sql) (target_id, contact_id, score) ->
              let dyn =
                dyn
                |> add id (Pool_common.Id.create ())
                |> add id target_id
                |> add id contact_id
                |> add Caqti_type.float score
              in
              let values =
                [ id_sql; id_sql; id_sql; "?" ]
                |> CCString.concat ","
                |> Format.asprintf "(%s)"
              in
              let sql = sql @ [ values ] in
              dyn, sql)
           (empty, [])
    in
    let sql = CCString.concat "," sql in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request = insert_request sql |> pt ->. Caqti_type.unit in
    Database.exec pool request pv
;;

let find_request =
  let open Caqti_request.Infix in
  {sql| WHERE pool_contacts_possible_duplicates.uuid = UNHEX(REPLACE(?, '-', '')) |sql}
  |> find_request_sql
  |> Id.t ->! t
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request id
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Duplicate)
;;

let find_by_contact ?query pool contact =
  let where =
    let open Contact in
    let sql =
      {sql|
        pool_contacts_possible_duplicates.contact_a = UNHEX(REPLACE($1, '-', ''))
          OR 
        pool_contacts_possible_duplicates.contact_a = UNHEX(REPLACE($1, '-', ''))
      |sql}
    in
    sql, Dynparam.(empty |> add Repo.Id.t (id contact))
  in
  Query.collect_and_count pool query ~where ~select:find_request_sql Repo_entity.t
;;

let all ?query pool =
  Query.collect_and_count pool query ~select:find_request_sql Repo_entity.t
;;

let ingore_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
      UPDATE 
        pool_contacts_possible_duplicates
      SET
        `ignore` = 1
      WHERE
        uuid = UNHEX(REPLACE($1, '-', '')) 
    |sql}
  |> Repo_entity.Id.t ->. Caqti_type.unit
;;

let ignore pool { Entity.id; _ } = Database.exec pool ingore_request id

let find_to_check pool =
  let open Caqti_request.Infix in
  let open Contact.Repo in
  let request =
    Format.asprintf
      {sql|
        SELECT
          %s
        FROM pool_contacts
          %s
        WHERE 
          (duplicates_last_checked IS NULL
            OR 
          duplicates_last_checked < NOW() - INTERVAL 1 WEEK)
          AND disabled = 0
        ORDER BY duplicates_last_checked IS NULL DESC, duplicates_last_checked ASC
        LIMIT 1
      |sql}
      (CCString.concat ", " sql_select_columns)
      joins
    |> Caqti_type.unit ->! t
  in
  Database.find_opt pool request ()
;;

let mark_as_checked pool contact =
  let open Caqti_request.Infix in
  let request =
    Format.asprintf
      {sql|
        UPDATE pool_contacts
        SET duplicates_last_checked = NOW()
        WHERE user_uuid = UNHEX(REPLACE($1, '-', ''));
      |sql}
    |> Contact.Repo.Id.t ->. Caqti_type.unit
  in
  Database.exec pool request (Contact.id contact)
;;

let count pool =
  let open Caqti_request.Infix in
  let request =
    {sql|
      SELECT
        COUNT(id)
      FROM
        pool_contacts_possible_duplicates
      WHERE
        `ignore` = 0
    |sql}
    |> Caqti_type.(unit ->! int)
  in
  Database.find pool request ()
;;
