module Dynparam = Database.Dynparam

let id_select_fragment = Pool_common.Id.sql_select_fragment
let id_value_fragment = Pool_common.Id.sql_value_fragment

let t =
  let open Database.Caqti_encoders in
  let open Entity in
  let open CCResult in
  let decode (target_user_uuid, (user_uuid, (email, (similarity_score, ())))) =
    let* similarity_score =
      CCFloat.of_string_opt similarity_score
      |> CCOption.to_result "Invalid float"
    in
    Ok { target_user_uuid; user_uuid; email; similarity_score }
  in
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom
    ~encode
    ~decode
    Caqti_type.
      [ Pool_common.Repo.Id.t
      ; Pool_common.Repo.Id.t
      ; Pool_user.Repo.EmailAddress.t
      ; string
      ]
;;

let similarity_request user_columns custom_field_columns similarities average =
  let user_columns = user_columns |> CCString.concat "," in
  let custom_field_columns = custom_field_columns |> CCString.concat "," in
  let similarities = similarities |> CCString.concat "," in
  let contact_joins =
    {sql| 
      INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
      LEFT JOIN pool_custom_field_answers ON pool_contacts.user_uuid = pool_custom_field_answers.entity_uuid
    |sql}
  in
  [%string
    {sql|
      WITH target_contact AS (
        SELECT
          user_users.uuid,
          %{user_columns},
          %{custom_field_columns}
        FROM
          pool_contacts
        %{contact_joins}
        WHERE user_users.uuid = UNHEX(REPLACE($1,'-',''))
        GROUP BY user_users.uuid
      ),
      contacts AS (
        SELECT
          user_users.uuid,
          user_users.email,
          %{user_columns},
          %{custom_field_columns}
        FROM
          pool_contacts
        %{contact_joins}
        WHERE user_users.uuid <> UNHEX(REPLACE($1,'-',''))
        GROUP BY user_users.uuid
      ),
      similarity_scores AS (
        SELECT
          t.uuid as target_uuid,
          contacts.uuid,
          contacts.email,
          %{similarities}
        FROM
          contacts,
          target_contact t
      ),
      average_similarity AS (
        SELECT
          target_uuid,
          uuid,
          email,
          %{average} AS similarity_score
        FROM similarity_scores
      )
      SELECT
        %{id_select_fragment ~field:"target_uuid"},
        %{id_select_fragment ~field:"uuid"},
        email,
        similarity_score
      FROM
        average_similarity
      WHERE 
        similarity_score >= 0.3
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
    let id = Custom_field.(id field |> Id.to_common |> Id.value) in
    let target_col = asprintf "t.`%s`" id in
    let user_col = asprintf "contacts.`%s`" id in
    asprintf
      "%s as %s"
      (make_comparison (target_col, user_col) SimilarityCriteria.Exact)
      (make_similarity_name id |> asprintf "`%s`")
  in
  let user_columns =
    columns
    >|= fun col -> asprintf "%s as %s" (concat_sql col) col.Column.sql_column
  in
  (* Dynparam not required anymore *)
  let dyn = Dynparam.(empty |> add Pool_common.Repo.Id.t user_uuid) in
  let custom_field_columns =
    let open Custom_field in
    (* Using placeholders like $2 or ? is not supported in colum names *)
    custom_fields
    >|= fun field ->
    let id = id field |> Id.to_common |> Id.value in
    let column =
      asprintf
        {sql| MAX(CASE WHEN pool_custom_field_answers.custom_field_uuid = %s THEN pool_custom_field_answers.value END) AS %s |sql}
        (id |> asprintf "\"%s\"" |> id_value_fragment)
        (asprintf "`%s`" id)
    in
    column
  in
  let similarities =
    map user_similarities columns @ map field_similarities custom_fields
  in
  let average_similarity =
    let not_null = asprintf "(%s IS NOT NULL)" in
    let coalesce = asprintf "COALESCE(%s, 0)" in
    let user_similarities =
      columns
      >|= fun { Column.sql_column; _ } -> make_similarity_name sql_column
    in
    let custom_field_similarities =
      custom_fields
      >|= fun field ->
      Custom_field.(
        id field
        |> Id.to_common
        |> Id.value
        |> make_similarity_name
        |> asprintf "`%s`")
    in
    let similarities = user_similarities @ custom_field_similarities in
    let division =
      similarities
      >|= not_null
      |> CCString.concat " + "
      |> asprintf "NULLIF(%s, 0)"
    in
    similarities
    >|= coalesce
    |> CCString.concat " + "
    |> fun average -> Format.asprintf "(%s) / %s" average division
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    similarity_request
      user_columns
      custom_field_columns
      similarities
      average_similarity
    |> pt ->* t
  in
  let () =
    Caqti_request.make_pp_with_param () Format.std_formatter (request, pv)
  in
  print_endline "";
  Database.collect database_label request pv
;;
