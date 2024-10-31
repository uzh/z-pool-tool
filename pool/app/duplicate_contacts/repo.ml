let id_fragment = Pool_common.Id.sql_select_fragment

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

let similarity_request user_columns similarities average =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    WITH target_contact AS (
      SELECT
        user_users.uuid,
        %s
      FROM
        pool_contacts
      INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid
      WHERE user_users.uuid = UNHEX(REPLACE(?,'-',''))
    ),
    similarity_scores AS (
      SELECT
        t.uuid as target_uuid,
        user_users.uuid,
        user_users.email,
        %s
      FROM
        pool_contacts
        INNER JOIN user_users ON pool_contacts.user_uuid = user_users.uuid,
        target_contact t
      WHERE
        user_users.uuid <> t.uuid
    ),
    average_similarity AS (
      SELECT
        target_uuid,
        uuid,
        email,
        name_similarity,
        given_name_similarity,
        %s AS similarity_score
      FROM similarity_scores
    )
    SELECT
      %s,
      %s,
      email,
      similarity_score
    FROM
      average_similarity
    WHERE 
      similarity_score >= 0.5
    ORDER BY
      similarity_score DESC;
  |sql}
    (user_columns |> CCString.concat ",")
    (similarities |> CCString.concat ",")
    average
    (id_fragment ~field:"target_uuid")
    (id_fragment ~field:"uuid")
  |> Pool_common.Repo.Id.t ->* t
;;

let find_similars database_label ~user_uuid =
  let open Entity in
  let open Format in
  let concat_sql { Column.sql_table; sql_column; _ } =
    asprintf "%s.%s" sql_table sql_column
  in
  let user_columns =
    columns
    |> CCList.map (fun col ->
      asprintf "%s as %s" (concat_sql col) col.Column.sql_column)
  in
  let make_similarity_name { Column.sql_column; _ } =
    asprintf "%s_similarity" sql_column
  in
  let make_comparison column =
    let open SimilarityCriteria in
    let open Column in
    let with_name comp =
      asprintf "%s as %s" comp (make_similarity_name column)
    in
    let target_col = asprintf "t.%s" column.sql_column in
    let user_col = concat_sql column in
    with_name
    @@
    match column.criteria with
    | Fuzzy -> asprintf "SOUNDEX(%s) = SOUNDEX(%s)" user_col target_col
    | Exact -> asprintf "%s = %s" user_col target_col
  in
  let similarities = columns |> CCList.map make_comparison in
  let average_similarity =
    let count = CCList.length similarities in
    columns
    |> CCList.map make_similarity_name
    |> CCString.concat " + "
    |> fun average -> Format.asprintf "(%s) / %d" average count
  in
  let () =
    Caqti_request.make_pp_with_param
      ()
      Format.std_formatter
      ( similarity_request user_columns similarities average_similarity
      , user_uuid )
  in
  print_endline "";
  Database.collect
    database_label
    (similarity_request user_columns similarities average_similarity)
    user_uuid
;;
