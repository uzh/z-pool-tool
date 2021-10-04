module RepoEntity = Repo_entity

module Sql = struct
  let update_request =
    {sql|
      UPDATE pool_tenant
      SET
        title = $2,
        description = $3,
        url = $4,
        database_url = $5,
        database_label = $6,
        smtp_auth_server = $7,
        smtp_auth_port = $8,
        smtp_auth_username = $9,
        smtp_auth_password = $10,
        smtp_auth_authentication_method = $11,
        smtp_auth_protocol = $12,
        styles = $13,
        icon = $14,
        logos = $15,
        partner_logos = $16,
        mainenance = $17,
        disabled = $18,
        default_language = $19,
        created_at = $20,
        updated_at = $21
      WHERE
      pool_tenant.uuid = UNHEX(REPLACE($1, '-', ''));
    |sql}
    |> Caqti_request.exec RepoEntity.t
  ;;

  let update = Utils.Database.exec update_request

  let select_from_tenants_sql where_fragment full =
    let database_fragment =
      match full with
      | true ->
        {sql|
          database_url,
          database_label,
        |sql}
      | false -> {sql|
          database_label,
        |sql}
    in
    let smtp_auth_fragment =
      match full with
      | true ->
        {sql|
          smtp_auth_server,
          smtp_auth_port,
          smtp_auth_username,
          smtp_auth_password,
          smtp_auth_authentication_method,
          smtp_auth_protocol,
        |sql}
      | false ->
        {sql|
          smtp_auth_server,
          smtp_auth_port,
          smtp_auth_username,
          smtp_auth_authentication_method,
          smtp_auth_protocol,
        |sql}
    in
    let select_from =
      Format.asprintf
        {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(uuid), 1, 8), '-',
              SUBSTR(HEX(uuid), 9, 4), '-',
              SUBSTR(HEX(uuid), 13, 4), '-',
              SUBSTR(HEX(uuid), 17, 4), '-',
              SUBSTR(HEX(uuid), 21)
            )),
            title,
            description,
            url,
            %s
            %s
            styles,
            icon,
            logos,
            partner_logos,
            mainenance,
            disabled,
            default_language,
            created_at,
            updated_at
          FROM pool_tenant
        |sql}
        database_fragment
        smtp_auth_fragment
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_all_request =
    select_from_tenants_sql "" false
    |> Caqti_request.collect Caqti_type.unit RepoEntity.Read.t
  ;;

  let find_fragment =
    {sql|
      WHERE pool_tenant.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  ;;

  let find_request =
    select_from_tenants_sql find_fragment false
    |> Caqti_request.find Caqti_type.string RepoEntity.Read.t
  ;;

  let find_full_request =
    select_from_tenants_sql find_fragment true
    |> Caqti_request.find Caqti_type.string RepoEntity.t
  ;;

  let find_all = Utils.Database.collect find_all_request
  let find id = Utils.Database.find find_request (id |> Pool_common.Id.value)

  let find_full id =
    Utils.Database.find find_full_request (id |> Pool_common.Id.value)
  ;;

  let insert_sql =
    {sql|
      INSERT INTO pool_tenant (
        uuid,
        title,
        description,
        url,
        database_url,
        database_label,
        smtp_auth_server,
        smtp_auth_port,
        smtp_auth_username,
        smtp_auth_password,
        smtp_auth_authentication_method,
        smtp_auth_protocol,
        styles,
        icon,
        logos,
        partner_logos,
        mainenance,
        disabled,
        default_language,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      );
    |sql}
  ;;

  let insert_request = Caqti_request.exec RepoEntity.t insert_sql
  let insert = Utils.Database.exec insert_request
end

let find = Sql.find
let find_full = Sql.find_full
let find_all = Sql.find_all
let insert = Sql.insert
let update = Sql.update
let destroy = Utils.todo
