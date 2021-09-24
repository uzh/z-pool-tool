module RepoEntity = Repo_entity

module Sql = struct
  let select_from_tenants fragment =
    let select_from =
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
          smtp_auth_server,
          smtp_auth_port,
          smtp_auth_username,
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
        FROM pool_tenant
      |sql}
    in
    Format.asprintf "%s %s" select_from fragment
  ;;

  let find_all_request =
    ""
    |> select_from_tenants
    |> Caqti_request.collect Caqti_type.unit RepoEntity.Read.t
  ;;

  let find_by_id_request =
    {sql|
      WHERE pool_tenant.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_from_tenants
    |> Caqti_request.find Caqti_type.string RepoEntity.Read.t
  ;;

  let find_all = Utils.Database.collect find_all_request
  let find_by_id = Utils.Database.find find_by_id_request

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
  let insert t = Utils.Database.exec insert_request t
end

let find_by_id = Sql.find_by_id
let find_all = Sql.find_all
let insert = Sql.insert
let update t = Utils.todo t
let destroy = Utils.todo
