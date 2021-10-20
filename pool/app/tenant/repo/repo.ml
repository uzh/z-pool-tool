module RepoEntity = Repo_entity
module Label = Pool_common.Database.Label

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
        styles = UNHEX(REPLACE($13, '-', '')),
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
    |> Caqti_request.exec RepoEntity.Write.t
  ;;

  let update pool = Utils.Database.exec pool update_request

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
            LOWER(CONCAT(
              SUBSTR(HEX(styles), 1, 8), '-',
              SUBSTR(HEX(styles), 9, 4), '-',
              SUBSTR(HEX(styles), 13, 4), '-',
              SUBSTR(HEX(styles), 17, 4), '-',
              SUBSTR(HEX(styles), 21)
            )),
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

  let find_fragment =
    {sql|
      WHERE pool_tenant.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  ;;

  let find_request =
    select_from_tenants_sql find_fragment false
    |> Caqti_request.find Caqti_type.string RepoEntity.t
  ;;

  let find pool id =
    Utils.Database.find pool find_request (id |> Pool_common.Id.value)
  ;;

  let find_full_request =
    select_from_tenants_sql find_fragment true
    |> Caqti_request.find Caqti_type.string RepoEntity.Write.t
  ;;

  let find_full pool id =
    Utils.Database.find pool find_full_request (id |> Pool_common.Id.value)
  ;;

  let find_all_request =
    select_from_tenants_sql "" false
    |> Caqti_request.collect Caqti_type.unit RepoEntity.t
  ;;

  let find_all pool = Utils.Database.collect pool find_all_request

  let find_databases_request =
    {sql|
        SELECT
          database_url,
          database_label
        FROM pool_tenant
        WHERE NOT disabled
      |sql}
    |> Caqti_request.collect Caqti_type.unit Pool_common.Repo.Database.t
  ;;

  let find_databases pool = Utils.Database.collect pool find_databases_request

  let insert_request =
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
        UNHEX(REPLACE(?, '-', '')),
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
    |> Caqti_request.exec RepoEntity.Write.t
  ;;

  let insert pool = Utils.Database.exec pool insert_request

  let find_selectable_request =
    {sql|
      SELECT
        url,
        database_label
      FROM pool_tenant
    |sql}
    |> Caqti_request.collect Caqti_type.unit RepoEntity.Selection.t
  ;;

  let find_selectable pool = Utils.Database.collect pool find_selectable_request
end

let find pool = Sql.find (Label.value pool)
let find_full pool = Sql.find_full (Label.value pool)
let find_all pool = Sql.find_all (Label.value pool)
let find_databases pool = Sql.find_databases (Label.value pool)
let find_selectable pool = Sql.find_selectable (Label.value pool)
let insert pool = Sql.insert (Label.value pool)

let update pool : Entity.Write.t -> (unit, string) result Lwt.t =
  Sql.update (Label.value pool)
;;

let destroy = Utils.todo
