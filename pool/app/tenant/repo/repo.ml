module RepoEntity = Repo_entity
module Label = Pool_common.Database.Label
module Id = Pool_common.Id
module LogoMapping = Entity_logo_mapping
module LogoMappingRepo = Repo_logo_mapping

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
        icon = UNHEX(REPLACE($14, '-', '')),
        mainenance = $15,
        disabled = $16,
        default_language = $17,
        created_at = $18,
        updated_at = $19
      WHERE
      pool_tenant.uuid = UNHEX(REPLACE($1, '-', ''));
    |sql}
    |> Caqti_request.exec RepoEntity.Write.t
  ;;

  let update pool = Utils.Database.exec (Label.value pool) update_request

  let select_from_tenants_sql where_fragment full =
    let database_fragment =
      match full with
      | true ->
        {sql|
          pool_tenant.database_url,
          pool_tenant.database_label,
        |sql}
      | false -> {sql|
          pool_tenant.database_label,
        |sql}
    in
    let smtp_auth_fragment =
      match full with
      | true ->
        {sql|
          pool_tenant.smtp_auth_server,
          pool_tenant.smtp_auth_port,
          pool_tenant.smtp_auth_username,
          pool_tenant.smtp_auth_password,
          pool_tenant.smtp_auth_authentication_method,
          pool_tenant.smtp_auth_protocol,
        |sql}
      | false ->
        {sql|
          pool_tenant.smtp_auth_server,
          pool_tenant.smtp_auth_port,
          pool_tenant.smtp_auth_username,
          pool_tenant.smtp_auth_authentication_method,
          pool_tenant.smtp_auth_protocol,
        |sql}
    in
    let styles_fragment =
      match full with
      | true ->
        {sql|
          LOWER(CONCAT(
            SUBSTR(HEX(styles.uuid), 1, 8), '-',
            SUBSTR(HEX(styles.uuid), 9, 4), '-',
            SUBSTR(HEX(styles.uuid), 13, 4), '-',
            SUBSTR(HEX(styles.uuid), 17, 4), '-',
            SUBSTR(HEX(styles.uuid), 21)
          )),
        |sql}
      | false ->
        {sql|
          LOWER(CONCAT(
            SUBSTR(HEX(styles.uuid), 1, 8), '-',
            SUBSTR(HEX(styles.uuid), 9, 4), '-',
            SUBSTR(HEX(styles.uuid), 13, 4), '-',
            SUBSTR(HEX(styles.uuid), 17, 4), '-',
            SUBSTR(HEX(styles.uuid), 21)
          )),
          styles.filename,
          styles.filesize,
          styles.mime,
          styles.created,
          styles.updated,
        |sql}
    in
    let icon_fragment =
      match full with
      | true ->
        {sql|
          LOWER(CONCAT(
            SUBSTR(HEX(icon.uuid), 1, 8), '-',
            SUBSTR(HEX(icon.uuid), 9, 4), '-',
            SUBSTR(HEX(icon.uuid), 13, 4), '-',
            SUBSTR(HEX(icon.uuid), 17, 4), '-',
            SUBSTR(HEX(icon.uuid), 21)
          )),
        |sql}
      | false ->
        {sql|
          LOWER(CONCAT(
            SUBSTR(HEX(icon.uuid), 1, 8), '-',
            SUBSTR(HEX(icon.uuid), 9, 4), '-',
            SUBSTR(HEX(icon.uuid), 13, 4), '-',
            SUBSTR(HEX(icon.uuid), 17, 4), '-',
            SUBSTR(HEX(icon.uuid), 21)
          )),
          icon.filename,
          icon.filesize,
          icon.mime,
          icon.created,
          icon.updated,
        |sql}
    in
    let select_from =
      Format.asprintf
        {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(pool_tenant.uuid), 1, 8), '-',
              SUBSTR(HEX(pool_tenant.uuid), 9, 4), '-',
              SUBSTR(HEX(pool_tenant.uuid), 13, 4), '-',
              SUBSTR(HEX(pool_tenant.uuid), 17, 4), '-',
              SUBSTR(HEX(pool_tenant.uuid), 21)
            )),
            pool_tenant.title,
            pool_tenant.description,
            pool_tenant.url,
            %s
            %s
            %s
            %s
            pool_tenant.mainenance,
            pool_tenant.disabled,
            pool_tenant.default_language,
            pool_tenant.created_at,
            pool_tenant.updated_at
          FROM pool_tenant
          LEFT JOIN storage_handles styles
            ON pool_tenant.styles = styles.uuid
          LEFT JOIN storage_handles icon
            ON pool_tenant.icon = icon.uuid
        |sql}
        database_fragment
        smtp_auth_fragment
        styles_fragment
        icon_fragment
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
    let open Lwt.Infix in
    Utils.Database.find_opt (Label.value pool) find_request (Id.value id)
    >|= CCOpt.to_result Pool_common.Message.(NotFound Tenant)
  ;;

  let find_full_request =
    select_from_tenants_sql find_fragment true
    |> Caqti_request.find Caqti_type.string RepoEntity.Write.t
  ;;

  let find_full pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt (Label.value pool) find_full_request (Id.value id)
    >|= CCOpt.to_result Pool_common.Message.(NotFound Tenant)
  ;;

  let find_by_label_request =
    select_from_tenants_sql
      {sql| WHERE pool_tenant.database_label = ? |sql}
      false
    |> Caqti_request.find Caqti_type.string RepoEntity.t
  ;;

  let find_by_label pool label =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Label.value pool)
      find_by_label_request
      (Label.value label)
    >|= CCOpt.to_result Pool_common.Message.(NotFound Tenant)
  ;;

  let find_all_request =
    select_from_tenants_sql "" false
    |> Caqti_request.collect Caqti_type.unit RepoEntity.t
  ;;

  let find_all pool = Utils.Database.collect (Label.value pool) find_all_request

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

  let find_databases pool =
    Utils.Database.collect (Label.value pool) find_databases_request
  ;;

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
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?
      );
    |sql}
    |> Caqti_request.exec RepoEntity.Write.t
  ;;

  let insert pool = Utils.Database.exec (Label.value pool) insert_request

  let find_selectable_request =
    {sql|
      SELECT
        url,
        database_label
      FROM pool_tenant
    |sql}
    |> Caqti_request.collect Caqti_type.unit RepoEntity.Selection.t
  ;;

  let find_selectable pool =
    Utils.Database.collect (Label.value pool) find_selectable_request
  ;;
end

let set_logos tenant logos =
  let tenant_logos, partner_logo =
    let open LogoMapping.LogoType in
    CCList.partition_filter_map
      (fun l ->
        match l.LogoMapping.logo_type with
        | TenantLogo -> `Left l.LogoMapping.file
        | PartnerLogo -> `Right l.LogoMapping.file)
      logos
  in
  let open Entity.Read in
  Entity.
    { id = tenant.Read.id
    ; title = tenant.title
    ; description = tenant.description
    ; url = tenant.url
    ; database_label = tenant.database_label
    ; smtp_auth = tenant.smtp_auth
    ; styles = tenant.styles
    ; icon = tenant.icon
    ; logos = tenant_logos
    ; partner_logo
    ; maintenance = tenant.maintenance
    ; disabled = tenant.disabled
    ; default_language = tenant.default_language
    ; created_at = tenant.created_at
    ; updated_at = tenant.updated_at
    }
;;

let find pool id =
  let open Lwt_result.Syntax in
  let* tenant = Sql.find pool id in
  let%lwt logos = LogoMappingRepo.find_by_tenant id in
  set_logos tenant logos |> Lwt.return_ok
;;

let find_by_label pool label =
  let open Lwt_result.Syntax in
  let* tenant = Sql.find_by_label pool label in
  let%lwt logos = LogoMappingRepo.find_by_tenant tenant.Entity.Read.id in
  set_logos tenant logos |> Lwt.return_ok
;;

let find_full = Sql.find_full

let find_all pool () =
  let%lwt tenants = Sql.find_all pool () in
  let%lwt logos = LogoMappingRepo.find_all () in
  let logos_of_tenant id =
    CCList.filter (fun logo -> Id.equal logo.LogoMapping.tenant_id id) logos
  in
  CCList.map (fun t -> set_logos t (logos_of_tenant t.Entity.Read.id)) tenants
  |> Lwt.return
;;

let find_databases = Sql.find_databases
let find_selectable = Sql.find_selectable
let insert = Sql.insert
let update = Sql.update
let destroy = Utils.todo
