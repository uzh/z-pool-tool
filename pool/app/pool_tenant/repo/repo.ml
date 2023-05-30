module RepoEntity = Repo_entity
module Database = Pool_database
module Id = Pool_common.Id
module LogoMapping = Entity_logo_mapping
module LogoMappingRepo = Repo_logo_mapping

module Sql = struct
  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_tenant
      SET
        title = $2,
        description = $3,
        url = $4,
        database_url = $5,
        database_label = $6,
        gtx_api_key = $7,
        styles = UNHEX(REPLACE($8, '-', '')),
        icon = UNHEX(REPLACE($9, '-', '')),
        mainenance = $10,
        disabled = $11,
        default_language = $12,
        created_at = $13,
        updated_at = $14
      WHERE
      pool_tenant.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Write.t ->. Caqti_type.unit
  ;;

  let update pool =
    Utils.Database.exec (Database.Label.value pool) update_request
  ;;

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
      let api_key =
        match full with
        | true -> "pool_tenant.gtx_api_key,"
        | false -> ""
      in
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
        api_key
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
    let open Caqti_request.Infix in
    select_from_tenants_sql find_fragment false
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_request
      (Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tenant)
  ;;

  let find_full_request =
    let open Caqti_request.Infix in
    select_from_tenants_sql find_fragment true
    |> Caqti_type.string ->! RepoEntity.Write.t
  ;;

  let find_full pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_full_request
      (Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tenant)
  ;;

  let find_by_label_request =
    let open Caqti_request.Infix in
    select_from_tenants_sql
      {sql| WHERE pool_tenant.database_label = ? |sql}
      false
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find_by_label pool label =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_by_label_request
      (Database.Label.value label)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tenant)
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    select_from_tenants_sql "" false |> Caqti_type.unit ->* RepoEntity.t
  ;;

  let find_all pool =
    Utils.Database.collect (Database.Label.value pool) find_all_request
  ;;

  let find_databases_request =
    let open Caqti_request.Infix in
    {sql|
        SELECT
          database_url,
          database_label
        FROM pool_tenant
        WHERE NOT disabled
      |sql}
    |> Caqti_type.unit ->* Database.Repo.t
  ;;

  let find_databases pool =
    Utils.Database.collect (Database.Label.value pool) find_databases_request
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_tenant (
        uuid,
        title,
        description,
        url,
        database_url,
        database_label,
        gtx_api_key,
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
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?
      )
    |sql}
    |> RepoEntity.Write.t ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Database.Label.value pool) insert_request
  ;;

  let find_selectable_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        url,
        database_label
      FROM pool_tenant
      WHERE NOT disabled
    |sql}
    |> Caqti_type.unit ->* RepoEntity.Selection.t
  ;;

  let find_selectable pool =
    Utils.Database.collect (Database.Label.value pool) find_selectable_request
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
  let open Utils.Lwt_result.Infix in
  let* tenant = Sql.find pool id in
  let%lwt logos = LogoMappingRepo.find_by_tenant id in
  set_logos tenant logos |> Lwt.return_ok
;;

let find_by_label pool label =
  let open Utils.Lwt_result.Infix in
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

let find_gtx_api_key_by_label_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      gtx_api_key
    FROM
      pool_tenant
    WHERE
    pool_tenant.database_label = ?
  |sql}
  |> Caqti_type.(string ->! RepoEntity.GtxApiKey.t)
;;

let find_gtx_api_key_by_label pool database_label =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_gtx_api_key_by_label_request
    (Pool_database.Label.value database_label)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.GtxApiKey)
;;

module Url = struct
  let of_pool = RepoEntity.Url.of_pool
end
