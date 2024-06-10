open Caqti_request.Infix
module RepoEntity = Repo_entity
module Database = Database
module Id = Pool_common.Id
module LogoMapping = Entity_logo_mapping
module LogoMappingRepo = Repo_logo_mapping

module Cache : sig
  val lru_find_by_url
    : (Database.Label.t * Entity.Url.t, Entity.t option Lwt.t) CCCache.t

  val clear : unit -> unit
end = struct
  open CCCache

  let equal_find_by_url (label1, (url1 : Entity.Url.t)) (label2, url2) =
    Database.Label.equal label1 label2 && Entity.Url.equal url1 url2
  ;;

  let lru_find_by_url =
    lru ~eq:equal_find_by_url Database.Config.expected_databases
  ;;

  let clear () = clear lru_find_by_url
end

module Sql = struct
  let update_request =
    {sql|
      UPDATE pool_tenant
      JOIN pool_tenant_databases ON pool_tenant_databases.label = pool_tenant.database_label
      SET
        pool_tenant.title = $2,
        pool_tenant.description = $3,
        pool_tenant.url = $4,
        pool_tenant.default_language = $5,
        pool_tenant.gtx_sender = $6,
        pool_tenant.created_at = $7,
        pool_tenant.updated_at = $8,
        pool_tenant.database_label = $9,
        pool_tenant.styles = UNHEX(REPLACE($10, '-', '')),
        pool_tenant.icon = UNHEX(REPLACE($11, '-', '')),
        pool_tenant.gtx_api_key = $12
      WHERE
        pool_tenant.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Write.t ->. Caqti_type.unit
  ;;

  let update pool = Database.exec pool update_request

  let sql_select_storage_handle_columns ~alias =
    let uuid = Id.sql_select_fragment ~field:[%string "%{alias}.uuid"] in
    let prefix field = CCString.concat "." [ alias; field ] in
    function
    | `Write -> [ uuid ]
    | `Read ->
      [ uuid
      ; prefix "filename"
      ; prefix "filesize"
      ; prefix "mime"
      ; prefix "created"
      ; prefix "updated"
      ]
  ;;

  let sql_select_columns =
    let base =
      [ Id.sql_select_fragment ~field:"pool_tenant.uuid"
      ; "pool_tenant.title"
      ; "pool_tenant.description"
      ; "pool_tenant.url"
      ; "pool_tenant.default_language"
      ; "pool_tenant.gtx_sender"
      ; "pool_tenant.created_at"
      ; "pool_tenant.updated_at"
      ]
    in
    function
    | `Read -> base @ [ "pool_tenant_databases.status" ]
    | `Write -> base
  ;;

  let joins =
    let database_join =
      Database.Repo.sql_database_join_on_label
        ~status:`All
        ~join_prefix:"INNER"
        "pool_tenant.database_label"
    in
    [%string
      {sql|
        %{database_join}
        LEFT JOIN storage_handles styles
          ON pool_tenant.styles = styles.uuid
        LEFT JOIN storage_handles icon
          ON pool_tenant.icon = icon.uuid
      |sql}]
  ;;

  let select_from_tenants_sql where_fragment kind =
    let api_key =
      match kind with
      | `Write -> "pool_tenant.gtx_api_key"
      | `Read ->
        {sql| gtx_api_key IS NOT NULL AND gtx_api_key <> "" AS text_messages_enabled |sql}
    in
    let columns =
      sql_select_columns kind
      @ [ Database.Repo.sql_select_label ]
      @ sql_select_storage_handle_columns ~alias:"styles" kind
      @ sql_select_storage_handle_columns ~alias:"icon" kind
      @ [ api_key ]
      |> CCString.concat ",\n"
    in
    [%string
      {sql|
        SELECT
          %{columns}
        FROM pool_tenant
        %{joins}
        %{where_fragment}
      |sql}]
  ;;

  let find_fragment =
    [%string {sql| WHERE pool_tenant.uuid = %{Id.sql_value_fragment "?"} |sql}]
  ;;

  let find_request =
    select_from_tenants_sql find_fragment `Read
    |> Pool_common.Repo.Id.t ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_request id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Tenant)
  ;;

  let find_full_request =
    select_from_tenants_sql find_fragment `Write
    |> RepoEntity.Id.t ->! RepoEntity.Write.t
  ;;

  let find_full pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_full_request id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Tenant)
  ;;

  let find_by_label_request =
    select_from_tenants_sql
      {sql| WHERE pool_tenant.database_label = ? |sql}
      `Read
    |> Database.Repo.Label.t ->! RepoEntity.t
  ;;

  let find_by_label pool label =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_by_label_request label
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Tenant)
  ;;

  let find_by_url_request =
    select_from_tenants_sql {sql| WHERE pool_tenant.url = ? |sql} `Read
    |> RepoEntity.(Url.t ->! t)
  ;;

  let find_by_url pool = Database.find_opt pool find_by_url_request

  let find_all_request =
    select_from_tenants_sql "" `Read |> Caqti_type.unit ->* RepoEntity.t
  ;;

  let find_all pool = Database.collect pool find_all_request

  let insert_request =
    [%string
      {sql|
        INSERT INTO pool_tenant (
          uuid,
          title,
          description,
          url,
          default_language,
          gtx_sender,
          created_at,
          updated_at,
          database_label,
          styles,
          icon,
          gtx_api_key
        ) VALUES (
          %{Id.sql_value_fragment "$1"},
          $2,
          $3,
          $4,
          $5,
          $6,
          $7,
          $8,
          $9,
          %{Id.sql_value_fragment "$10"},
          %{Id.sql_value_fragment "$11"},
          $12
        )
      |sql}]
    |> RepoEntity.Write.t ->. Caqti_type.unit
  ;;

  let insert pool = Database.exec pool insert_request
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
    ; gtx_sender = tenant.gtx_sender
    ; styles = tenant.styles
    ; icon = tenant.icon
    ; logos = tenant_logos
    ; partner_logo
    ; status = tenant.status
    ; default_language = tenant.default_language
    ; text_messages_enabled = tenant.text_messages_enabled
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

let find_by_url pool url =
  let open Utils.Lwt_result.Infix in
  let cb ~in_cache _ _ =
    if in_cache
    then (
      let tags = Database.Logger.Tags.create pool in
      Logs.debug (fun m ->
        m ~tags "Found in cache: Tenant %s" ([%show: Entity.Url.t] url)))
    else ()
  in
  let find_by_url' (pool, url) : Entity.t option Lwt.t =
    Database.query pool (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let combine = function
        | Some ({ Entity.Read.id; _ } as tenant) ->
          id
          |> Connection.collect_list Repo_logo_mapping.Sql.find_request
          ||> CCResult.to_opt
          ||> CCOption.map (set_logos tenant)
        | None -> Lwt.return_none
      in
      Connection.find_opt Sql.find_by_url_request url |>> combine)
  in
  (pool, url)
  |> CCCache.(with_cache ~cb Cache.lru_find_by_url find_by_url')
  ||> CCOption.to_result Pool_message.Error.SessionTenantNotFound
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

let insert pool (tenant, database) =
  let open Database in
  transaction_iter
    pool
    [ exec_query Repo.insert_request database
    ; exec_query Sql.insert_request tenant
    ]
;;

let update = Sql.update

let update_database pool (tenant, database) =
  let open Database in
  transaction_iter
    pool
    [ exec_query
        Database.Repo.update_request
        (tenant.Entity.Write.database_label, database)
    ; exec_query
        Sql.update_request
        { tenant with
          Entity.Write.database_label = Database.label database
        ; updated_at = Pool_common.UpdatedAt.create_now ()
        }
    ]
;;

let find_gtx_api_key_by_label_request =
  {sql|
    SELECT
      gtx_api_key
    FROM
      pool_tenant
    WHERE
      pool_tenant.database_label = ?
  |sql}
  |> Database.Repo.Label.t ->! RepoEntity.GtxApiKey.t
;;

let find_gtx_api_key_by_label pool database_label =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_gtx_api_key_by_label_request database_label
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.GtxApiKey)
;;
