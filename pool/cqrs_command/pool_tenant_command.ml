module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_tenant.Id
module File = Pool_common.File

let src = Logs.Src.create "pool_tenant.cqrs"

let create_logo_mappings tenant logo_type =
  let open Pool_tenant in
  CCList.map (fun asset_id ->
    LogoMapping.Write.
      { id = Pool_common.Id.create ()
      ; tenant_id = tenant.Write.id
      ; asset_id
      ; logo_type
      })
;;

type create =
  { title : Pool_tenant.Title.t
  ; description : Pool_tenant.Description.t option
  ; url : Pool_tenant.Url.t
  ; styles : Pool_tenant.Styles.Write.t option
  ; icon : Pool_tenant.Icon.Write.t option
  ; default_language : Pool_common.Language.t
  ; tenant_logos : Pool_common.Id.t list
  ; partner_logos : Pool_common.Id.t list option
  }

let system_event_from_job ?id job =
  let open System_event in
  job |> create ?id |> created |> Pool_event.system_event
;;

module Create : sig
  type t = create

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_database.t
    -> Pool_tenant.GtxApiKey.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end = struct
  type t = create

  let command
    title
    description
    url
    styles
    icon
    default_language
    tenant_logos
    partner_logos
    =
    { title
    ; description
    ; url
    ; styles
    ; icon
    ; default_language
    ; tenant_logos
    ; partner_logos
    }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Pool_tenant.Title.schema ()
          ; Conformist.optional @@ Pool_tenant.Description.schema ()
          ; Pool_tenant.Url.schema ()
          ; Conformist.optional @@ Pool_tenant.Styles.Write.schema ()
          ; Conformist.optional @@ Pool_tenant.Icon.Write.schema ()
          ; Pool_common.Language.schema ()
          ; Pool_tenant.Logos.schema ()
          ; Conformist.optional @@ Pool_tenant.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) database gtx_api_key (command : t) =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let tenant =
      Pool_tenant.Write.create
        command.title
        command.description
        command.url
        database
        gtx_api_key
        command.styles
        command.icon
        command.default_language
    in
    let logo_mappings =
      let open Pool_tenant.LogoMapping in
      CCList.filter_map
        (fun (id_list, logo_type) ->
          id_list |> CCOption.map (create_logo_mappings tenant logo_type))
        [ command.partner_logos, LogoType.PartnerLogo
        ; Some command.tenant_logos, LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Pool_tenant.Created tenant |> Pool_event.pool_tenant
      ; Pool_tenant.LogosUploaded logo_mappings |> Pool_event.pool_tenant
      ; Database.Migrated database |> Pool_event.database
      ; System_event.Job.TenantDatabaseAdded database.Pool_database.label
        |> system_event_from_job
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Pool_tenant.Guard.Access.create
end

module EditDetails : sig
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t option
    ; url : Pool_tenant.Url.t
    ; disabled : Pool_tenant.Disabled.t
    ; default_language : Pool_common.Language.t
    ; styles : Pool_tenant.Styles.Write.t option
    ; icon : Pool_tenant.Icon.Write.t option
    ; tenant_logos : Pool_common.Id.t list option
    ; partner_logos : Pool_common.Id.t list option
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t option
    ; url : Pool_tenant.Url.t
    ; disabled : Pool_tenant.Disabled.t
    ; default_language : Pool_common.Language.t
    ; styles : Pool_tenant.Styles.Write.t option
    ; icon : Pool_tenant.Icon.Write.t option
    ; tenant_logos : Pool_common.Id.t list option
    ; partner_logos : Pool_common.Id.t list option
    }

  let command
    title
    description
    url
    disabled
    default_language
    styles
    icon
    tenant_logos
    partner_logos
    =
    { title
    ; description
    ; url
    ; disabled
    ; default_language
    ; styles
    ; icon
    ; tenant_logos
    ; partner_logos
    }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Pool_tenant.Title.schema ()
          ; Conformist.optional @@ Pool_tenant.Description.schema ()
          ; Pool_tenant.Url.schema ()
          ; Pool_tenant.Disabled.schema ()
          ; Pool_common.Language.schema ()
          ; Conformist.optional @@ Pool_tenant.Styles.Write.schema ()
          ; Conformist.optional @@ Pool_tenant.Icon.Write.schema ()
          ; Conformist.optional @@ Pool_tenant.Logos.schema ()
          ; Conformist.optional @@ Pool_tenant.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    (tenant : Pool_tenant.Write.t)
    (command : t)
    =
    Logs.info ~src (fun m -> m "Handle command EditDetails" ~tags);
    let update =
      Pool_tenant.
        { title = command.title
        ; description = command.description
        ; url = command.url
        ; disabled = command.disabled
        ; styles = command.styles
        ; icon = command.icon
        ; default_language = command.default_language
        }
    in
    let logo_mappings =
      let open Pool_tenant.LogoMapping in
      CCList.filter_map
        (fun (id_list, logo_type) ->
          id_list |> CCOption.map (create_logo_mappings tenant logo_type))
        [ command.partner_logos, LogoType.PartnerLogo
        ; command.tenant_logos, LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Pool_tenant.DetailsEdited (tenant, update) |> Pool_event.pool_tenant
      ; Pool_tenant.LogosUploaded logo_mappings |> Pool_event.pool_tenant
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Pool_tenant.Guard.Access.update
end

type database_command =
  { database_url : Pool_database.Url.t
  ; database_label : Pool_database.Label.t
  }

let database_command database_url database_label =
  { database_url; database_label }
;;

let database_schema =
  Conformist.(
    make
      Field.[ Pool_database.Url.schema (); Pool_database.Label.schema () ]
      database_command)
;;

let decode_database data =
  Conformist.decode_and_validate database_schema data
  |> CCResult.map_err Pool_common.Message.to_conformist_error
;;

module UpdateDatabase : sig
  type t = Pool_database.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?system_event_id:System_event.Id.t
    -> Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (database_command, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_database.t

  let handle
    ?(tags = Logs.Tag.empty)
    ?system_event_id
    (tenant : Pool_tenant.Write.t)
    database
    =
    Logs.info ~src (fun m -> m "Handle command UpdateDatabase" ~tags);
    Ok
      [ Pool_tenant.DatabaseEdited (tenant, database) |> Pool_event.pool_tenant
      ; System_event.Job.TenantDatabaseUpdated database.Pool_database.label
        |> system_event_from_job ?id:system_event_id
      ]
  ;;

  let decode = decode_database
  let effects = Pool_tenant.Guard.Access.update
end

module UpdateGtxApiKey : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.Write.t
    -> Pool_tenant.GtxApiKey.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  let handle ?(tags = Logs.Tag.empty) (tenant : Pool_tenant.Write.t) api_key =
    Logs.info ~src (fun m -> m "Handle command UpdateGtxApiKey" ~tags);
    Ok
      [ Pool_tenant.GtxApiKeyUpdated (tenant, api_key) |> Pool_event.pool_tenant
      ]
  ;;

  let effects = Pool_tenant.Guard.Access.update
end

module DestroyLogo : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.t
    -> Pool_common.Id.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  let handle ?(tags = Logs.Tag.empty) tenant asset_id =
    Logs.info ~src (fun m -> m "Handle command DestroyLogo" ~tags);
    Ok [ Pool_tenant.LogoDeleted (tenant, asset_id) |> Pool_event.pool_tenant ]
  ;;

  let effects = Pool_tenant.Guard.Access.update
end

module Destroy : sig
  type t = Pool_tenant.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_tenant.t

  let handle ?(tags = Logs.Tag.empty) t =
    Logs.info ~src (fun m -> m "Handle command Destroy" ~tags);
    Ok
      [ Pool_tenant.Destroyed t.Pool_tenant.id |> Pool_event.pool_tenant
      ; System_event.Job.TenantDatabaseDeleted t.Pool_tenant.database_label
        |> system_event_from_job
      ]
  ;;

  let effects = Pool_tenant.Guard.Access.delete
end
