module Conformist = Pool_common.Utils.PoolConformist
module Id = Pool_tenant.Id
module File = Pool_common.File

let src = Logs.Src.create "pool_tenant.cqrs"

let create_logo_mappings files tenant logo_type =
  let open Pool_tenant in
  CCList.map
    (fun asset_id ->
      LogoMapping.Write.
        { id = Pool_common.Id.create ()
        ; tenant_id = tenant.Write.id
        ; asset_id
        ; logo_type
        })
    files
;;

module Create : sig
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t option
    ; url : Pool_tenant.Url.t
    ; database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    ; styles : Pool_tenant.Styles.Write.t
    ; icon : Pool_tenant.Icon.Write.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Pool_common.Id.t list
    ; partner_logos : Pool_common.Id.t list
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t option
    ; url : Pool_tenant.Url.t
    ; database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    ; styles : Pool_tenant.Styles.Write.t
    ; icon : Pool_tenant.Icon.Write.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Pool_common.Id.t list
    ; partner_logos : Pool_common.Id.t list
    }

  let command
    title
    description
    url
    database_url
    database_label
    styles
    icon
    default_language
    tenant_logos
    partner_logos
    =
    { title
    ; description
    ; url
    ; database_url
    ; database_label
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
          ; Pool_database.Url.schema ()
          ; Pool_database.Label.schema ()
          ; Pool_tenant.Styles.Write.schema ()
          ; Pool_tenant.Icon.Write.schema ()
          ; Pool_common.Language.schema ()
          ; Pool_tenant.Logos.schema ()
          ; Pool_tenant.PartnerLogos.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) (command : t) =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let database =
      Pool_database.
        { url = command.database_url; label = command.database_label }
    in
    let tenant =
      Pool_tenant.Write.create
        command.title
        command.description
        command.url
        database
        command.styles
        command.icon
        command.default_language
    in
    let logo_mappings =
      CCList.map
        (fun (id_list, logo_type) ->
          create_logo_mappings id_list tenant logo_type)
        [ command.partner_logos, Pool_tenant.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Pool_tenant.LogoMapping.LogoType.TenantLogo
        ]
      |> CCList.flatten
    in
    Ok
      [ Pool_tenant.Created tenant |> Pool_event.pool_tenant
      ; Pool_tenant.LogosUploaded logo_mappings |> Pool_event.pool_tenant
      ; Database.Added database |> Pool_event.database
      ; Database.Migrated command.database_label |> Pool_event.database
      ; Settings.(DefaultRestored default_values) |> Pool_event.settings
      ; I18n.(DefaultRestored default_values) |> Pool_event.i18n
      ; Message_template.(
          DefaultRestored default_values_tenant |> Pool_event.message_template)
      ; Guard.(DefaultRestored root_permissions) |> Pool_event.guard
      ]
  ;;

  let effects = [ `Create, `TargetEntity `Tenant ]

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module EditDetails : sig
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t option
    ; url : Pool_tenant.Url.t
    ; disabled : Pool_tenant.Disabled.t
    ; default_language : Pool_common.Language.t
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

  val effects : Pool_tenant.Write.t -> Guard.Authorizer.effect list
end = struct
  type t =
    { title : Pool_tenant.Title.t
    ; description : Pool_tenant.Description.t option
    ; url : Pool_tenant.Url.t
    ; disabled : Pool_tenant.Disabled.t
    ; default_language : Pool_common.Language.t
    ; tenant_logos : Pool_common.Id.t list option
    ; partner_logos : Pool_common.Id.t list option
    }

  let command
    title
    description
    url
    disabled
    default_language
    tenant_logos
    partner_logos
    =
    { title
    ; description
    ; url
    ; disabled
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
          ; Pool_tenant.Disabled.schema ()
          ; Pool_common.Language.schema ()
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
        ; default_language = command.default_language
        }
    in
    let logo_mappings =
      CCList.map
        (fun (id_list, logo_type) ->
          id_list
          |> CCOption.map (fun ids -> create_logo_mappings ids tenant logo_type)
          |> CCOption.value ~default:[])
        [ command.partner_logos, Pool_tenant.LogoMapping.LogoType.PartnerLogo
        ; command.tenant_logos, Pool_tenant.LogoMapping.LogoType.TenantLogo
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

  let effects { Pool_tenant.Write.id; _ } =
    [ `Update, `Target (id |> Guard.Uuid.target_of Id.value)
    ; `Update, `TargetEntity `Tenant
    ]
  ;;
end

module EditDatabase : sig
  type t =
    { database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.Write.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects
    :  Pool_database.Label.t
    -> (Guard.Authorizer.effect list, Pool_common.Message.error) Lwt_result.t
end = struct
  type t =
    { database_url : Pool_database.Url.t
    ; database_label : Pool_database.Label.t
    }

  let command database_url database_label = { database_url; database_label }

  let schema =
    Conformist.(
      make
        Field.[ Pool_database.Url.schema (); Pool_database.Label.schema () ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    (tenant : Pool_tenant.Write.t)
    (command : t)
    =
    Logs.info ~src (fun m -> m "Handle command EditDatabase" ~tags);
    let database =
      Pool_database.
        { url = command.database_url; label = command.database_label }
    in
    Ok
      [ Pool_tenant.DatabaseEdited (tenant, database) |> Pool_event.pool_tenant
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects dblabel =
    let open Utils.Lwt_result.Infix in
    let* tenant = Pool_tenant.find_by_label dblabel in
    Lwt.return_ok
      [ `Update, `Target (tenant.Pool_tenant.id |> Guard.Uuid.target_of Id.value)
      ; `Update, `TargetEntity `Tenant
      ]
  ;;
end

module DestroyLogo : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_tenant.t
    -> Pool_common.Id.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  let handle ?(tags = Logs.Tag.empty) tenant asset_id =
    Logs.info ~src (fun m -> m "Handle command DestroyLogo" ~tags);
    Ok [ Pool_tenant.LogoDeleted (tenant, asset_id) |> Pool_event.pool_tenant ]
  ;;

  let effects = [ `Update, `TargetEntity `Tenant ]
end

module Destroy : sig
  (* TODO: Type safety *)
  type t = { tenant_id : string }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : t -> Guard.Authorizer.effect list
end = struct
  type t = { tenant_id : string }

  let handle ?(tags = Logs.Tag.empty) t =
    Logs.info ~src (fun m -> m "Handle command Destroy" ~tags);
    Ok
      [ Pool_tenant.Destroyed (t.tenant_id |> Id.of_string)
        |> Pool_event.pool_tenant
      ]
  ;;

  let effects { tenant_id } =
    [ `Delete, `Target (Guard.Uuid.Target.of_string_exn tenant_id) ]
  ;;
end
