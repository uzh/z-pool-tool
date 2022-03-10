module Common = Pool_common
module Database = Pool_database
module Id = Common.Id
module CreatedAt = Common.CreatedAt
module UpdatedAt = Common.UpdatedAt
module File = Common.File
module SmtpAuth = Entity_smtp_auth
module LogoMapping = Entity_logo_mapping
module PoolError = Common.Message

module Title = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create title =
    if CCString.is_empty title
    then Error PoolError.(Invalid Title)
    else Ok title
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.Title "title"
  ;;
end

module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create description =
    if CCString.is_empty description
    then Error PoolError.(Invalid Description)
    else Ok description
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder
      create
      value
      PoolError.Description
      "description"
  ;;
end

module Url = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create url =
    if CCString.is_empty url then Error PoolError.(Invalid Url) else Ok url
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.Url "url"
  ;;
end

module Styles = struct
  type t = File.t [@@deriving eq, show]

  let value m = m
  let id m = m.File.id
  let mime_type m = m.File.mime_type

  module Write = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create styles =
      if CCString.is_empty styles
      then Error PoolError.(Invalid Styles)
      else Ok styles
    ;;

    let schema () =
      Pool_common.Utils.schema_decoder create value PoolError.Styles "styles"
    ;;
  end
end

module Icon = struct
  type t = File.t [@@deriving eq, show]

  let value m = m

  module Write = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create icon =
      if CCString.is_empty icon then Error PoolError.(Invalid Icon) else Ok icon
    ;;

    let schema () =
      Pool_common.Utils.schema_decoder create value PoolError.Icon "icon"
    ;;
  end
end

module Logos = struct
  type t = File.t list [@@deriving eq, show]

  let value m = m
  let create m = Ok (CCList.map Common.Id.of_string m)

  let schema () =
    Pool_common.Utils.schema_list_decoder
      create
      (CCList.map Common.Id.value)
      "tenant_logo"
  ;;
end

module PartnerLogos = struct
  type t = File.t list [@@deriving eq, show]

  let create m = Ok (CCList.map Common.Id.of_string m)
  let value m = m

  let schema () =
    Pool_common.Utils.schema_list_decoder
      create
      (fun l -> l |> CCList.map Common.Id.value)
      "partner_logo"
  ;;
end

module Maintenance = struct
  type t = bool [@@deriving eq, show]

  let create t = t

  let stringify = function
    | true -> "true"
    | false -> "false"
  ;;

  let of_string = function
    | "true" -> true
    | _ -> false
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder
      (fun m -> Ok (of_string m))
      stringify
      PoolError.TenantMaintenanceFlag
      "maintenance"
  ;;
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let create t = t
  let value m = m

  let stringify = function
    | true -> "true"
    | false -> "false"
  ;;

  let of_string = function
    | "true" -> true
    | _ -> false
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder
      (fun m -> Ok (of_string m))
      stringify
      PoolError.TenantDisabledFlag
      "disabled"
  ;;
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database_label : Database.Label.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logo : PartnerLogos.t
  ; maintenance : Maintenance.t
  ; disabled : Disabled.t
  ; default_language : Common.Language.t
  ; created_at : CreatedAt.t
  ; updated_at : UpdatedAt.t
  }
[@@deriving eq, show]

module Read = struct
  type t =
    { id : Id.t
    ; title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database_label : Database.Label.t
    ; smtp_auth : SmtpAuth.t
    ; styles : Styles.t
    ; icon : Icon.t
    ; maintenance : Maintenance.t
    ; disabled : Disabled.t
    ; default_language : Common.Language.t
    ; created_at : CreatedAt.t
    ; updated_at : UpdatedAt.t
    }
  [@@deriving eq, show]
end

module Write = struct
  type t =
    { id : Id.t
    ; title : Title.t
    ; description : Description.t
    ; url : Url.t
    ; database : Database.t
    ; smtp_auth : SmtpAuth.Write.t
    ; styles : Styles.Write.t
    ; icon : Icon.Write.t
    ; maintenance : Maintenance.t
    ; disabled : Disabled.t
    ; default_language : Common.Language.t
    ; created_at : CreatedAt.t
    ; updated_at : CreatedAt.t
    }
  [@@deriving eq, show]

  let create
      title
      description
      url
      database
      smtp_auth
      styles
      icon
      default_language
    =
    { id = Id.create ()
    ; title
    ; description
    ; url
    ; database
    ; smtp_auth
    ; styles
    ; icon
    ; maintenance = Maintenance.create false
    ; disabled = Disabled.create false
    ; default_language
    ; created_at = CreatedAt.create ()
    ; updated_at = UpdatedAt.create ()
    }
  ;;
end

module Selection = struct
  type t =
    { url : Url.t
    ; database_label : Database.Label.t
    }
  [@@deriving eq, show]

  let create url database_label = { url; database_label }
  let url ({ url; _ } : t) = url |> Url.value
  let label ({ database_label; _ } : t) = database_label
end

module Context = struct
  open Sexplib.Conv

  type t =
    { query_language : Pool_common.Language.t option
    ; language : Pool_common.Language.t
    ; tenant_db : Pool_database.Label.t
    }
  [@@deriving sexp_of]

  let create (query_language, language, tenant_db) =
    { query_language; language; tenant_db }
  ;;

  let key : t Opium.Context.key =
    Opium.Context.Key.create ("tenant context", sexp_of_t)
  ;;

  let find req =
    Opium.Context.find key req.Opium.Request.env
    |> CCOption.to_result Pool_common.Message.TenantContextNotFound
    |> CCResult.map_err (fun err -> Pool_common.Utils.with_log_error err)
  ;;

  let find_exn req =
    match Opium.Context.find key req.Opium.Request.env with
    | Some context -> context
    | None -> failwith "Cannot find tenant context."
  ;;

  let set req context =
    let env = req.Opium.Request.env in
    let env = Opium.Context.add key context env in
    Opium.Request.{ req with env }
  ;;
end
