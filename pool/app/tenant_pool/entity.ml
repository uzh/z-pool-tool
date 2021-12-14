module Common = Pool_common
module Database = Database_pool
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
    Conformist.custom
      Common.(Utils.schema_decoder create Message.Title)
      CCList.pure
      "title"
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
    Conformist.custom
      Common.(Utils.schema_decoder create Message.Description)
      CCList.pure
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
    Conformist.custom
      Common.(Utils.schema_decoder create PoolError.Url)
      CCList.pure
      "url"
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
      Conformist.custom
        Common.(Utils.schema_decoder create Message.Styles)
        CCList.pure
        "styles"
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
      Conformist.custom
        Common.(Utils.schema_decoder create Message.Icon)
        CCList.pure
        "icon"
    ;;
  end
end

module Logos = struct
  type t = File.t list [@@deriving eq, show]

  let value m = m
  let create m = Ok (CCList.map Common.Id.of_string m)

  let schema () =
    Conformist.custom
      (fun l -> l |> create)
      (fun l -> l |> CCList.map Common.Id.value)
      "tenant_logo"
  ;;
end

module PartnerLogos = struct
  type t = File.t list [@@deriving eq, show]

  let create m = Ok (CCList.map Common.Id.of_string m)
  let value m = m

  let schema () =
    Conformist.custom
      (fun l -> l |> create)
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
    Conformist.custom
      Common.(
        Utils.schema_decoder
          (fun m -> m |> of_string |> CCResult.pure)
          Message.TenantMaintenanceFlag)
      (fun l -> l |> stringify |> CCList.pure)
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
    Conformist.custom
      Common.(
        Utils.schema_decoder
          (fun m -> m |> of_string |> CCResult.pure)
          Message.TenantDisabledFlag)
      (fun l -> l |> stringify |> CCList.pure)
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
