module Id = Pool_common.Id
module Database = Pool_common.Database
module CreatedAt = Pool_common.CreatedAt
module UpdatedAt = Pool_common.UpdatedAt
module File = Pool_common.File
module SmtpAuth = Entity_smtp_auth
module LogoMapping = Entity_logo_mapping
module PoolError = Pool_common.Error

module Title = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create title =
    if String.length title <= 0
    then Error PoolError.(Invalid Title)
    else Ok title
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Error.Title)
      CCList.pure
      "title"
  ;;
end

module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create description =
    if String.length description <= 0
    then Error PoolError.(Invalid Description)
    else Ok description
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Error.Description)
      CCList.pure
      "description"
  ;;
end

module Url = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create url =
    if String.length url <= 0 then Error PoolError.(Invalid Url) else Ok url
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Error.Url)
      CCList.pure
      "url"
  ;;
end

module Styles = struct
  type t = File.t [@@deriving eq, show]

  let value m = m

  module Write = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create styles =
      if String.length styles <= 0
      then Error PoolError.(Invalid Styles)
      else Ok styles
    ;;

    let schema () =
      Conformist.custom
        Pool_common.(Utils.schema_decoder create Error.Styles)
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
      if String.length icon <= 0
      then Error PoolError.(Invalid Icon)
      else Ok icon
    ;;

    let schema () =
      Conformist.custom
        Pool_common.(Utils.schema_decoder create Error.Icon)
        CCList.pure
        "icon"
    ;;
  end
end

module Logos = struct
  type t = File.t list [@@deriving eq, show]

  let value m = m
  let create m = Ok (CCList.map Pool_common.Id.of_string m)

  let schema () =
    Conformist.custom
      (fun l -> l |> create)
      (fun l -> l |> CCList.map Pool_common.Id.value)
      "tenant_logo"
  ;;
end

module PartnerLogos = struct
  type t = File.t list [@@deriving eq, show]

  let create m = Ok (CCList.map Pool_common.Id.of_string m)
  let value m = m

  let schema () =
    Conformist.custom
      (fun l -> l |> create)
      (fun l -> l |> CCList.map Pool_common.Id.value)
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
      Pool_common.(
        Utils.schema_decoder
          (fun m -> m |> of_string |> CCResult.pure)
          Error.TenantMaintenanceFlag)
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
      Pool_common.(
        Utils.schema_decoder
          (fun m -> m |> of_string |> CCResult.pure)
          Error.TenantDisabledFlag)
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
  ; default_language : Settings.Language.t
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
    ; default_language : Settings.Language.t
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
    ; default_language : Settings.Language.t
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
  let url (m : t) = m.url
  let label (m : t) : Database.Label.t = m.database_label
end

(* The system should proactively report degraded health to operators *)
module StatusReport = struct
  module CreatedAt = struct
    type t = Ptime.t [@@deriving eq, show]
  end

  type t = { created_at : CreatedAt.t } [@@deriving eq, show]
end
