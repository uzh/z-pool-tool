module Id = Pool_common.Id
module Database = Pool_common.Database
module CreatedAt = Pool_common.CreatedAt
module UpdatedAt = Pool_common.UpdatedAt
module File = Pool_common.File
module SmtpAuth = Entity_smtp_auth
module LogoMapping = Entity_logo_mapping

module Title = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create title =
    if String.length title <= 0 then Error "Invalid title!" else Ok title
  ;;

  let schema () =
    Conformist.custom (Utils.schema_decoder create "title") CCList.pure "title"
  ;;
end

module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create description =
    if String.length description <= 0
    then Error "Invalid description!"
    else Ok description
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "description")
      CCList.pure
      "description"
  ;;
end

module Url = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create url =
    if String.length url <= 0 then Error "Invalid url!" else Ok url
  ;;

  let schema () =
    Conformist.custom (Utils.schema_decoder create "url") CCList.pure "url"
  ;;
end

module Styles = struct
  type t = File.t [@@deriving eq, show]

  let value m = m

  module Write = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create styles =
      if String.length styles <= 0 then Error "Invalid styles!" else Ok styles
    ;;

    let schema () =
      Conformist.custom
        (Utils.schema_decoder create "styles")
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
      if String.length icon <= 0 then Error "Invalid icon!" else Ok icon
    ;;

    let schema () =
      Conformist.custom (Utils.schema_decoder create "icon") CCList.pure "icon"
    ;;
  end
end

module Logos = struct
  type t = File.t list [@@deriving eq, show]
  type create = File.t list -> t

  let value m = m
end

module PartnerLogos = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create partner_logo =
    if String.length partner_logo <= 0
    then Error "Invalid partner logos!"
    else Ok partner_logo
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "partner logos")
      CCList.pure
      "partner_logos"
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
      (Utils.schema_decoder
         (fun l -> Ok (of_string l))
         "tenant maintenance flag")
      (fun l -> [ stringify l ])
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
      (Utils.schema_decoder (fun l -> Ok (of_string l)) "tenant disabled flag")
      (fun l -> [ stringify l ])
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
  ; partner_logos : PartnerLogos.t
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
    ; partner_logos : PartnerLogos.t
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
    ; partner_logos : PartnerLogos.t
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
      partner_logos
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
    ; partner_logos
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
