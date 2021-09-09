module SmtpAuth = Entity_smtp_auth

module Id = struct
  type t = string [@@deriving eq, show]

  let create () = Uuidm.create `V4 |> Uuidm.to_string
  let to_human id = id
  let t = Caqti_type.string
end

module Title = struct
  type t = string [@@deriving eq, show]

  let create title =
    if String.length title <= 0 then Error "Invalid title!" else Ok title
  ;;

  let t = Caqti_type.string
end

module Description = struct
  type t = string [@@deriving eq, show]

  let create description =
    if String.length description <= 0
    then Error "Invalid description!"
    else Ok description
  ;;

  let t = Caqti_type.string
end

module Url = struct
  type t = string [@@deriving eq, show]

  let create url =
    if String.length url <= 0 then Error "Invalid url!" else Ok url
  ;;

  let t = Caqti_type.string
end

module Database = struct
  type t = string [@@deriving eq, show]

  let create database =
    if String.length database <= 0
    then Error "Invalid database!"
    else Ok database
  ;;

  let t = Caqti_type.string
end

module Styles = struct
  type t = string [@@deriving eq, show]

  let create styles =
    if String.length styles <= 0 then Error "Invalid styles!" else Ok styles
  ;;

  let t = Caqti_type.string
end

module Icon = struct
  type t = string [@@deriving eq, show]

  let create icon =
    if String.length icon <= 0 then Error "Invalid icon!" else Ok icon
  ;;

  let t = Caqti_type.string
end

module Logos = struct
  type t = string [@@deriving eq, show]

  let create logos =
    if String.length logos <= 0 then Error "Invalid logos!" else Ok logos
  ;;

  let t = Caqti_type.string
end

module PartnerLogo = struct
  type t = string [@@deriving eq, show]

  let create partner_logo =
    if String.length partner_logo <= 0
    then Error "Invalid partner logo!"
    else Ok partner_logo
  ;;

  let t = Caqti_type.string
end

module Maintenance = struct
  type t = bool [@@deriving eq, show]

  let create t = Ok t
  let t = Caqti_type.bool
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let create t = Ok t
  let t = Caqti_type.bool
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database : Database.t
  ; smtp_auth : SmtpAuth.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogo.t
  ; maintenance : Maintenance.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

let create
    ?id
    title
    description
    url
    database
    smtp_auth
    styles
    icon
    logos
    partner_logos
    default_language
    ()
  =
  { id = id |> Option.value ~default:(Id.create ())
  ; title
  ; description
  ; url
  ; database
  ; smtp_auth
  ; styles
  ; icon
  ; logos
  ; partner_logos
  ; maintenance = false
  ; disabled = false
  ; default_language
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
;;

(* The system should proactively report degraded health to operators *)
module StatusReport = struct
  module CreatedAt = struct
    type t = Ptime.t [@@deriving eq, show]
  end

  type t = { created_at : CreatedAt.t } [@@deriving eq, show]
end
