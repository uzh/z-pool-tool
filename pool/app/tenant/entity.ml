module SmtpAuth = Entity_smtp_auth

module Title : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create title =
    if String.length title <= 0 then Error "Invalid title!" else Ok title
  ;;
end

module Description : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create description =
    if String.length description <= 0
    then Error "Invalid description!"
    else Ok description
  ;;
end

module Url : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create url =
    if String.length url <= 0 then Error "Invalid url!" else Ok url
  ;;
end

module Database : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create database =
    if String.length database <= 0
    then Error "Invalid database!"
    else Ok database
  ;;
end

module Styles : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create styles =
    if String.length styles <= 0 then Error "Invalid styles!" else Ok styles
  ;;
end

module Icon : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create icon =
    if String.length icon <= 0 then Error "Invalid icon!" else Ok icon
  ;;
end

module Logos : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create logos =
    if String.length logos <= 0 then Error "Invalid logos!" else Ok logos
  ;;
end

module PartnerLogo : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create partner_logo =
    if String.length partner_logo <= 0
    then Error "Invalid partner logo!"
    else Ok partner_logo
  ;;
end

module Maintenance : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : bool -> t
  val value : t -> bool
  val stringify : t -> string
end = struct
  type t = bool [@@deriving eq, show]

  let create t = t
  let value m = m

  let stringify = function
    | true -> "true"
    | false -> "false"
  ;;
end

module Disabled : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : bool -> t
  val value : t -> bool
  val stringify : t -> string
end = struct
  type t = bool [@@deriving eq, show]

  let create t = t
  let value m = m

  let stringify = function
    | true -> "true"
    | false -> "false"
  ;;
end

type t =
  { id : Common.Id.t
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
  { id = id |> Option.value ~default:(Common.Id.create ())
  ; title
  ; description
  ; url
  ; database
  ; smtp_auth
  ; styles
  ; icon
  ; logos
  ; partner_logos
  ; maintenance = Maintenance.create false
  ; disabled = Disabled.create false
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
