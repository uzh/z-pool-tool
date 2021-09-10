module Title = struct
  type t = string [@@deriving eq, show]
end

module Description = struct
  type t = string [@@deriving eq, show]
end

module Url = struct
  type t = string [@@deriving eq, show]
end

module Database = struct
  type t = string [@@deriving eq, show]
end

module Styles = struct
  type t = string [@@deriving eq, show]
end

module Icon = struct
  type t = string [@@deriving eq, show]
end

module Logos = struct
  type t = string [@@deriving eq, show]
end

module PartnerLogos = struct
  type t = string [@@deriving eq, show]
end

module Disabled = struct
  type t = bool [@@deriving eq, show]
end

type t =
  { id : Common.Id.t
  ; title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database : Database.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogos.t
  ; disabled : Disabled.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]
