module Id = struct
  type t = string [@@deriving eq, show]

  let to_human id = id
  let t = Caqti_type.string
end

module Title = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Description = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Url = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Database = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Styles = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Icon = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Logos = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module PartnerLogo = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let t = Caqti_type.bool
end

(* Peropheral Systems

   - Do we need an email address ( & phone number ) as sender for emails/sms? *)

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; url : Url.t
  ; database : Database.t
  ; styles : Styles.t
  ; icon : Icon.t
  ; logos : Logos.t
  ; partner_logos : PartnerLogo.t
  ; disabled : Disabled.t
  ; default_language : Settings.Language.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

module StatusReport = struct
  module CreatedAt = struct
    type t = Ptime.t [@@deriving eq, show]
  end

  type t = { created_at : CreatedAt.t } [@@deriving eq, show]
end
