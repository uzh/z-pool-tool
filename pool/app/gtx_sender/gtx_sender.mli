module Id : sig
  include Pool_model.Base.IdSig
end

module ApiKey : sig
  include Pool_model.Base.StringSig
end

module Sender : sig
  include Pool_model.Base.StringSig
end

type t =
  { id : Id.t
  ; api_key : ApiKey.t
  ; sender : Sender.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val create : ?id:Id.t -> ApiKey.t -> Sender.t -> t

type event =
  | Created of t
  | Updated of t * t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : ?user_uuid:Pool_common.Id.t -> Database.Label.t -> event -> unit Lwt.t
val find_exn : Database.Label.t -> t Lwt.t
val find_opt : Database.Label.t -> t option Lwt.t
val clear_cache : unit -> unit
