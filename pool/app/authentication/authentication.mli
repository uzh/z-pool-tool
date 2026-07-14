module Id : sig
  include module type of Pool_common.Id

  val schema : unit -> (Pool_conformist.error_msg, t) Pool_conformist.Field.t
end

module Channel : sig
  type t = Email

  val equal : t -> t -> bool
  val show : t -> string
end

module Token : sig
  type t

  val length : int
  val equal : t -> t -> bool
  val generate : unit -> t
  val value : t -> string
  val schema : unit -> (Pool_conformist.error_msg, t) Pool_conformist.Field.t
end

module UsageCount : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> int
  val of_int : int -> t
  val limit : t
end

type t =
  { id : Id.t
  ; user_uuid : Pool_user.Id.t
  ; channel : Channel.t
  ; token : Token.t
  ; usage_count : UsageCount.t
  }

val resend_cooldown_seconds : int
val equal : t -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit

val create
  :  ?id:Id.t
  -> ?token:Token.t
  -> user:Pool_user.t
  -> channel:Channel.t
  -> unit
  -> t

type event =
  | Created of t
  | Deleted of Id.t
  | IncreaseUsageCount of t
  | ResetExpired

val equal_event : event -> event -> bool
val show_event : event -> string
val pp_event : Format.formatter -> event -> unit
val handle_event : Database.Label.t -> event -> unit Lwt.t

val find_valid_by_id
  :  Database.Label.t
  -> Id.t
  -> (t * Pool_user.t, Pool_message.Error.t) Lwt_result.t

val find_id_by_user : Database.Label.t -> Pool_user.Id.t -> Id.t option Lwt.t
val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t
