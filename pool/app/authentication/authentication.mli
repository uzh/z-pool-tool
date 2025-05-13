module Id : sig
  include module type of Pool_common.Id

  val schema : unit -> (Pool_conformist.error_msg, t) Pool_conformist.Field.t
end

module Channel : sig
  type t = Email
end

module Token : sig
  type t

  val equal : t -> t -> bool
  val generate : unit -> t
  val value : t -> string
  val schema : unit -> (Pool_conformist.error_msg, t) Pool_conformist.Field.t
end

type t =
  { id : Id.t
  ; user_uuid : Pool_user.Id.t
  ; channel : Channel.t
  ; token : Token.t
  }

val equal : t -> t -> bool
val show : t -> string
val create : user:Pool_user.t -> channel:Channel.t -> t

type event =
  | Created of t
  | Deleted of t

val equal_event : event -> event -> bool
val show_event : event -> string
val pp_event : Format.formatter -> event -> unit
val handle_event : Database.Label.t -> event -> unit Lwt.t

val find_valid_by_id
  :  Database.Label.t
  -> Id.t
  -> (t * Pool_user.t, Pool_message.Error.t) Lwt_result.t
