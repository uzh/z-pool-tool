module Content : sig
  type t

  val render : string -> (string * string) list -> t
  val value : t -> string
end

type t =
  { recipient : Pool_user.PhoneNumber.t
  ; sender : Pool_tenant.Title.t
  ; text : Content.t
  }

val create : Pool_user.PhoneNumber.t -> Pool_tenant.Title.t -> Content.t -> t

val render_and_create
  :  Pool_user.PhoneNumber.t
  -> Pool_tenant.Title.t
  -> string * (string * string) list
  -> t

module Service : sig
  val send : Pool_database.Label.t -> t -> unit Lwt.t
end

type event =
  | Sent of t
  | BulkSent of t list

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
