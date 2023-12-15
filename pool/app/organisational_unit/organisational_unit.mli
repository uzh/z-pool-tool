module Id : sig
  include Pool_common.Model.IdSig
end

module Name : sig
  include Pool_common.Model.StringSig
end

type t =
  { id : Id.t
  ; name : Name.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val create : ?id:Id.t -> Name.t -> t

type event =
  | Created of t
  | Updated of (t * Name.t)

val handle_event : Pool_tenant.Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
  end
end

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val all : Pool_database.Label.t -> unit -> t list Lwt.t

module Repo : sig
  val sql_select_columns : string list

  module Id : sig
    val t : Id.t Caqti_type.t
  end

  val t : t Caqti_type.t
end
