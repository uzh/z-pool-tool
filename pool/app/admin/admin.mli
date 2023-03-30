module Id : module type of Pool_common.Id

type t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val sexp_of_t : t -> Sexplib0.Sexp.t
val user : t -> Sihl_user.t
val create : Sihl_user.t -> t
val id : t -> Id.t
val email : t -> string
val full_name : t -> string

type create =
  { id : Pool_common.Id.t option
  ; email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  ; roles : Guard.RoleSet.t option
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type update =
  { firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

type event =
  | Created of create
  | DetailsUpdated of t * update
  | PasswordUpdated of t * Pool_user.Password.t * Pool_user.PasswordConfirmed.t
  | Disabled of t
  | Enabled of t
  | Verified of t

val handle_event
  :  tags:Logs.Tag.set
  -> Pool_tenant.Database.Label.t
  -> event
  -> unit Lwt.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val user_is_admin : Pool_database.Label.t -> Sihl_user.t -> bool Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all : Pool_database.Label.t -> unit -> t list Lwt.t

val find_all_with_role
  :  Pool_database.Label.t
  -> Role.Actor.t list
  -> exclude:Role.Actor.t list
  -> t list Lwt.t

module Duplicate : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Guard : sig
  module Actor : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Role.Actor.t Guard.Actor.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Role.Target.t Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module RuleSet : sig
    val assistant : Pool_common.Id.t -> Guard.Rule.t list
    val experimenter : Pool_common.Id.t -> Guard.Rule.t list
    val location_manager : Pool_common.Id.t -> Guard.Rule.t list
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
  end
end
