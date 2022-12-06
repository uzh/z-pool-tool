module Id : module type of Pool_common.Id

type t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val sexp_of_t : t -> Sexplib0.Sexp.t
val user : t -> Sihl_user.t
val create : Sihl_user.t -> t
val id : t -> Id.t

type create =
  { email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
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

module Duplicate : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Guard : sig
  module Actor : sig
    val to_authorizable
      :  ?ctx:Guardian__Persistence.context
      -> t
      -> ( [> `Admin ] Guard.Authorizable.t
         , Pool_common.Message.error )
         Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Target : sig
    val to_authorizable
      :  ?ctx:Guardian__Persistence.context
      -> Role__Entity.Target.admins
      -> t
      -> ( [> `Admin of Role__Entity.Target.admins ] Guard.AuthorizableTarget.t
         , Pool_common.Message.error )
         Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module RuleSet : sig
    val assistant : Pool_common.Id.t -> Guard.Authorizer.auth_rule list
    val experimenter : Pool_common.Id.t -> Guard.Authorizer.auth_rule list
    val location_manager : Pool_common.Id.t -> Guard.Authorizer.auth_rule list
  end
end
