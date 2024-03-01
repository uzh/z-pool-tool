module Id : module type of Pool_common.Id

type t =
  { user : Sihl_user.t
  ; email_verified : Pool_user.EmailVerified.t option
  ; import_pending : Pool_user.ImportPending.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val sexp_of_t : t -> Sexplib0.Sexp.t
val user : t -> Sihl_user.t
val create : email_verified:Pool_user.EmailVerified.t option -> Sihl_user.t -> t
val id : t -> Id.t
val email_address : t -> Pool_user.EmailAddress.t
val full_name : t -> string
val full_name_reversed : t -> string

type create =
  { id : Id.t option
  ; email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  ; roles : (Role.Role.t * Guard.Uuid.Target.t option) list
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
  | EmailVerified of t
  | Disabled of t
  | Enabled of t
  | ImportConfirmed of t * Pool_user.Password.t
  | ImportDisabled of t
  | PasswordUpdated of
      t
      * Pool_user.Password.t
      * Pool_user.Password.t
      * Pool_user.PasswordConfirmed.t
  | PromotedContact of Pool_common.Id.t
  | SignInCounterUpdated of t

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
  -> (t, Pool_message.Error.t) result Lwt.t

val find_by_email
  :  Pool_database.Label.t
  -> Pool_user.EmailAddress.t
  -> (t, Pool_message.Error.t) result Lwt.t

val find_by
  :  ?query:Query.t
  -> Pool_database.Label.t
  -> (t list * Query.t) Lwt.t

val find_all_with_role
  :  ?exclude:(Role.Role.t * Guard.Uuid.Target.t option) list
  -> Pool_database.Label.t
  -> Role.Role.t * Guard.Uuid.Target.t option
  -> t list Lwt.t

val find_all_with_roles
  :  ?exclude:(Role.Role.t * Guard.Uuid.Target.t option) list
  -> Pool_database.Label.t
  -> (Role.Role.t * Guard.Uuid.Target.t option) list
  -> t list Lwt.t

val search_by_name_and_email
  :  ?dyn:Utils.Database.Dynparam.t
  -> ?exclude:Id.t list
  -> ?limit:int
  -> Pool_database.Label.t
  -> string
  -> t list Lwt.t

val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t

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
      -> (Guard.Actor.t, Pool_message.Error.t) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
  end
end

module Repo : sig
  module Entity : sig
    module Id : sig
      val t : Id.t Caqti_type.t
    end

    val t : t Caqti_type.t
  end

  val sql_select_columns : string list
  val joins : string
end
