module Id : sig
  include Pool_model.Base.IdSig

  val to_common : t -> Pool_common.Id.t
  val of_common : Pool_common.Id.t -> t
end

module Status : sig
  type t =
    | Active
    | Inactive

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val to_human : Pool_common.Language.t -> t -> string
  val create : string -> (t, Pool_message.Error.t) result
  val all : t list
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module PasswordConfirmed : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> t

  val schema
    :  ?field:Pool_message.Field.t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module Password : sig
  module Policy : sig
    type rule =
      | MinLength of int
      | MustContainCapitalLetter
      | MustContainNumber
      | MustContainSpecialChar of char list

    type t

    val default_special_char_set : char list
    val default : t
  end

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_message.Error.t) result
  val create_unvalidated : string -> (t, Pool_message.Error.t) result
  val to_confirmed : t -> PasswordConfirmed.t

  val schema
    :  ?field:Pool_message.Field.t
    -> (string -> (t, Pool_message.Error.t) result)
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t

  val validate_password_confirmation
    :  t
    -> PasswordConfirmed.t
    -> (unit, Pool_message.Error.t) result
end

module HashedPassword : sig
  include Pool_model.Base.StringSig

  val create : Password.t -> (t, Pool_message__Pool_message_error.t) result
end

module Firstname : sig
  include Pool_model.Base.StringSig

  val of_string : string -> t
end

module Lastname : sig
  include Pool_model.Base.StringSig

  val of_string : string -> t
end

module Paused : sig
  include Pool_model.Base.BooleanSig
end

module Disabled : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> bool
  val create : bool -> t
  val compare : t -> t -> int
end

module TermsAccepted : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
  val compare : t -> t -> int
end

module Verified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
  val compare : t -> t -> int
end

module CellPhone : sig
  type t

  val create : string -> (t, Pool_message.Error.t) result
  val of_string : string -> t
  val value : t -> string
  val equal : t -> t -> bool
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int

  val schema_test_cell_phone
    :  unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module ImportPending : sig
  include Pool_model.Base.BooleanSig
end

module UnverifiedCellPhone : sig
  type t =
    { cell_phone : CellPhone.t
    ; created_at : Pool_common.CreatedAt.t
    }

  type full =
    { cell_phone : CellPhone.t
    ; verification_code : Pool_common.VerificationCode.t
    ; created_at : Pool_common.CreatedAt.t
    }
end

module EmailAddress : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val validate_characters : t -> (t, Pool_message.Error.t) result

  val validate
    :  Settings.EmailSuffix.t list option
    -> t
    -> (unit, Pool_message.Error.t) result

  val value : t -> string
  val create : string -> (t, Pool_message.Error.t) result
  val of_string : string -> t
  val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end

module EmailVerified : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.t -> t
  val create_now : unit -> t
  val value : t -> Ptime.t
  val compare : t -> t -> int
end

module PasswordReset : sig
  val create_reset_token
    :  Database.Label.t
    -> EmailAddress.t
    -> string option Lwt.t

  val reset_password
    :  token:string
    -> Database.Label.t
    -> Password.t
    -> PasswordConfirmed.t
    -> (unit, Pool_message.Error.t) Result.t Lwt.t

  val register : unit -> Sihl.Container.Service.t
  val lifecycle : Sihl.Container.lifecycle
end

type t =
  { id : Id.t
  ; email : EmailAddress.t
  ; name : Lastname.t
  ; given_name : Firstname.t
  ; password : HashedPassword.t
  ; status : Status.t
  ; admin : bool
  ; confirmed : bool
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val compare : t -> t -> int
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val sexp_of_t : t -> Sexplib0.Sexp.t
val is_admin : t -> bool
val user_firstname : t -> Firstname.t
val user_lastname : t -> Lastname.t
val user_fullname : t -> string
val user_lastname_firstname : t -> string
val user_email_address : t -> EmailAddress.t

val validate_current_password
  :  ?field:Pool_message.Field.t
  -> t
  -> Password.t
  -> (unit, Pool_message.Error.t) result

module Repo : sig
  module Id : Pool_model.Base.CaqtiSig with type t = Id.t
  module Paused : Pool_model.Base.CaqtiSig with type t = Paused.t
  module Disabled : Pool_model.Base.CaqtiSig with type t = Disabled.t
  module TermsAccepted : Pool_model.Base.CaqtiSig with type t = TermsAccepted.t
  module Verified : Pool_model.Base.CaqtiSig with type t = Verified.t
  module EmailVerified : Pool_model.Base.CaqtiSig with type t = EmailVerified.t
  module CellPhone : Pool_model.Base.CaqtiSig with type t = CellPhone.t
  module ImportPending : Pool_model.Base.CaqtiSig with type t = ImportPending.t

  module UnverifiedCellPhone : sig
    include Pool_model.Base.CaqtiSig with type t = UnverifiedCellPhone.t

    val full : UnverifiedCellPhone.full Caqti_type.t
  end

  module EmailAddress : Pool_model.Base.CaqtiSig with type t = EmailAddress.t

  val t : t Caqti_type.t
  val sql_select_columns : string list
end

val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t
val column_first_name : Query.Column.t
val column_last_name : Query.Column.t
val column_name : Query.Column.t
val column_email : Query.Column.t
val column_inactive : Query.Column.t

val find_active_user_by_email_opt
  :  Database.Label.t
  -> EmailAddress.t
  -> t option Lwt.t

val create_session
  :  Database.Label.t
  -> EmailAddress.t
  -> Password.t
  -> (t, [ `Does_not_exist | `Incorrect_password ]) Lwt_result.t

module FailedLoginAttempt : sig
  module Id : sig
    include Pool_model.Base.IdSig
  end

  module Counter : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : int -> t
    val init : t
    val value : t -> int
    val increment : t -> t
  end

  module BlockedUntil : sig
    type t

    val equal : t -> t -> bool
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val value : t -> Ptime.t
    val create : Ptime.t -> (t, Pool_message.Error.t) result
  end

  type t =
    { id : Id.t
    ; email : EmailAddress.t
    ; counter : Counter.t
    ; blocked_until : BlockedUntil.t option
    }

  val create
    :  ?id:Id.t
    -> EmailAddress.t
    -> Counter.t
    -> BlockedUntil.t option
    -> t

  module Repo : sig
    val find_opt : Database.Label.t -> EmailAddress.t -> t option Lwt.t
    val insert : Database.Label.t -> t -> unit Lwt.t
    val delete : Database.Label.t -> t -> unit Lwt.t
  end
end

module Web : sig
  (** [user_from_token ?key database_label read_token request] returns the user that is
      associated to the user id in the [Bearer] token of the [request].

      [key] is the key in the token associated with the user id. By default,
      the value is [user_id].

      [read_token] is a function that returns the associated value of [key] in
      a given token. *)
  val user_from_token
    :  ?key:string
    -> Database.Label.t
    -> (Database.Label.t -> string -> k:string -> Id.t option Lwt.t)
    -> Rock.Request.t
    -> t option Lwt.t

  (** [user_from_session ?cookie_key ?secret ?key ?secret database_label request]
      returns the user that is associated to the user id in the session of the
      [request].

      [cookie_key] is the name/key of the session cookie. By default, the
      value is [_session].

      [secret] is used to verify the signature of the session cookie. By
      default, [SIHL_SECRET] is used.

      [key] is the key in the session associated with the user id. By default,
      the value is [user_id]. *)
  val user_from_session
    :  ?cookie_key:string
    -> ?secret:string
    -> ?key:string
    -> Database.Label.t
    -> Rock.Request.t
    -> t option Lwt.t
end

(** [find_opt database_label id] returns a user with [id]. *)
val find_opt : Database.Label.t -> Id.t -> t option Lwt.t

(** [find database_label id] returns a user with [id], [None] otherwise. *)
val find : Database.Label.t -> Id.t -> t Lwt.t

(** [find_by_email database_label email] returns a [User.t] if there is a user with
    email address [email]. The lookup is case-insensitive. Raises an
    [{!Exception}] otherwise. *)
val find_by_email : Database.Label.t -> EmailAddress.t -> t Lwt.t

(** [find_by_email_opt database_label email] returns a [User.t] if there is a user with
    email address [email]. *)
val find_by_email_opt : Database.Label.t -> EmailAddress.t -> t option Lwt.t

(** [update_password database_label user ~old_password ~new_password
    ~new_password_confirmation]
    updates the password of a [user] to [new_password] and returns the user.
    The [old_password] is the current password that the user has to enter.
    [new_password] has to equal [new_password_confirmation]. *)
val update_password
  :  Database.Label.t
  -> t
  -> old_password:Password.t
  -> new_password:Password.t
  -> new_password_confirmation:PasswordConfirmed.t
  -> (t, Pool_message.Error.t) Lwt_result.t

(** [update ?email ?name ?given_name ?status user] stores the
    updated [user] and returns it. *)
val update
  :  ?email:EmailAddress.t
  -> ?name:Lastname.t
  -> ?given_name:Firstname.t
  -> ?status:Status.t
  -> Database.Label.t
  -> t
  -> t Lwt.t

(** [set_password user ~password ~password_confirmation]
    overrides the current password of a [user] and returns that user.
    [password] has to equal [password_confirmation].

    The current password doesn't have to be provided, therefore you should not
    expose this function to users but only admins. If you want the user to
    update their own password use {!update_password} instead. *)
val set_password
  :  Database.Label.t
  -> t
  -> Password.t
  -> PasswordConfirmed.t
  -> (t, Pool_message.Error.t) Lwt_result.t

(** [create_user ?id label email name given_name password] returns
    a non-admin user. Note that using [create_user] skips the registration
    workflow and should only be used with care.*)
val create_user
  :  ?id:Id.t
  -> Database.Label.t
  -> EmailAddress.t
  -> Lastname.t
  -> Firstname.t
  -> Password.t
  -> t Lwt.t

(** [create_admin ?id label email name given_name password] returns
    an admin user. *)
val create_admin
  :  ?id:Id.t
  -> Database.Label.t
  -> EmailAddress.t
  -> Lastname.t
  -> Firstname.t
  -> Password.t
  -> t Lwt.t

(** [register_user ?id label email name given_name password
    password_confirmation]
    creates a new user if the password is valid and if the email address was
    not already registered. *)
val register_user
  :  ?id:Id.t
  -> Database.Label.t
  -> EmailAddress.t
  -> Lastname.t
  -> Firstname.t
  -> Password.t
  -> PasswordConfirmed.t
  -> ( t
       , [ `Already_registered
         | `Invalid_password_provided of Pool_message.Error.t
         ] )
       Lwt_result.t

(** [login label email password] returns the user associated with [email] if
    [password] matches the current password. *)
val login
  :  Database.Label.t
  -> EmailAddress.t
  -> Password.t
  -> (t, [ `Does_not_exist | `Incorrect_password ]) Lwt_result.t

val lifecycle : Sihl.Container.lifecycle
val register : ?commands:Sihl.Command.t list -> unit -> Sihl.Container.Service.t
