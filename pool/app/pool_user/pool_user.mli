module Id : sig
  include Pool_model.Base.IdSig

  val to_common : t -> Pool_common.Id.t
  val of_common : Pool_common.Id.t -> t
end

module EmailAddress : sig
  include Pool_model.Base.StringSig

  val validate_characters : t -> (t, Pool_message.Error.t) result

  val validate
    :  Settings.EmailSuffix.t list option
    -> t
    -> (unit, Pool_message.Error.t) result

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

  module Confirmation : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> 'a -> unit
    val show : t -> string
    val create : string -> t

    val schema
      :  ?field:Pool_message.Field.t
      -> unit
      -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  end

  module Plain : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> 'a -> unit
    val show : t -> string
    val create : string -> t

    val schema
      :  ?field:Pool_message.Field.t
      -> ?validation:(t -> (t, Pool_message.Error.t) result)
      -> unit
      -> (Pool_message.Error.t, t) Pool_conformist.Field.t

    val validate : t -> (t, Pool_message.Error.t) result
  end

  val to_confirmed : Plain.t -> Confirmation.t

  val validate_confirmation
    :  Plain.t
    -> Confirmation.t
    -> (unit, Pool_message.Error.t) result

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> 'a -> unit
  val show : t -> string

  val schema
    :  ?field:Pool_message.Field.t
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t

  val validate_current : Database.Label.t -> Id.t -> Plain.t -> bool Lwt.t

  (** [define database_label user_id password password_confirmation]
      overrides the current password of a [user_id] if [password] and
      [password_confirmation] are equal.

      The current password doesn't have to be provided, therefore it should not
      expose this function to users but only admins. If you want the user to
      update their own password use {!update} instead. *)
  val define
    :  Database.Label.t
    -> Id.t
    -> Plain.t
    -> Confirmation.t
    -> (unit, Pool_message.Error.t) Lwt_result.t

  (** [update database_label user_id ~old_password ~new_password
    ~new_password_confirmation]
      updates the password of a [user_id] to [new_password] and returns the user.
      The [old_password] is the current password that the user has to enter.
      [new_password] has to equal [new_password_confirmation]. *)
  val update
    :  Database.Label.t
    -> Id.t
    -> old_password:Plain.t
    -> new_password:Plain.t
    -> new_password_confirmation:Confirmation.t
    -> (unit, Pool_message.Error.t) Lwt_result.t

  module Reset : sig
    val create_token : Database.Label.t -> EmailAddress.t -> string option Lwt.t

    val reset_password
      :  token:string
      -> Database.Label.t
      -> Plain.t
      -> Confirmation.t
      -> (unit, Pool_message.Error.t) Lwt_result.t

    val register : unit -> Sihl.Container.Service.t
    val lifecycle : Sihl.Container.lifecycle
  end
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

module Firstname : sig
  include Pool_model.Base.StringSig
end

module Lastname : sig
  include Pool_model.Base.StringSig
end

module Paused : sig
  include Pool_model.Base.BooleanSig
end

module Disabled : sig
  include Pool_model.Base.BooleanSig
end

module TermsAccepted : sig
  include Pool_model.Base.PtimeSig
end

module Verified : sig
  include Pool_model.Base.PtimeSig
end

module CellPhone : sig
  include Pool_model.Base.StringSig
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

module EmailVerified : sig
  include Pool_model.Base.PtimeSig
end

module IsAdmin : sig
  include Pool_model.Base.BooleanSig
end

module Confirmed : sig
  include Pool_model.Base.BooleanSig
end

type t =
  { id : Id.t
  ; email : EmailAddress.t
  ; lastname : Lastname.t
  ; firstname : Firstname.t
  ; status : Status.t
  ; admin : IsAdmin.t
  ; confirmed : Confirmed.t
  }

val compare : t -> t -> int
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val sexp_of_t : t -> Sexplib0.Sexp.t
val id : t -> Id.t
val email : t -> EmailAddress.t
val firstname : t -> Firstname.t
val lastname : t -> Lastname.t
val fullname : ?reversed:bool -> t -> string
val status : t -> Status.t
val is_admin : t -> bool
val is_confirmed : t -> bool

val find_active_by_email_opt
  :  Database.Label.t
  -> EmailAddress.t
  -> t option Lwt.t

module Repo : sig
  module Id : Pool_model.Base.CaqtiSig with type t = Id.t
  module CellPhone : Pool_model.Base.CaqtiSig with type t = CellPhone.t
  module Disabled : Pool_model.Base.CaqtiSig with type t = Disabled.t
  module EmailAddress : Pool_model.Base.CaqtiSig with type t = EmailAddress.t
  module EmailVerified : Pool_model.Base.CaqtiSig with type t = EmailVerified.t
  module ImportPending : Pool_model.Base.CaqtiSig with type t = ImportPending.t
  module Paused : Pool_model.Base.CaqtiSig with type t = Paused.t
  module TermsAccepted : Pool_model.Base.CaqtiSig with type t = TermsAccepted.t
  module Verified : Pool_model.Base.CaqtiSig with type t = Verified.t

  module UnverifiedCellPhone : sig
    include Pool_model.Base.CaqtiSig with type t = UnverifiedCellPhone.t

    val full : UnverifiedCellPhone.full Caqti_type.t
  end

  val t : t Caqti_type.t
  val sql_select_columns : string list
  val make_sql_select_columns : tablename:string -> string list
  val update_request : (t, unit, [ `Zero ]) Caqti_request.t
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

type event =
  | PasswordUpdated of
      Id.t * Password.Plain.t * Password.Plain.t * Password.Confirmation.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : ?tags:Logs.Tag.set -> Database.Label.t -> event -> unit Lwt.t

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

(** [find database_label id] returns a user with [id], [Error NotFound] otherwise. *)
val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t

(** [find_exn database_label id] returns a user with [id], throws exception otherwise. *)
val find_exn : Database.Label.t -> Id.t -> t Lwt.t

(** [find_opt database_label id] returns a user with [id], [None] otherwise. *)
val find_opt : Database.Label.t -> Id.t -> t option Lwt.t

(** [find_by_email database_label email] returns a [User.t] if there is a user with
    email address [email]. The lookup is case-insensitive. [Error NotFound] otherwise. *)
val find_by_email
  :  Database.Label.t
  -> EmailAddress.t
  -> (t, Pool_message.Error.t) Lwt_result.t

(** [find_by_email database_label email] returns a [User.t] if there is a user with
    email address [email]. The lookup is case-insensitive. Raises an
    [{!Exception}] otherwise. *)
val find_by_email_exn : Database.Label.t -> EmailAddress.t -> t Lwt.t

(** [find_by_email_opt database_label email] returns a [User.t] if there is a user with
    email address [email]. *)
val find_by_email_opt : Database.Label.t -> EmailAddress.t -> t option Lwt.t

(** [update ?email ?lastname ?firstname ?status database_label user] stores the
    updated [user] and returns it. *)
val update
  :  ?email:EmailAddress.t
  -> ?lastname:Lastname.t
  -> ?firstname:Firstname.t
  -> ?status:Status.t
  -> ?confirmed:Confirmed.t
  -> Database.Label.t
  -> t
  -> t Lwt.t

(** [confirm database_label user] stores the [user] as confirmed and returns it. *)
val confirm : Database.Label.t -> t -> t Lwt.t

(** [create_user ?id label email lastname firstname password password_confirmed] returns
    a non-admin user. *)
val create_user
  :  ?id:Id.t
  -> Database.Label.t
  -> EmailAddress.t
  -> Lastname.t
  -> Firstname.t
  -> Password.Plain.t
  -> Password.Confirmation.t
  -> (t, Pool_message.Error.t) Lwt_result.t

(** [create_admin ?id label email lastname firstname password password_confirmed] returns
    an admin user. *)
val create_admin
  :  ?id:Id.t
  -> Database.Label.t
  -> EmailAddress.t
  -> Lastname.t
  -> Firstname.t
  -> Password.Plain.t
  -> Password.Confirmation.t
  -> (t, Pool_message.Error.t) Lwt_result.t

(** [login label email password] returns the user associated with [email] if
    [password] matches the current password. *)
val login
  :  Database.Label.t
  -> EmailAddress.t
  -> Password.Plain.t
  -> (t, Pool_message.Error.t) Lwt_result.t

val lifecycle : Sihl.Container.lifecycle
val register : ?commands:Sihl.Command.t list -> unit -> Sihl.Container.Service.t
