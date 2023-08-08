module Id = Pool_common.Id

module MessageChannel : sig
  type t =
    | Email
    | SMS

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module NumberOfInvitations : sig
  type t

  val init : t
  val of_int : int -> t
  val equal : t -> t -> bool
  val update : int -> t -> t
end

module NumberOfAssignments : sig
  type t

  val init : t
  val of_int : int -> t
  val equal : t -> t -> bool
  val update : int -> t -> t
end

module NumberOfShowUps : sig
  type t

  val init : t
  val of_int : int -> t
  val equal : t -> t -> bool
  val update : int -> t -> t
end

module NumberOfNoShows : sig
  type t

  val init : t
  val of_int : int -> t
  val equal : t -> t -> bool
  val update : int -> t -> t
end

module NumberOfParticipations : sig
  type t

  val init : t
  val of_int : int -> t
  val equal : t -> t -> bool
  val update : int -> t -> t
end

module AdminComment : sig
  type t

  val of_string : string -> t
  val equal : t -> t -> bool
  val value : t -> string
end

type t =
  { user : Service.User.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  ; experiment_type_preference : Pool_common.ExperimentType.t option
  ; cell_phone : Pool_user.CellPhone.t option
  ; paused : Pool_user.Paused.t
  ; disabled : Pool_user.Disabled.t
  ; verified : Pool_user.Verified.t option
  ; email_verified : Pool_user.EmailVerified.t option
  ; num_invitations : NumberOfInvitations.t
  ; num_assignments : NumberOfAssignments.t
  ; num_show_ups : NumberOfShowUps.t
  ; num_no_shows : NumberOfNoShows.t
  ; num_participations : NumberOfParticipations.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; language_version : Pool_common.Version.t
  ; experiment_type_preference_version : Pool_common.Version.t
  ; import_pending : Pool_user.ImportPending.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val profile_completion_cookie : string
val user : t -> Sihl_user.t
val id : t -> Pool_common.Id.t
val firstname : t -> Pool_user.Firstname.t
val lastname : t -> Pool_user.Lastname.t
val fullname : t -> string
val email_address : t -> Pool_user.EmailAddress.t
val sexp_of_t : t -> Sexplib0.Sexp.t
val show : t -> string

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_admin_comment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> AdminComment.t option Lwt.t

val find_multiple
  :  Pool_database.Label.t
  -> Pool_common.Id.t list
  -> t list Lwt.t

val find_by_email
  :  Pool_database.Label.t
  -> Pool_user.EmailAddress.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_by_user
  :  Pool_database.Label.t
  -> Sihl_user.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all
  :  Pool_database.Label.t
  -> ?query:Query.t
  -> unit
  -> (t list * Query.t) Lwt.t

val find_to_trigger_profile_update
  :  Pool_database.Label.t
  -> (t list, 'a) Lwt_result.t

val should_send_registration_attempt_notification
  :  Pool_database.Label.t
  -> t
  -> bool Lwt.t

val find_cell_phone_verification_by_contact
  :  Pool_database.Label.t
  -> t
  -> Pool_user.UnverifiedCellPhone.t option Lwt.t

val find_cell_phone_verification_by_contact_and_code
  :  Pool_database.Label.t
  -> t
  -> Pool_common.VerificationCode.t
  -> (Pool_user.UnverifiedCellPhone.t, Pool_common.Message.error) result Lwt.t

val find_full_cell_phone_verification_by_contact
  :  Pool_database.Label.t
  -> t
  -> (Pool_user.UnverifiedCellPhone.full, Pool_common.Message.error) result
     Lwt.t

val has_terms_accepted : Pool_database.Label.t -> t -> bool Lwt.t

val message_language
  :  Pool_database.Label.t
  -> t
  -> Pool_common.Language.t Lwt.t

type create =
  { user_id : Pool_common.Id.t
  ; email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  }

type session_participation =
  { no_show : bool
  ; participated : bool
  }

type event =
  | Created of create
  | EmailUpdated of t * Pool_user.EmailAddress.t
  | PasswordUpdated of
      t
      * Pool_user.Password.t
      * Pool_user.Password.t
      * Pool_user.PasswordConfirmed.t
  | Verified of t
  | EmailVerified of t
  | TermsAccepted of t
  | Disabled of t
  | UnverifiedDeleted of t
  | CellPhoneAdded of t * Pool_user.CellPhone.t * Pool_common.VerificationCode.t
  | CellPhoneVerified of t * Pool_user.CellPhone.t
  | CellPhoneVerificationReset of t
  | ImportConfirmed of t * Pool_user.Password.t
  | ImportDisabled of t
  | ProfileUpdateTriggeredAtUpdated of t list
  | RegistrationAttemptNotificationSent of t
  | Updated of t
  | SignInCounterUpdated of t

val created : create -> event
val updated : t -> event

val handle_event
  :  ?tags:Logs.Tag.set
  -> Pool_database.Label.t
  -> event
  -> unit Lwt.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val update_num_invitations : step:int -> t -> t
val update_num_assignments : step:int -> t -> t
val update_num_show_ups : step:int -> t -> t
val update_num_no_shows : step:int -> t -> t
val update_num_participations : step:int -> t -> t

module Preview : sig
  type t =
    { user : Sihl_user.t
    ; language : Pool_common.Language.t option
    ; cell_phone : Pool_user.CellPhone.t option
    ; paused : Pool_user.Paused.t
    ; verified : Pool_user.Verified.t option
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    }

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val fullname : t -> string
  val email_address : t -> Pool_user.EmailAddress.t
end

val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list

module Repo : sig
  module Preview : sig
    val t : Preview.t Caqti_type.t
  end

  module Entity : sig
    val t : t Caqti_type.t
  end

  module Sql : sig
    val find_request_sql : string -> string

    val select_imported_contacts_sql
      :  import_columns:string
      -> where:string
      -> limit:int
      -> string
  end
end

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Role.Target.t Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Actor : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Role.Actor.t Guard.Actor.t, Pool_common.Message.error) Lwt_result.t

    type t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val read_name : Guard.ValidationSet.t
    val read_email : Guard.ValidationSet.t
    val read_cellphone : Guard.ValidationSet.t
  end
end
