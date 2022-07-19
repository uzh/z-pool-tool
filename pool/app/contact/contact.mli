module RecruitmentChannel : sig
  type t =
    | Friend
    | Online
    | Lecture
    | Mailing

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t

  val show : t -> string
  val equal : t -> t -> bool
  val read : string -> t
  val all : t list
end

module NumberOfInvitations : sig
  type t

  val init : t
end

module NumberOfAssignments : sig
  type t

  val init : t
end

type t =
  { user : Sihl_user.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t
  ; language : Pool_common.Language.t option
  ; experiment_type_preference : Pool_common.ExperimentType.t option
  ; paused : Pool_user.Paused.t
  ; disabled : Pool_user.Disabled.t
  ; verified : Pool_user.Verified.t
  ; email_verified : Pool_user.EmailVerified.t
  ; num_invitations : NumberOfInvitations.t
  ; num_assignments : NumberOfAssignments.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; language_version : Pool_common.Version.t
  ; experiment_type_preference_version : Pool_common.Version.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

module Field : sig
  type htmx_field =
    | Firstname of Pool_user.Firstname.t
    | Lastname of Pool_user.Lastname.t
    | Paused of Pool_user.Paused.t
    | Language of Pool_common.Language.t option
    | Custom of string * string

  type t = htmx_field * Pool_common.Version.t

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
end

val id : t -> Pool_common.Id.t
val firstname : t -> Pool_user.Firstname.t
val lastname : t -> Pool_user.Lastname.t
val fullname : t -> string
val email_address : t -> Pool_user.EmailAddress.t
val version_selector : t -> string -> Pool_common.Version.t option
val show : t -> string

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_multiple
  :  Pool_database.Label.t
  -> Pool_common.Id.t list
  -> t list Lwt.t

val find_filtered
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> string
  -> t list Lwt.t

val find_by_email
  :  Pool_database.Label.t
  -> Pool_user.EmailAddress.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_by_user
  :  Pool_database.Label.t
  -> Sihl_user.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all : Pool_database.Label.t -> unit -> t list Lwt.t

val find_to_trigger_profile_update
  :  Pool_database.Label.t
  -> (t list, 'a) Lwt_result.t

val has_terms_accepted : Pool_database.Label.t -> t -> bool Lwt.t

type create =
  { user_id : Pool_common.Id.t
  ; email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.t
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t
  ; language : Pool_common.Language.t option
  }

type event =
  | Created of create
  | Updated of Field.t * t
  | FirstnameUpdated of t * Pool_user.Firstname.t
  | LastnameUpdated of t * Pool_user.Lastname.t
  | PausedUpdated of t * Pool_user.Paused.t
  | EmailUpdated of t * Pool_user.EmailAddress.t
  | PasswordUpdated of
      t
      * Pool_user.Password.t
      * Pool_user.Password.t
      * Pool_user.PasswordConfirmed.t
  | LanguageUpdated of t * Pool_common.Language.t
  | Verified of t
  | EmailVerified of t
  | TermsAccepted of t
  | Disabled of t
  | UnverifiedDeleted of t
  | AssignmentIncreased of t
  | ShowUpIncreased of t
  | ProfileUpdateTriggeredAtUpdated of t list

val created : create -> event

(* TODO[timhub]: remove *)
val firstnameupdated : t -> Pool_user.Firstname.t -> event
val lastnameupdated : t -> Pool_user.Lastname.t -> event
val pausedupdated : t -> Pool_user.Paused.t -> event
val languageupdated : t -> Pool_common.Language.t -> event
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

module Preview : sig
  type t =
    { user : Sihl_user.t
    ; language : Pool_common.Language.t option
    ; paused : Pool_user.Paused.t
    ; verified : Pool_user.Verified.t
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    }

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val fullname : t -> string
  val email_address : t -> Pool_user.EmailAddress.t
end

module Repo : sig
  module Preview : sig
    val t : Preview.t Caqti_type.t
  end
end
