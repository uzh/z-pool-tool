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
end

module NumberOfAssignments : sig
  type t

  val init : t
  val of_int : int -> t
end

module NumberOfShowUps : sig
  type t

  val init : t
  val of_int : int -> t
end

module NumberOfParticipations : sig
  type t

  val init : t
  val of_int : int -> t
end

type t =
  { user : Service.User.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  ; experiment_type_preference : Pool_common.ExperimentType.t option
  ; paused : Pool_user.Paused.t
  ; disabled : Pool_user.Disabled.t
  ; verified : Pool_user.Verified.t option
  ; email_verified : Pool_user.EmailVerified.t option
  ; num_invitations : NumberOfInvitations.t
  ; num_assignments : NumberOfAssignments.t
  ; num_show_ups : NumberOfShowUps.t
  ; num_participations : NumberOfParticipations.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; language_version : Pool_common.Version.t
  ; experiment_type_preference_version : Pool_common.Version.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val profile_completion_cookie : string
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

val has_terms_accepted : Pool_database.Label.t -> t -> bool Lwt.t

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
  { show_up : bool
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
  | NumAssignmentsDecreasedBy of (t * int)
  | NumAssignmentsIncreasedBy of (t * int)
  | NumInvitationsIncreased of t
  | ProfileUpdateTriggeredAtUpdated of t list
  | SessionParticipationSet of t * session_participation
  | RegistrationAttemptNotificationSent of t

val created : create -> event
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

module Preview : sig
  type t =
    { user : Sihl_user.t
    ; language : Pool_common.Language.t option
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

  module Model : sig
    val t : t Caqti_type.t
  end

  module Sql : sig
    val find_request_sql : string -> string
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

    val authorizable_of_req
      :  ?ctx:(string * string) list
      -> Rock.Request.t
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
  end
end
