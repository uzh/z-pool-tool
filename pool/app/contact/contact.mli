module Id : sig
  include Pool_model.Base.IdSig

  val of_common : Pool_common.Id.t -> t
  val to_common : t -> Pool_common.Id.t
  val of_user : Pool_user.Id.t -> t
  val to_user : t -> Pool_user.Id.t
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
  { user : Pool_user.t
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
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val profile_completion_cookie : string
val user : t -> Pool_user.t
val id : t -> Id.t
val firstname : t -> Pool_user.Firstname.t
val lastname : t -> Pool_user.Lastname.t
val fullname : t -> string
val lastname_firstname : t -> string
val email_address : t -> Pool_user.EmailAddress.t
val cell_phone : t -> Pool_user.CellPhone.t option
val is_inactive : t -> bool
val sexp_of_t : t -> Sexplib0.Sexp.t
val yojson_of_t : t -> Yojson.Safe.t
val show : t -> string
val compare : t -> t -> int
val set_email_address : t -> Pool_user.EmailAddress.t -> t
val set_firstname : t -> Pool_user.Firstname.t -> t
val set_lastname : t -> Pool_user.Lastname.t -> t
val set_language : t -> Pool_common.Language.t option -> t
val set_cellphone : t -> Pool_user.CellPhone.t option -> t
val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_admin_comment : Database.Label.t -> Id.t -> AdminComment.t option Lwt.t
val find_multiple : Database.Label.t -> Id.t list -> t list Lwt.t

val find_by_email
  :  Database.Label.t
  -> Pool_user.EmailAddress.t
  -> (t, Pool_message.Error.t) Lwt_result.t

val find_by_user
  :  Database.Label.t
  -> Pool_user.t
  -> (t, Pool_message.Error.t) Lwt_result.t

val find_all
  :  ?query:Query.t
  -> ?actor:Guard.Actor.t
  -> ?permission:Guard.Permission.t
  -> Database.Label.t
  -> unit
  -> (t list * Query.t) Lwt.t

val find_to_trigger_profile_update
  :  Database.Label.t
  -> (t list, 'a) Lwt_result.t

val should_send_registration_attempt_notification
  :  Database.Label.t
  -> t
  -> bool Lwt.t

val find_cell_phone_verification_by_contact
  :  Database.Label.t
  -> t
  -> Pool_user.UnverifiedCellPhone.t option Lwt.t

val find_cell_phone_verification_by_contact_and_code
  :  Database.Label.t
  -> t
  -> Pool_common.VerificationCode.t
  -> (Pool_user.UnverifiedCellPhone.t, Pool_message.Error.t) Lwt_result.t

val find_full_cell_phone_verification_by_contact
  :  Database.Label.t
  -> t
  -> (Pool_user.UnverifiedCellPhone.full, Pool_message.Error.t) Lwt_result.t

val has_terms_accepted : Database.Label.t -> t -> bool Lwt.t

type create =
  { user_id : Id.t
  ; email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.Plain.t
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
  | EmailVerified of t
  | TermsAccepted of t
  | MarkedAsDeleted of t
  | UnverifiedDeleted of t
  | CellPhoneAdded of t * Pool_user.CellPhone.t * Pool_common.VerificationCode.t
  | CellPhoneVerified of t * Pool_user.CellPhone.t
  | CellPhoneVerificationReset of t
  | ImportConfirmed of t * Pool_user.Password.Plain.t
  | ImportDisabled of t
  | ProfileUpdateTriggeredAtUpdated of t list
  | RegistrationAttemptNotificationSent of t
  | Updated of t
  | SignInCounterUpdated of t

val created : create -> event
val updated : t -> event
val handle_event : ?tags:Logs.Tag.set -> Database.Label.t -> event -> unit Lwt.t
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
    { user : Pool_user.t
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

val column_cell_phone : Query.Column.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t

module Repo : sig
  module Id : Pool_model.Base.CaqtiSig with type t = Id.t
  module Preview : Pool_model.Base.CaqtiSig with type t = Preview.t

  val t : t Caqti_type.t
  val joins : string
  val sql_select_columns : string list

  val make_sql_select_columns
    :  user_table:string
    -> contact_table:string
    -> string list

  val find_request_sql
    :  ?additional_joins:string list
    -> ?count:bool
    -> string
    -> string
end

module VersionHistory : Changelog.TSig with type record = t

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

    type t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Actor : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Actor.t, Pool_message.Error.t) Lwt_result.t

    type t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index_permission : Guard.Permission.t
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val read_of_target : Guard.Uuid.Target.t -> Guard.PermissionOnTarget.t list

    val read_name
      :  ?verify_on_ids:Guard.Uuid.Target.t list
      -> unit
      -> Guard.PermissionOnTarget.t list

    val read_info
      :  ?verify_on_ids:Guard.Uuid.Target.t list
      -> unit
      -> Guard.PermissionOnTarget.t list

    val send_direct_message : Guard.ValidationSet.t
  end
end
