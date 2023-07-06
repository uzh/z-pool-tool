module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module Title : sig
  include Pool_common.Model.StringSig
end

module PublicTitle : sig
  include Pool_common.Model.StringSig
end

module Description : sig
  include Pool_common.Model.StringSig
end

module CostCenter : sig
  include Pool_common.Model.StringSig
end

module DirectRegistrationDisabled : sig
  include Pool_common.Model.BooleanSig
end

module RegistrationDisabled : sig
  include Pool_common.Model.BooleanSig
end

module AllowUninvitedSignup : sig
  type t

  val equal : t -> t -> t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : bool -> t
  val value : t -> bool

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t =
  { id : Id.t
  ; title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t option
  ; cost_center : CostCenter.t option
  ; organisational_unit : Organisational_unit.t option
  ; filter : Filter.t option
  ; contact_person_id : Admin.Id.t option
  ; smtp_auth_id : Email.SmtpAuth.Id.t option
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Id.t
  -> Title.t
  -> PublicTitle.t
  -> Description.t option
  -> CostCenter.t option
  -> Organisational_unit.t option
  -> Admin.Id.t option
  -> Email.SmtpAuth.Id.t option
  -> DirectRegistrationDisabled.t
  -> RegistrationDisabled.t
  -> AllowUninvitedSignup.t
  -> Pool_common.ExperimentType.t option
  -> Pool_common.Reminder.LeadTime.t option
  -> (t, Pool_common.Message.error) result

type create =
  { title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t option
  ; cost_center : CostCenter.t option
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

module Public : sig
  type t =
    { id : Id.t
    ; public_title : PublicTitle.t
    ; description : Description.t option
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    ; experiment_type : Pool_common.ExperimentType.t option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

type event =
  | Created of t
  | Updated of t
  | Deleted of Id.t

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val created : t -> event
val updated : t -> event
val deleted : Pool_common.Id.t -> event
val boolean_fields : Pool_common.Message.Field.t list

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all
  :  ?query:Query.t
  -> ?actor:'a Guard.Actor.t
  -> ?action:Guard.Action.t
  -> Pool_database.Label.t
  -> (t list * Query.t) Lwt.t

val find_public
  :  Pool_database.Label.t
  -> Id.t
  -> Contact.t
  -> (Public.t, Pool_common.Message.error) result Lwt.t

val find_of_session
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_of_mailing
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all_public_by_contact
  :  Pool_database.Label.t
  -> Contact.t
  -> Public.t list Lwt.t

val find_pending_waitinglists_by_contact
  :  Pool_database.Label.t
  -> Contact.t
  -> Public.t list Lwt.t

val find_past_experiments_by_contact
  :  Pool_database.Label.t
  -> Contact.t
  -> Public.t list Lwt.t

val session_count : Pool_database.Label.t -> Id.t -> int Lwt.t

val search
  :  Pool_database.Label.t
  -> Id.t list
  -> string
  -> (Id.t * Title.t) list Lwt.t

val search_multiple_by_id
  :  Pool_database.Label.t
  -> Pool_common.Id.t list
  -> (Id.t * Title.t) list Lwt.t

val possible_participant_count : t -> int Lwt.t
val possible_participants : t -> Contact.t list Lwt.t
val title_value : t -> string
val public_title_value : t -> string
val session_reminder_lead_time_value : t -> Ptime.span option
val direct_registration_disabled_value : t -> bool
val registration_disabled_value : t -> bool
val allow_uninvited_signup_value : t -> bool

val smtp_auth
  :  Pool_database.Label.t
  -> t
  -> (Email.SmtpAuth.t option, Pool_common.Message.error) Lwt_result.t

module Repo : sig
  module Public : sig
    val select_from_experiments_sql : ?distinct:bool -> string -> string

    module Entity : sig
      val t : Public.t Caqti_type.t
    end
  end

  module Id : sig
    type t = Id.t

    val t : t Caqti_type.t
  end
end

module Guard : sig
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

  module Access : sig
    val index_action : Guard.Action.t
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val read : Id.t -> Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
    val recruiter_of : Id.t -> Guard.ValidationSet.t
  end
end

val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
