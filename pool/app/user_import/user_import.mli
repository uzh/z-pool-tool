module Token : Pool_model.Base.StringSig

module ConfirmedAt : sig
  include Pool_model.Base.PtimeSig
end

module NotifiedAt : sig
  include Pool_model.Base.PtimeSig
end

module ReminderCount : sig
  include Pool_model.Base.IntegerSig

  val init : t
end

module LastRemindedAt : sig
  include Pool_model.Base.PtimeSig
end

type t =
  { user_uuid : Pool_common.Id.t
  ; token : Token.t
  ; confirmed_at : ConfirmedAt.t option
  ; notified_at : NotifiedAt.t option
  ; reminder_count : ReminderCount.t
  ; last_reminded_at : LastRemindedAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool

val find_pending_by_token
  :  Database.Label.t
  -> Token.t
  -> (t, Pool_message.Error.t) result Lwt.t

val find_pending_by_user_id_opt
  :  Database.Label.t
  -> Pool_common.Id.t
  -> t option Lwt.t

val find_pending_by_email_opt
  :  Database.Label.t
  -> Pool_user.EmailAddress.t
  -> t option Lwt.t

type event =
  | Confirmed of t
  | Notified of t
  | Reminded of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t
val insert : Database.Label.t -> t -> unit Lwt.t
val update : Database.Label.t -> t -> unit Lwt.t

val find_contacts_to_notify
  :  Database.Label.t
  -> int
  -> unit
  -> (Contact.t * t) list Lwt.t

val find_contacts_to_remind
  :  Settings.UserImportReminder.FirstReminderAfter.t
     * Settings.UserImportReminder.SecondReminderAfter.t
  -> Database.Label.t
  -> int
  -> unit
  -> (Contact.t * t) list Lwt.t

module Repo : sig
  val sql_select_columns : string list
  val joins : string
  val t : t Caqti_type.t
end

module Service : sig
  val run : Database.Label.t -> unit Lwt.t
  val register : unit -> Sihl.Container.Service.t
end
