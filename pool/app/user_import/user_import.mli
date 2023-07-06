module Token : Pool_common.Model.StringSig

module ConfirmedAt : sig
  include Pool_common.Model.PtimeSig
end

module NotifiedAt : sig
  include Pool_common.Model.PtimeSig
end

module ReminderCount : sig
  include Pool_common.Model.IntegerSig

  val init : t
end

module LastRemindedAt : sig
  include Pool_common.Model.PtimeSig
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

val find_pending_by_token
  :  Pool_database.Label.t
  -> Token.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_pending_by_user_opt
  :  Pool_database.Label.t
  -> Sihl_user.t
  -> t option Lwt.t

type event =
  | Confirmed of t
  | Notified of t
  | Reminded of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_tenant.Database.Label.t -> event -> unit Lwt.t

module Repo : sig
  val select_user_import_columns : string
  val t : t Caqti_type.t
end

module Service : sig
  val run : Pool_database.Label.t -> unit Lwt.t
  val register : unit -> Sihl.Container.Service.t
end
