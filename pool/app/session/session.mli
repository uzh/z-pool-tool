(* TODO [aerben] maybe can extract even more? *)
module type Base = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Description : sig
  include Base

  val value : t -> string
  val create : string -> (t, Pool_common.Message.error) result
end

module ParticipantAmount : sig
  include Base

  val value : t -> int
  val create : int -> (t, Pool_common.Message.error) result

  val schema
    :  Pool_common.Message.Field.t
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Start : sig
  include Base

  val value : t -> Ptime.t
  val create : Ptime.t -> t
end

module Duration : sig
  include Base

  val value : t -> Ptime.Span.t
  val create : Ptime.Span.t -> (t, Pool_common.Message.error) result
end

type base =
  { start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_text : Pool_common.Reminder.Text.t option
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; reminder_language : Pool_common.Language.t option
  }

module AssignmentCount : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> int
  val create : int -> (t, Pool_common.Message.error) result
end

type t =
  { id : Pool_common.Id.t
  ; start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; location : Pool_location.t
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; reminder_text : Pool_common.Reminder.Text.t option
  ; reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; reminder_language : Pool_common.Language.t option
  ; reminder_sent_at : Pool_common.Reminder.SentAt.t
  ; assignment_count : AssignmentCount.t
  ; (* TODO [aerben] want multiple follow up session?
     * 1. Ja es gibt immer wieder Sessions mit mehreren Following Sessions
     * 2. Eigentlich ist es immer eine Hauptsession mit mehreren Following Sessions

     * Could this model as the following, just flatten tail of linked list
     *  : ; follow_up : t *)

    (* TODO [aerben] make type for canceled_at? *)
    canceled_at : Ptime.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val is_fully_booked : t -> bool
val session_date_to_human : t -> string

(* TODO [aerben] this should be experiment id type *)
(* TODO [aerben] maybe Experiment.t Pool_common.Id.t *)
type event =
  | Created of (base * Pool_common.Id.t * Pool_location.t)
  | Canceled of t
  | Deleted of t
  | Updated of (base * Pool_location.t * t)

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

module Public : sig
  type t =
    { id : Pool_common.Id.t
    ; start : Start.t
    ; duration : Duration.t
    ; description : Description.t option
    ; location : Pool_location.t
    ; max_participants : ParticipantAmount.t
    ; min_participants : ParticipantAmount.t
    ; overbook : ParticipantAmount.t
    ; assignment_count : AssignmentCount.t
    ; canceled_at : Ptime.t option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val is_fully_booked : t -> bool
end

(* TODO [aerben] this should be experiment id type *)
val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_public
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> Contact.t
  -> (Public.t, Pool_common.Message.error) Lwt_result.t

val find_all_public_by_location
  :  Pool_database.Label.t
  -> Pool_location.Id.t
  -> (Public.t list, Pool_common.Message.error) result Lwt.t

val find_all_for_experiment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t list, Pool_common.Message.error) result Lwt.t

val find_all_public_for_experiment
  :  Pool_database.Label.t
  -> Contact.t
  -> Pool_common.Id.t
  -> (Public.t list, Pool_common.Message.error) result Lwt.t

val find_public_by_assignment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (Public.t, Pool_common.Message.error) result Lwt.t

val find_experiment_id_and_title
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (Pool_common.Id.t * string, Pool_common.Message.error) result Lwt.t

val to_email_text
  :  Pool_common.Language.t
  -> Start.t
  -> Duration.t
  -> Pool_location.t
  -> string
