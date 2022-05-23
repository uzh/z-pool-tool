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
  val create : Ptime.t -> (t, Pool_common.Message.error) result
end

module Duration : sig
  include Base

  val value : t -> Ptime.Span.t
  val create : Ptime.Span.t -> (t, Pool_common.Message.error) result
end

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
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  ; assignments_count : AssignmentCount.t
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

type base =
  { start : Start.t
  ; duration : Duration.t
  ; description : Description.t option
  ; max_participants : ParticipantAmount.t
  ; min_participants : ParticipantAmount.t
  ; overbook : ParticipantAmount.t
  }

(* TODO [aerben] this should be experiment id type *)
(* TODO [aerben] maybe Experiment.t Pool_common.Id.t *)
type event =
  | Created of (base * Pool_common.Id.t)
  | Canceled of t
  | Deleted of t
  | Updated of (base * t)

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

module Public : sig
  type t =
    { id : Pool_common.Id.t
    ; start : Start.t
    ; duration : Duration.t
    ; description : Description.t option
    ; canceled_at : Ptime.t option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
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

val find_all_for_experiment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> t list Lwt.t

val find_all_public_for_experiment
  :  Pool_database.Label.t
  -> Contact.t
  -> Pool_common.Id.t
  -> Public.t list Lwt.t

val find_public_by_assignment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (Public.t, Pool_common.Message.error) result Lwt.t

module Repo : sig
  module Public : sig
    val t : Public.t Caqti_type.t
  end
end
