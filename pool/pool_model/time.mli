(** [Pool_core.Time] plus everything that needs [Pool_message] or
    [Pool_conformist]: error-typed parsers and conformist schemas. *)

include module type of struct
    include Pool_core.Time
  end
  with module Span := Pool_core.Time.Span
   and module Date := Pool_core.Time.Date

(* Parsing *)
val parse_time : string -> (t, Pool_message.Error.t) result
val parse_time_span : string -> (Ptime.Span.t, Pool_message.Error.t) result
val parse_date_from_calendar : string -> (t, Pool_message.Error.t) result
val start_is_before_end : start:t -> end_at:t -> (unit, Pool_message.Error.t) result

module Date : sig
  include module type of struct
    include Pool_core.Time.Date
  end

  val of_string : string -> (t, Pool_message.Error.t) result
end

val schema
  :  Pool_message.Field.t
  -> (t -> (t, Pool_message.Error.t) result)
  -> unit
  -> (Pool_message.Error.t, t) Pool_conformist.Field.t

module Span : sig
  include module type of struct
    include Pool_core.Time.Span
  end

  val schema
    :  Pool_message.Field.t
    -> (t -> (t, Pool_message.Error.t) result)
    -> unit
    -> (Pool_message.Error.t, t) Pool_conformist.Field.t
end
