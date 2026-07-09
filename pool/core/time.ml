(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/core_time.ml
   Unused functions ([date_from_now], the ptime (de)serializers and most of the [Span]
   helpers) have been removed. *)

type duration =
  | OneSecond
  | OneMinute
  | TenMinutes
  | OneHour
  | OneDay
  | OneWeek
  | OneMonth
  | OneYear
[@@deriving show, eq]

let duration_to_span duration =
  let duration_s =
    match duration with
    | OneSecond -> 1.
    | OneMinute -> 60.
    | TenMinutes -> 60. *. 10.
    | OneHour -> 60. *. 60.
    | OneDay -> 60. *. 60. *. 24.
    | OneWeek -> 60. *. 60. *. 24. *. 7.
    | OneMonth -> 60. *. 60. *. 24. *. 30.
    | OneYear -> 60. *. 60. *. 24. *. 365.
  in
  match Ptime.of_float_s duration_s with
  | Some ptime -> Ptime.to_span ptime
  | None -> failwith "Invalid ptime provided"
;;

module Span = struct
  let minutes n = Ptime.Span.of_int_s (60 * n)
  let hours n = Ptime.Span.of_int_s (60 * 60 * n)
end
