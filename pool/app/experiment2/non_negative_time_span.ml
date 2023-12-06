open Containers

type t = Ptime.Span.t [@@deriving eq, show]

let make m =
  if Ptime.Span.abs m |> Ptime.Span.equal m
  then Time_span.make m
  else Error (`Time_error `Negative_amount)
;;

let t_of_yojson = Time_span.t_of_yojson
let yojson_of_t = Time_span.yojson_of_t

let schema =
  let open Caqti_type in
  let encode span =
    make span
    |> Result.map_err (fun (`Time_error `Negative_amount) ->
      "Expected non-negative timestamp")
  in
  custom ~encode ~decode:(fun span -> Ok span) ptime_span
;;
