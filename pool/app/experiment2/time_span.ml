type t = Ptime.Span.t [@@deriving eq, show]

let make m = Ok m

let t_of_yojson m =
  m
  |> Yojson.Safe.to_string
  |> CCInt.of_string
  |> CCOption.map Ptime.Span.of_int_s
  |> CCOption.get_exn_or "Invalid timespan provided"
;;

let yojson_of_t m =
  m
  |> Ptime.Span.to_int_s
  |> CCOption.map (fun i -> i |> CCInt.to_string |> Yojson.Safe.from_string)
  |> CCOption.get_exn_or "Invalid timespan provided"
;;

let schema =
  let open Caqti_type in
  let encode span = Ok span in
  let decode = encode in
  custom ~encode ~decode ptime_span
;;
