(* TODO [aerben] move everything to pool_common utils *)
let decimal n =
  if n < 10 then Format.asprintf "0%d" n else Format.asprintf "%d" n
;;

let formatted_date date =
  let year, month, day = date in
  Format.asprintf "%s.%s.%d" (decimal day) (decimal month) year
;;

let formatted_time time =
  let (h, m, s), _ = time in
  Format.asprintf "%s:%s:%s" (decimal h) (decimal m) (decimal s)
;;

let formatted_date_time (date : Ptime.t) =
  Format.asprintf
    "%s %s"
    (formatted_date (Ptime.to_date date))
    (formatted_time (Ptime.to_date_time date))
;;

let ptime_to_sexp p =
  let formatted = p |> formatted_date_time in
  Sexplib0.Sexp.Atom formatted
;;
