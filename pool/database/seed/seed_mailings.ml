let () = CCRandom.self_init ()
let get_or_failwith = Pool_common.Utils.get_or_failwith

let generate_end (start : Mailing.StartAt.t) =
  let rand_day = CCRandom.(run (int_range 0 8)) in
  let delta_hour = 1 in
  start
  |> Mailing.StartAt.value
  |> CCFun.flip
       Ptime.add_span
       (Ptime.Span.of_int_s ((rand_day * 24) + (delta_hour * 3600)))
  |> CCOption.get_exn_or
       "Mailing series seed: Could not generate mailing end ptime."
  |> Mailing.EndAt.create
  |> get_or_failwith
;;

let generate_rate () =
  CCRandom.(run (int_range 1 5)) * 100 |> Mailing.Rate.create |> get_or_failwith
;;

let generate_events experiments =
  let n_timestamps = CCList.range_by ~step:5 0 180 in
  CCList.map
    (fun delta ->
      let start =
        Ptime_clock.now ()
        |> CCFun.flip
             Ptime.add_span
             (Ptime.Span.of_int_s ((delta * 24 * 3600) + 1))
        |> CCOption.get_exn_or
             "Mailing series seed: could not generate mailings."
        |> Mailing.StartAt.create
        |> get_or_failwith
      in
      Mailing.Created
        ( Mailing.create start (generate_end start) (generate_rate ()) None
          |> Pool_common.Utils.get_or_failwith
        , CCList.(CCRandom.run (random_choose experiments)) ))
    n_timestamps
;;

let create pool =
  let open Lwt.Infix in
  Experiment.find_all pool ()
  >|= CCList.map (fun m -> m.Experiment.id)
  >|= generate_events
  >>= Lwt_list.iter_s (Mailing.handle_event pool)
;;
