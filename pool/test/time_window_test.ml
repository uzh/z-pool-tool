open Test_utils

let database_label = Data.database_label

let find_overlapping _ () =
  let open Integration_utils in
  let date_to_ptime date =
    date |> Ptime.of_date |> CCOption.get_exn_or "Invalid date"
  in
  let create_start date = date |> date_to_ptime |> Session.Start.create in
  let create_end date = date |> date_to_ptime |> Session.End.create in
  let%lwt experiment = ExperimentRepo.create () in
  let start = 2024, 4, 1 in
  let end_at = 2024, 4, 10 in
  let duration =
    Time_window.duration ~start:(create_start start) ~end_at:(create_end end_at)
    |> get_or_failwith
  in
  let%lwt time_window =
    TimeWindowRepo.create (create_start start) duration experiment ()
  in
  let run ~start ~end_at expected =
    let start = create_start start in
    let end_at = create_end end_at in
    let%lwt result =
      Time_window.find_overlapping
        database_label
        experiment.Experiment.id
        ~start
        ~end_at
    in
    Alcotest.(check (list time_window_testable) "succeeds") expected result
    |> Lwt.return
  in
  let start_earlier = 2024, 3, 30 in
  let start_later = 2024, 4, 5 in
  let end_earlier = 2024, 4, 5 in
  let end_later = 2024, 5, 1 in
  (* START EARLIER *)
  let%lwt () = run ~start:start_earlier ~end_at [ time_window ] in
  let%lwt () = run ~start:start_earlier ~end_at:end_later [ time_window ] in
  let%lwt () = run ~start:start_earlier ~end_at:end_earlier [ time_window ] in
  (* START LATER *)
  let%lwt () = run ~start:start_later ~end_at [ time_window ] in
  let%lwt () = run ~start:start_later ~end_at:end_later [ time_window ] in
  let%lwt () = run ~start:start_later ~end_at:end_earlier [ time_window ] in
  (* START AT THE SAME TIME *)
  let%lwt () = run ~start ~end_at [ time_window ] in
  let%lwt () = run ~start ~end_at:end_later [ time_window ] in
  let%lwt () = run ~start ~end_at:end_earlier [ time_window ] in
  (* NO OVERLAPPING *)
  let%lwt () = run ~start:(2024, 5, 1) ~end_at:(2024, 5, 2) [] in
  let%lwt () = run ~start:(2024, 3, 20) ~end_at:(2024, 3, 21) [] in
  Lwt.return_unit
;;
