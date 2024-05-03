open Test_utils
module Command = Cqrs_command.Session_command

let database_label = Data.database_label

let date_to_ptime date =
  date |> Ptime.of_date |> CCOption.get_exn_or "Invalid date"
;;

let create_start date = date |> date_to_ptime |> Session.Start.create
let create_end date = date |> date_to_ptime |> Session.End.create

let check_result ?(msg = "succeeds") =
  let open Alcotest in
  check (result (list event) error) msg
;;

module Data = struct
  open Session
  open Pool_message

  let start = create_start (2024, 04, 01)
  let end_at = create_end (2024, 05, 01)
  let internal_description = InternalDescription.of_string "internal"
  let public_description = PublicDescription.of_string "public"

  let urlencoded =
    [ Field.Start, Start.value start |> Ptime.to_rfc3339
    ; Field.End, End.value end_at |> Ptime.to_rfc3339
    ; Field.InternalDescription, InternalDescription.value internal_description
    ; Field.PublicDescription, PublicDescription.value public_description
    ]
    |> CCList.map (fun (field, value) -> Field.show field, [ value ])
  ;;
end

let find_overlapping _ () =
  let open Integration_utils in
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

let create_timewindow () =
  let open CCResult.Infix in
  let experiment = Test_utils.Model.create_experiment () in
  let id = Session.Id.create () in
  let make_duration start end_at =
    Time_window.duration ~start ~end_at |> get_or_failwith
  in
  let events =
    let open Command.CreateTimeWindow in
    Data.urlencoded |> decode >>= handle ~id ~overlapps:[] experiment
  in
  let expected =
    let open Data in
    let duraction = make_duration start end_at in
    let time_window =
      Time_window.create
        ~id
        ~internal_description
        ~public_description
        start
        duraction
        experiment
    in
    Ok [ Time_window.Created time_window |> Pool_event.time_window ]
  in
  check_result expected events;
  let events =
    let open Command.CreateTimeWindow in
    let overlapps = [ Model.create_timewindow ~experiment () ] in
    Data.urlencoded |> decode >>= handle ~id ~overlapps experiment
  in
  let expected = Error Pool_message.Error.SessionOverlap in
  check_result expected events
;;
