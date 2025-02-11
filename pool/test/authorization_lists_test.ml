open Utils.Lwt_result.Infix

let pool = Test_utils.Data.database_label
let to_role (admin, role, target_uuid) = Guard.ActorRole.create ?target_uuid admin role

let create_actor () =
  Integration_utils.AdminRepo.create ()
  ||> Pool_context.admin
  >|> Pool_context.Utils.find_authorizable pool
  ||> Test_utils.get_or_failwith
;;

let experiment_target { Experiment.id; _ } =
  id |> Guard.Uuid.target_of Experiment.Id.value
;;

let assign_role { Guard.Actor.uuid; _ } role target =
  let open Guard in
  RolesGranted [ (uuid, role, target) |> to_role ] |> handle_event pool
;;

let revoke_role { Guard.Actor.uuid; _ } role target =
  let open Guard in
  RolesRevoked [ (uuid, role, target) |> to_role ] |> handle_event pool
;;

let experiments _ () =
  let open Alcotest in
  let testable = list Test_utils.experiment in
  let sort = CCList.sort Experiment.compare in
  let%lwt actor = create_actor () in
  let%lwt all_experiments = Experiment.all pool ||> sort in
  let get_by_user () = Experiment.list_by_user pool actor ||> fst ||> sort in
  (* Without any roles *)
  let%lwt experiments = get_by_user () in
  check testable "No experiments returned" [] experiments;
  (* Assign assistant experiment 1 *)
  let first = CCList.hd all_experiments in
  let%lwt () = assign_role actor `Assistant (Some (experiment_target first)) in
  let%lwt experiments = get_by_user () in
  check testable "first experiment returned" [ first ] experiments;
  (* Assign experimenter experiment 2 *)
  let second = CCList.nth all_experiments 2 in
  let%lwt () = assign_role actor `Experimenter (Some (experiment_target second)) in
  let%lwt experiments = get_by_user () in
  check testable "first and second experiments returned" [ first; second ] experiments;
  (* Assign global assistant *)
  let%lwt () = assign_role actor `Assistant None in
  let%lwt experiments = get_by_user () in
  check testable "all experiments returned" all_experiments experiments;
  (* Revoke global assistant *)
  let%lwt () = revoke_role actor `Assistant None in
  let%lwt experiments = get_by_user () in
  check testable "first and second experiments returned" [ first; second ] experiments;
  (* Revoke experimenter experiment 2 *)
  let%lwt () = revoke_role actor `Experimenter (Some (experiment_target second)) in
  let%lwt experiments = get_by_user () in
  check testable "first experiment returned" [ first ] experiments;
  Lwt.return ()
;;

let locations _ () =
  let open Alcotest in
  let testable = list Test_utils.location in
  let sort = CCList.sort Pool_location.compare in
  let%lwt actor = create_actor () in
  let get_by_user () = Pool_location.list_by_user pool actor ||> fst ||> sort in
  (* Without any roles *)
  let%lwt locations = get_by_user () in
  check testable "No locations returned" [] locations;
  Lwt.return ()
;;
