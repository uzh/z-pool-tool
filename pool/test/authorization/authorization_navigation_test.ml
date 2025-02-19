open Authorization_test_utils
open Alcotest
open Layout.Navigation
module NavLinks = NavElements.AdminTenantItems

let pool = Test_utils.Data.database_label
let testable = NavElement.(testable pp equal)
let filter = NavUtils.filter_items ~validate:true

let admin_navigation _ () =
  let open Integration_utils in
  let open NavLinks in
  let%lwt actor = create_admin_actor () in
  let filter links =
    let%lwt guardian = actor_permissions actor in
    filter ~actor ~guardian links |> Lwt.return
  in
  let run_test msg ~expected =
    let%lwt result = filter NavLinks.all in
    check (Alcotest.list testable) msg expected result |> Lwt.return
  in
  let filter_children item compare =
    let open NavElement in
    let children = item.children |> List.filter compare in
    { item with children }
  in
  let profile_nav = NavElements.Profile.nav ~contact:false ~prefix:"/admin" () in
  (* Without any roles *)
  let expected =
    [ dashboard
    ; NavElements.Profile.nav ~contact:false ~prefix:"/admin" ()
    ; NavElement.logout ()
    ]
  in
  let%lwt () = run_test "Without any roles" ~expected in
  (* As experimenter *)
  let%lwt experiment = ExperimentRepo.create () in
  let%lwt () = assign_role actor `Experimenter (Some (experiment_target experiment)) in
  let expected = [ dashboard; experiments; profile_nav; NavElement.logout () ] in
  let%lwt () = run_test "As experimenter" ~expected in
  (* As assistant *)
  let%lwt () = assign_role actor `Assistant (Some (experiment_target experiment)) in
  let expected =
    let user =
      filter_children user (fun x -> x.NavElement.label <> Pool_common.I18n.Admins)
    in
    [ dashboard; experiments; user; profile_nav; NavElement.logout () ]
  in
  let%lwt () = run_test "As assistant" ~expected in
  (* As location manager *)
  let%lwt () = revoke_role actor `Assistant (Some (experiment_target experiment)) in
  let%lwt () = revoke_role actor `Experimenter (Some (experiment_target experiment)) in
  let%lwt () = assign_role actor `LocationManager None in
  let expected =
    let settings =
      filter_children settings (fun x -> x.NavElement.label == Pool_common.I18n.Locations)
    in
    [ dashboard; settings; profile_nav; NavElement.logout () ]
  in
  let%lwt () = run_test "As location manager" ~expected in
  (* As operator *)
  let%lwt () = assign_role actor `Operator None in
  let%lwt () = run_test "As operator" ~expected:NavLinks.all in
  Lwt.return ()
;;
