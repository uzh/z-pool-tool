open Authorization_test_utils
open Alcotest
open Layout.Navigation
module NavLinks = NavElements.AdminTenantItems

let pool = Test_utils.Data.database_label
let testable = NavElement.(testable pp equal)
let filter = NavUtils.filter_items ~validate:true

let filter_links actor links =
  let%lwt guardian = actor_permissions actor in
  filter ~actor ~guardian links |> Lwt.return
;;

let run_test msg actor ~expected =
  let open Utils.Lwt_result.Infix in
  let%lwt result = NavLinks.all pool >|> filter_links actor in
  check (Alcotest.list testable) msg expected result |> Lwt.return
;;

let user_profile_navigation _ () =
  let open Pool_common.I18n in
  let open Gtx_config in
  let handle_events = Lwt_list.iter_s (handle_event pool) in
  let testable = Alcotest.(list (pair string (testable pp_nav_link equal_nav_link))) in
  let%lwt api_config = Gtx_config.find_exn pool in
  let nav_items =
    [ "/user/login-information", LoginInformation
    ; "/user/personal-details", PersonalDetails
    ]
  in
  (* Without GTX API key *)
  let%lwt () = [ Removed; CacheCleared ] |> handle_events in
  let%lwt nav = NavElements.Profile.dropdown_items pool ~contact:true () in
  Alcotest.check testable "without api key" nav_items nav;
  (* With GTX API key *)
  let%lwt () = [ Created api_config; CacheCleared ] |> handle_events in
  let%lwt nav = NavElements.Profile.dropdown_items pool ~contact:true () in
  let expected = nav_items @ [ "/user/contact-information", ContactInformation ] in
  Alcotest.check testable "with api key" expected nav;
  handle_event pool CacheCleared
;;

let admin_navigation _ () =
  let open Integration_utils in
  let open NavLinks in
  let%lwt actor = create_admin_actor () in
  let filter_children item compare =
    let open NavElement in
    let children = item.children |> List.filter compare in
    { item with children }
  in
  let%lwt profile_nav = NavElements.Profile.nav pool ~contact:false ~prefix:"/admin" () in
  (* Without any roles *)
  let expected = [ dashboard; profile_nav; NavElement.logout () ] in
  let%lwt () = run_test "Without any roles" actor ~expected in
  (* As experimenter *)
  let%lwt experiment = ExperimentRepo.create () in
  let%lwt () = assign_role actor `Experimenter (Some (experiment_target experiment)) in
  let expected = [ dashboard; experiments; profile_nav; NavElement.logout () ] in
  let%lwt () = run_test "As experimenter" actor ~expected in
  (* As assistant *)
  let%lwt () = assign_role actor `Assistant (Some (experiment_target experiment)) in
  let expected =
    let user =
      filter_children user (fun x -> x.NavElement.label <> Pool_common.I18n.Admins)
    in
    [ dashboard; experiments; user; profile_nav; NavElement.logout () ]
  in
  let%lwt () = run_test "As assistant" actor ~expected in
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
  let%lwt () = run_test "As location manager" actor ~expected in
  (* With specific role *)
  let open Pool_common.I18n in
  let test_cases : (Role.Target.t * nav_link list) list =
    [ `CustomField, [ CustomFields ]
    ; `Filter, [ Filter ]
    ; `Queue, [ Queue ]
    ; `SystemSetting, [ SystemSettings; TextMessages ]
    ; `Schedule, [ Schedules ]
    ; `Smtp, [ Smtp ]
    ; `Permission, [ RolePermissions; ActorPermissions ]
    ; `Tag, [ Tags ]
    ; `MessageTemplate, [ MessageTemplates ]
    ; `I18n, [ I18n ]
    ; `SignupCode, [ SignupCodes ]
    ; `OrganisationalUnit, [ OrganisationalUnits ]
    ; `ApiKey, [ ApiKeys ]
    ]
  in
  let%lwt (_ : nav_link list) =
    Lwt_list.fold_left_s
      (fun nav_links (target, labels) ->
         let%lwt () =
           create_actor_model_permission actor Guard.Permission.Manage target
         in
         let nav_links = nav_links @ labels in
         let expected =
           let settings =
             filter_children settings (fun x -> CCList.mem x.NavElement.label nav_links)
           in
           [ dashboard; settings; profile_nav; NavElement.logout () ]
         in
         let msg = Format.asprintf "With %a access" Role.Target.pp target in
         let%lwt () = run_test msg actor ~expected in
         Lwt.return nav_links)
      [ Locations ]
      test_cases
  in
  Lwt.return ()
;;

let operator_navigation _ () =
  let open Integration_utils in
  let%lwt actor = create_admin_actor () in
  let%lwt () = assign_role actor `Operator None in
  let%lwt expected = NavLinks.all pool in
  let%lwt () = run_test "As operator" actor ~expected in
  Lwt.return_unit
;;
