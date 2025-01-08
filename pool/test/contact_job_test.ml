open Utils.Lwt_result.Infix
open Integration_utils

let pool = Test_utils.Data.database_label
let get_exn = Test_utils.get_or_failwith

module Utils = struct
  let init () =
    let open ContactRepo in
    let%lwt c1 = create () in
    let%lwt c2 = create () in
    Lwt.return (c1, c2)
  ;;

  let update_sign_in_at contact timestamp =
    let request =
      let open Caqti_request.Infix in
      {sql|
      UPDATE
        pool_contacts
      SET
        last_sign_in_at = $1
      WHERE
        user_uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
      |> Caqti_type.(t2 ptime Contact.Repo.Id.t ->. unit)
    in
    Database.exec pool request (timestamp, Contact.id contact)
  ;;
end

let find_contacts_to_remind _ () =
  let open Utils in
  let open Message_template in
  let testable =
    let open Alcotest in
    result
      (pair
         (list Email.(testable pp_dispatch equal_dispatch))
         (list Contact.(testable pp_event equal_event)))
      Test_utils.error
  in
  let find_to_warn = Contact_job.find_to_warn_about_inactivity pool in
  let%lwt make_warning_msg = InactiveContactWarning.prepare pool ||> get_exn in
  let%lwt _ = InactiveContactDeactivation.prepare pool ||> get_exn in
  let%lwt c1, c2 = init () in
  let n_days n = 60 * 60 * 24 * n |> Ptime.Span.of_int_s in
  let time_ago span =
    Ptime.(sub_span (Ptime_clock.now ()) span) |> CCOption.get_exn_or "Invalid ptime"
  in
  let disable_after = Settings.InactiveUser.DisableAfter.create (n_days 10) |> get_exn in
  let warn_after = [ n_days 5 ] in
  (* Dont expect anyone to be disabled*)
  let%lwt events =
    Contact_job.Inactivity.handle_disable_contacts pool disable_after warn_after
  in
  Alcotest.check testable "No contact found to deactivage" (Ok ([], [])) events;
  let%lwt () = update_sign_in_at c1 (time_ago (n_days 1)) in
  let%lwt () = update_sign_in_at c2 (time_ago (n_days 6)) in
  (* Expect 1 contact to be notified *)
  let%lwt events = Contact_job.Inactivity.handle_contact_warnings pool warn_after in
  let%lwt expected =
    let%lwt msg = make_warning_msg c2 ||> get_exn in
    let event = Contact.NotifiedAbountInactivity c2 in
    Lwt_result.return ([ msg ], [ event ])
  in
  Alcotest.check testable "Notify c2 about inactivity" expected events;
  let%lwt to_notify = find_to_warn warn_after in
  Alcotest.(check (list Test_utils.contact) "Notify contact 2" to_notify [ c2 ]);
  (* Handle notification event, do not expect any returns *)
  let%lwt () =
    let open Contact in
    to_notify
    |> Lwt_list.iter_s (fun contact ->
      NotifiedAbountInactivity contact |> handle_event pool)
  in
  let%lwt to_notify = find_to_warn warn_after in
  Alcotest.(check (list Test_utils.contact) "Notify no one " to_notify []);
  (* Expect contact 1 to be returned *)
  let%lwt () = update_sign_in_at c1 (time_ago (n_days 6)) in
  let%lwt to_notify = find_to_warn warn_after in
  Alcotest.(check (list Test_utils.contact) "Notify contact 1" to_notify []);
  Lwt.return_unit
;;
