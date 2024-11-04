open Announcement
module Command = Cqrs_command.Announcement_command
open Pool_message
module Language = Pool_common.Language

let get_exn = Test_utils.get_or_failwith

module Data = struct
  let boolean_fields = Field.[ show ShowToAdmins; show ShowToContacts ]

  let text_name lang =
    Format.asprintf "%s[%s]" Field.(show Text) (Language.show lang)
  ;;

  let text_en = "EN"
  let text_de = "DE"
  let exn_opt = CCOption.get_exn_or "invalid timespan"

  module Time = struct
    open Ptime
    open Ptime_clock
    open Test_utils

    let in_an_hour = add_span (now ()) Model.hour |> exn_opt
    let in_two_hours = add_span (now ()) Model.two_hours |> exn_opt
    let an_hour_ago = sub_span (now ()) Model.hour |> exn_opt
    let two_hours_ago = sub_span (now ()) Model.two_hours |> exn_opt
  end

  let start_at = StartAt.create Time.in_an_hour
  let end_at = EndAt.create Time.in_two_hours

  let text =
    Text.create [ Language.En, text_en; Language.De, text_de ] |> get_exn
  ;;

  let show_to_admins = ShowToAdmins.create true
  let show_to_contacts = ShowToContacts.create true

  let urlencoded =
    [ text_name Language.De, text_de
    ; text_name Language.En, text_en
    ; Field.(show Start), start_at |> StartAt.value |> Ptime.to_rfc3339
    ; Field.(show End), end_at |> EndAt.value |> Ptime.to_rfc3339
    ; Field.(show ShowToAdmins), "true"
    ; Field.(show ShowToContacts), "true"
    ]
    |> CCList.map (fun (k, v) -> k, [ v ])
    |> Http_utils.format_request_boolean_values boolean_fields
  ;;
end

let create () =
  let id = Id.create () in
  let create = create ~id in
  let tenant_ids = Pool_tenant.[ Id.create () ] in
  let run_test ?(tenant_ids = tenant_ids) urlencoded expected msg =
    let open CCResult in
    let result =
      let open Command.Create in
      urlencoded |> decode >>= handle ~id tenant_ids
    in
    Test_utils.check_result ~msg expected result
  in
  let urlencoded_remove = Test_utils.urlencoded_remove Data.urlencoded in
  let urlencoded_update = Test_utils.urlencoded_update Data.urlencoded in
  (* CREATE ALL SET *)
  let expected =
    let open Data in
    let announcement =
      create text (Some start_at) (Some end_at) show_to_admins show_to_contacts
    in
    Ok [ Created (announcement, tenant_ids) |> Pool_event.announcement ]
  in
  let () = run_test Data.urlencoded expected "create all set" in
  (* CREATE NO START / NO END *)
  let updates key = CCList.mem key Field.[ show Start; show End ] in
  let expected =
    let open Data in
    let announcement = create text None None show_to_admins show_to_contacts in
    Ok [ Created (announcement, tenant_ids) |> Pool_event.announcement ]
  in
  let () =
    run_test (urlencoded_remove updates) expected "create no start / no end"
  in
  (* CREATE START AFTER END *)
  let updates =
    let open Ptime in
    [ ( CCString.equal Field.(show Start)
      , add_span (Data.start_at |> StartAt.value) Test_utils.Model.two_hours
        |> CCOption.get_exn_or "invalid time"
        |> to_rfc3339 )
    ]
  in
  let expected = Error Error.EndBeforeStart in
  let () =
    run_test (urlencoded_update updates) expected "create start after end"
  in
  (* CREATE WITHOUT TEXT *)
  let expected = Error (Error.AtLeastOneLanguageRequired Field.Text) in
  let updates = CCString.starts_with ~prefix:Field.(show Text) in
  let () =
    run_test (urlencoded_remove updates) expected "create without text"
  in
  (* CREATE WITHOUT NO DISPLAY FLAG *)
  let expected =
    Error Error.(AtLeastOneSelected (Field.ShowToAdmins, Field.ShowToContacts))
  in
  let updates key =
    CCList.mem key Field.[ show ShowToAdmins; show ShowToContacts ]
  in
  let () =
    run_test (urlencoded_remove updates) expected "create with no display flag"
  in
  ()
;;

let find_current _ () =
  let open Utils.Lwt_result.Infix in
  let open Integration_utils in
  let pool = Database.Pool.Root.label in
  let%lwt tenant =
    Pool_tenant.find_by_label Test_utils.Data.database_label ||> get_exn
  in
  let tenand_db = tenant.Pool_tenant.database_label in
  let%lwt as_contat =
    let open Contact in
    ContactRepo.create () ||> id ||> Id.to_common ||> CCPair.make `Contact
  in
  let%lwt as_admin =
    let open Admin in
    AdminRepo.create () ||> id ||> Id.to_common ||> CCPair.make `Admin
  in
  let tenant_id = tenant.Pool_tenant.id in
  let id = Id.create () in
  let%lwt announcement = AnnouncementRepo.create ~id [ tenant_id ] in
  let run_test ?(tenants = [ tenant_id ]) announcement context found msg =
    let%lwt () = Updated (announcement, tenants) |> handle_event pool in
    let%lwt expected =
      match found with
      | false -> Lwt.return_none
      | true -> find_of_tenant tenand_db id ||> get_exn ||> CCOption.return
    in
    let%lwt res = find_by_user tenand_db context in
    Alcotest.(check (option Test_utils.annoncement) msg expected res)
    |> Lwt.return
  in
  (* GET WITHOUT START / END *)
  let%lwt () = run_test announcement as_admin true "get without start / end" in
  let%lwt () =
    let m =
      { announcement with
        show_to_admins = ShowToAdmins.create false
      ; show_to_contacts = ShowToContacts.create true
      }
    in
    let msg = Format.asprintf "no start/end, hidden from admins, as %s" in
    let%lwt () = run_test m as_admin false (msg "contact") in
    let%lwt () = run_test m as_contat true (msg "admin") in
    Lwt.return_unit
  in
  (* GET WITH PAST START *)
  let%lwt () =
    let start_at = StartAt.create Data.Time.two_hours_ago in
    let m = { announcement with start_at = Some start_at } in
    let msg = "get with past start" in
    let%lwt () = run_test m as_admin true msg in
    let%lwt () = run_test m as_contat true msg in
    Lwt.return_unit
  in
  (* GET WITH FUTURE START *)
  let%lwt () =
    let start_at = StartAt.create Data.Time.in_two_hours in
    let m = { announcement with start_at = Some start_at } in
    let msg = "get with future start" in
    let%lwt () = run_test m as_admin false msg in
    let%lwt () = run_test m as_contat false msg in
    Lwt.return_unit
  in
  (* GET WITH START AND END *)
  let%lwt () =
    let start_at = StartAt.create Data.Time.two_hours_ago in
    let end_at = EndAt.create Data.Time.in_an_hour in
    let m =
      { announcement with start_at = Some start_at; end_at = Some end_at }
    in
    let msg = Format.asprintf "get with past start and future end, as %s" in
    let%lwt () = run_test m as_admin true (msg "admin") in
    let%lwt () = run_test m as_contat true (msg "user") in
    Lwt.return_unit
  in
  (* ON DIFFERENT TENANT *)
  let%lwt () =
    let msg = Format.asprintf "on different tenant, as %s" in
    let%lwt () =
      run_test ~tenants:[] announcement as_admin false (msg "admin")
    in
    let%lwt () =
      run_test ~tenants:[] announcement as_contat false (msg "contact")
    in
    Lwt.return_unit
  in
  (* MARK AS READ *)
  let%lwt () =
    let msg = "mark as read" in
    let%lwt () =
      [ snd as_admin; snd as_contat ]
      |> CCList.map (fun id -> Hidden (announcement, id))
      |> Lwt_list.iter_s (handle_event pool)
    in
    let%lwt () = run_test announcement as_admin false msg in
    let%lwt () = run_test announcement as_contat false msg in
    Lwt.return_unit
  in
  Lwt.return_unit
;;
