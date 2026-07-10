let hour = 60 * 60
let ptime = Alcotest.testable Ptime.pp Ptime.equal

let time str =
  match Ptime.of_rfc3339 str with
  | Ok (time, _, _) -> time
  | Error _ -> failwith (Format.asprintf "Invalid time: %s" str)
;;

(* The Zurich offset is derived from the EU DST rule and must not depend on the
   host's TZ setting: CEST from the last Sunday of March, 01:00 UTC, until the
   last Sunday of October, 01:00 UTC (2026: March 29 / October 25). *)
let zurich_offsets () =
  let check name expected str =
    Alcotest.(
      check int name (expected * hour) (Utils.Ptime.to_zurich_tz_offset_s (time str)))
  in
  check "winter is CET (+01:00)" 1 "2026-01-15T12:00:00Z";
  check "summer is CEST (+02:00)" 2 "2026-07-15T12:00:00Z";
  check "last second of CET in March" 1 "2026-03-29T00:59:59Z";
  check "first second of CEST in March" 2 "2026-03-29T01:00:00Z";
  check "last second of CEST in October" 2 "2026-10-25T00:59:59Z";
  check "first second of CET in October" 1 "2026-10-25T01:00:00Z"
;;

let zurich_formatting () =
  let check name expected str =
    Alcotest.(check string name expected (Utils.Ptime.formatted_date_time (time str)))
  in
  check "UTC noon renders as 13:00 in winter" "15.01.2026 13:00" "2026-01-15T12:00:00Z";
  check "UTC noon renders as 14:00 in summer" "15.07.2026 14:00" "2026-07-15T12:00:00Z"
;;

(* Serialised timestamps must carry explicit timezone information: "Z", not
   Ptime's default "-00:00" (RFC 3339 for "offset unknown"). *)
let rfc3339_stamps_utc () =
  let instant = time "2026-07-15T12:00:00Z" in
  Alcotest.(
    check
      string
      "to_rfc3339 stamps the explicit UTC offset"
      "2026-07-15T12:00:00Z"
      (Utils.Ptime.to_rfc3339 instant));
  Alcotest.(
    check
      string
      "yojson serialisation carries the UTC offset"
      {|"2026-07-15T12:00:00Z"|}
      (Yojson.Safe.to_string (Utils.Ptime.yojson_of_ptime instant)))
;;

let parse_date_from_calendar () =
  let parse = Pool_model.Time.parse_date_from_calendar in
  let check name expected str =
    Alcotest.(check ptime name (time expected) (parse str |> Test_utils.get_or_failwith))
  in
  (* FullCalendar sends range bounds with the browser's UTC offset; the exact
     instant is used, not the truncated UTC date *)
  check "instant with offset" "2026-03-28T22:00:00Z" "2026-03-29T00:00:00+02:00";
  check "instant in UTC" "2026-03-29T22:00:00Z" "2026-03-29T22:00:00Z";
  check "date-only falls back to midnight UTC" "2026-03-29T00:00:00Z" "2026-03-29";
  Alcotest.(
    check bool "invalid input is rejected" true (parse "not-a-date" |> CCResult.is_error))
;;
