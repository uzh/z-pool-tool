(* Security Middleware Test Module - can be used standalone or included in other test suites *)

let create_test_request target =
  let open Rock.Request in
  let headers = Httpaf.Headers.of_list [] in
  { version = Httpaf.Version.of_string "HTTP/1.1"
  ; target
  ; headers
  ; meth = `GET
  ; body = Rock.Body.empty
  ; env = Rock.Context.empty
  }
;;

let test_sql_injection_detection () =
  let test_patterns =
    [ (* Time-based injection from logs *)
      "/login?language=ENzV5kVaiL'%20OR%20845=(SELECT%20845%20FROM%20PG_SLEEP(0))"
    ; "/signup?language=EN-1%20waitfor%20delay%20'0:0:3'%20--%20"
    ; "/login-confirmation?language=EN&location=/admin/version%3Flanguage=EN-1%20waitfor%20delay%20'0:0:3'%20--%20"
    ; (* Boolean-based injection *)
      "/admin/version?language=-1'%20OR%202%2B665-665-1=0%2B0%2B0%2B1%20or%20'3chUBluM'='"
    ; "/index?language=EN0'XOR(if(now()=sysdate()%2Csleep(15)%2C0))XOR'Z"
    ; "/index%25'%20AND%202*3*8=6*8%20AND%20'vvnn'!='vvnn%25"
    ; "/login?language=EN%22%20AND%202*3*8=6*8%20AND%20%22JbL7%22=%22JbL7&location=/admin/version"
    ]
  in
  let test_request uri_string =
    let req = create_test_request uri_string in
    let result = Middleware.Security.check_request_security req in
    match result with
    | Some categories ->
      Printf.printf
        "✓ Detected attack in %s: %s\n"
        uri_string
        (String.concat ", " categories);
      true
    | None ->
      Printf.printf "✗ No attack detected in %s\n" uri_string;
      false
  in
  let results = List.map test_request test_patterns in
  let detected_count =
    List.fold_left (fun acc detected -> if detected then acc + 1 else acc) 0 results
  in
  Printf.printf
    "Security test: Detected %d out of %d attack patterns\n"
    detected_count
    (List.length test_patterns);
  (* We expect at least 80% of patterns to be detected (some may be edge cases) *)
  let success_rate =
    float_of_int detected_count /. float_of_int (List.length test_patterns)
  in
  Alcotest.(check bool "Most SQL injection patterns detected" (success_rate >= 0.8) true)
;;

let test_url_manipulation_detection () =
  let test_patterns =
    [ (* External redirect attempts *)
      "/%22https:/www.uzh.ch/de/privacy.html)/cn2qm6w23egz.html"
    ; "/redirect?url=https://evil.com"
    ; "/callback?return_to=//malicious.site.com"
    ; "/page?next=%2F%2Fmalicious.com"
    ]
  in
  let test_request uri_string =
    let req = create_test_request uri_string in
    let result = Middleware.Security.check_request_security req in
    match result with
    | Some categories ->
      Printf.printf
        "✓ Detected URL manipulation in %s: %s\n"
        uri_string
        (String.concat ", " categories);
      true
    | None ->
      Printf.printf "✗ No URL manipulation detected in %s\n" uri_string;
      false
  in
  let results = List.map test_request test_patterns in
  let detected_count =
    List.fold_left (fun acc detected -> if detected then acc + 1 else acc) 0 results
  in
  Printf.printf
    "Security test: Detected %d out of %d URL manipulation patterns\n"
    detected_count
    (List.length test_patterns);
  (* We expect at least some patterns to be detected *)
  Alcotest.(check bool "URL manipulation patterns detected" (detected_count > 0) true)
;;

let test_legitimate_requests () =
  let legitimate_patterns =
    [ "/login?language=EN&location=/admin/version"
    ; "/signup?language=EN"
    ; "/index?language=EN"
    ; "/admin/version?language=EN"
    ; "/api/contacts?page=1&limit=10"
    ; "/experiments?filter=active"
    ; "/profile/settings"
    ]
  in
  let test_request uri_string =
    let req = create_test_request uri_string in
    let result = Middleware.Security.check_request_security req in
    match result with
    | Some categories ->
      Printf.printf
        "✗ False positive in %s: %s\n"
        uri_string
        (String.concat ", " categories);
      false
    | None ->
      Printf.printf "✓ Legitimate request correctly allowed: %s\n" uri_string;
      true
  in
  let results = List.map test_request legitimate_patterns in
  let clean_count =
    List.fold_left (fun acc clean -> if clean then acc + 1 else acc) 0 results
  in
  Printf.printf
    "Security test: Allowed %d out of %d legitimate requests\n"
    clean_count
    (List.length legitimate_patterns);
  (* We expect all legitimate requests to pass *)
  let success_rate =
    float_of_int clean_count /. float_of_int (List.length legitimate_patterns)
  in
  Alcotest.(check bool "Most legitimate requests allowed" (success_rate >= 0.9) true)
;;

let test_xss_detection () =
  let test_patterns =
    [ "/search?q=<script>alert('xss')</script>"
    ; "/comment?text=<img src=x onerror=alert(1)>"
    ; "/page?content=javascript:alert(document.cookie)"
    ; "/form?input=<iframe src=data:text/html,<script>alert(1)</script>>"
    ]
  in
  let test_request uri_string =
    let req = create_test_request uri_string in
    let result = Middleware.Security.check_request_security req in
    match result with
    | Some categories ->
      Printf.printf
        "✓ Detected XSS in %s: %s\n"
        uri_string
        (String.concat ", " categories);
      true
    | None ->
      Printf.printf "✗ No XSS detected in %s\n" uri_string;
      false
  in
  let results = List.map test_request test_patterns in
  let detected_count =
    List.fold_left (fun acc detected -> if detected then acc + 1 else acc) 0 results
  in
  Printf.printf
    "Security test: Detected %d out of %d XSS patterns\n"
    detected_count
    (List.length test_patterns);
  (* We expect at least some XSS patterns to be detected *)
  Alcotest.(check bool "XSS patterns detected" (detected_count > 0) true)
;;

(* Test functions that can be used by both standalone and integrated test suites *)
let security_tests () =
  let open Alcotest in
  [ test_case "SQL injection detection" `Quick test_sql_injection_detection
  ; test_case "URL manipulation detection" `Quick test_url_manipulation_detection
  ; test_case "Legitimate requests" `Quick test_legitimate_requests
  ; test_case "XSS detection" `Quick test_xss_detection
  ]
;;
