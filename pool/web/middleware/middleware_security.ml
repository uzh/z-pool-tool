(* Enhanced SQL injection patterns based on real attack examples *)
let sql_injection_patterns =
  [ (* Basic SQL keywords *)
    "union select"
  ; "select * from"
  ; "insert into"
  ; "update "
  ; "delete from"
  ; "drop table"
  ; "drop database"
  ; "exec("
  ; "execute("
  ; "sp_executesql"
  ; "; --"
  ; "' or '1'='1"
  ; "' or 1=1"
  ; "admin'--"
  ; "hex("
  ; "char("
  ; "concat("
  ; "substring("
  ; "load_file("
  ; "into outfile"
  ; "into dumpfile" (* Time-based injection patterns from real attacks *)
  ; "pg_sleep("
  ; "sleep("
  ; "waitfor delay"
  ; "benchmark("
  ; "select 845 from pg_sleep" (* Boolean-based injection patterns *)
  ; " or 845="
  ; " and 2*3*8=6*8"
  ; " or 2+665-665-1=0+0+0+1"
  ; "xor(if(now()=sysdate()"
  ; "'xor'"
  ; " and 'vvnn'!='vvnn" (* Common injection markers and operators *)
  ; "' or "
  ; "' and "
  ; "' xor "
  ; "\" and "
  ; "\" or "
  ; "%20or%20"
  ; "%20and%20"
  ; "%27%20or%20"
  ; "%27%20and%20" (* MySQL specific functions *)
  ; "if(now()"
  ; "sysdate()"
  ; "version()"
  ; "user()"
  ; "database()" (* SQL Server specific *)
  ; "@@version"
  ; "waitfor delay '0:0:" (* PostgreSQL specific *)
  ; "pg_version"
  ; "current_user"
  ; "current_database" (* Mathematical operations in blind injection *)
  ; "+665-665"
  ; "*3*8=6*8"
  ; "=0+0+0+1"
  ; "2+2=4"
  ; "1=1"
  ; "0=0"
  ]
;;

(* XSS patterns *)
let xss_patterns =
  [ "<script"
  ; "</script>"
  ; "javascript:"
  ; "eval("
  ; "alert("
  ; "document.cookie"
  ; "window.location"
  ; "<iframe"
  ; "<object"
  ; "<embed"
  ; "<link"
  ; "<meta"
  ; "vbscript:"
  ; "expression("
  ; "onload="
  ; "onerror="
  ; "onclick="
  ]
;;

(* Directory traversal patterns *)
let path_traversal_patterns =
  [ "../"
  ; "..\\"
  ; "%2e%2e%2f"
  ; "%2e%2e%5c"
  ; "%252e%252e%252f"
  ; "%c0%ae%c0%ae%c0%af"
  ; "../../"
  ; "..\\..\\"
  ]
;;

(* Command injection patterns *)
let command_injection_patterns =
  [ "| "
  ; "; "
  ; "&& "
  ; "$("
  ; "`"
  ; "netmod"
  ; "wget "
  ; "curl "
  ; "nc "
  ; "netcat"
  ; "cat /etc/passwd"
  ; "ls -"
  ; "ps "
  ; "kill "
  ; "rm -"
  ; "mkdir "
  ; "chmod "
  ; "chown "
  ]
;;

(* URL manipulation and redirect patterns *)
let url_manipulation_patterns =
  [ (* External domain redirects - more specific *)
    "://www."
  ; "//www."
  ; "%2F%2Fwww"
  ; "https://evil"
  ; "http://malicious"
  ; "javascript:"
  ; "data:"
  ; "vbscript:" (* Suspicious URL structures - specific to attacks *)
  ; "/cn2qm6w23egz.html"
  ; ")/cn2qm6w23egz"
  ; "\"/cn2qm6w23egz"
  ; "%22https:/"
  ; "%22http:/"
  ; "\");"
  ; "');alert"
  ; "\";alert" (* Double slashes indicating redirect attempts *)
  ; "//malicious"
  ; "//evil"
  ; "return_to=//"
  ; "redirect=//"
  ; "url=//"
  ; "next=//"
  ]
;;

(* LDAP injection patterns *)
let ldap_injection_patterns = [ "*)()"; "*)(&"; "*)(objectclass=*"; ")(cn=*"; ")(()" ]

(* Other suspicious patterns *)
let misc_suspicious_patterns =
  [ "<?php"
  ; "<%"
  ; "%>"
  ; "${"
  ; "#{"
  ; "eval"
  ; "exec"
  ; "system"
  ; "passthru"
  ; "shell_exec"
  ; "proc_open"
  ; "popen"
  ; "file_get_contents"
  ; "file_put_contents"
  ; "fopen"
  ; "fwrite"
  ; "include"
  ; "require"
  ; "../../../"
  ]
;;

let all_patterns =
  [ "sql_injection", sql_injection_patterns
  ; "xss", xss_patterns
  ; "path_traversal", path_traversal_patterns
  ; "command_injection", command_injection_patterns
  ; "url_manipulation", url_manipulation_patterns
  ; "ldap_injection", ldap_injection_patterns
  ; "misc_suspicious", misc_suspicious_patterns
  ]
;;

let normalize_string s =
  (* URL decode the string first to catch encoded attacks *)
  let url_decoded =
    try Uri.pct_decode s with
    | _ -> s (* If decoding fails, use original string *)
  in
  (* Also check double-encoded strings *)
  let double_decoded =
    try Uri.pct_decode url_decoded with
    | _ -> url_decoded
  in
  (* Normalize to lowercase and remove spaces for pattern matching *)
  double_decoded |> String.lowercase_ascii |> CCString.replace ~which:`All ~sub:" " ~by:""
;;

let contains_pattern str pattern =
  let normalized_str = normalize_string str in
  let normalized_pattern = normalize_string pattern in
  (* Check both the original string and normalized version *)
  CCString.mem ~sub:normalized_pattern normalized_str
  || CCString.mem ~sub:pattern (String.lowercase_ascii str)
;;

let check_string_against_patterns str =
  CCList.fold_right
    (fun (category, patterns) acc ->
       let matches = CCList.exists (contains_pattern str) patterns in
       if matches then category :: acc else acc)
    all_patterns
    []
;;

let extract_request_data req =
  let uri = req.Rock.Request.target |> Uri.of_string in
  let query_params =
    Uri.query uri
    |> CCList.fold_left
         (fun acc (key, values) ->
            CCList.fold_left (fun acc2 value -> (key ^ "=" ^ value) :: acc2) acc values)
         []
    |> CCString.concat "&"
  in
  let path = Uri.path uri in
  let headers =
    req.Rock.Request.headers
    |> Httpaf.Headers.to_list
    |> CCList.map (fun (name, value) -> name ^ ": " ^ value)
    |> String.concat "\n"
  in
  path, query_params, headers
;;

let check_request_security req =
  let path, query_params, headers = extract_request_data req in
  let all_data = CCString.concat " " [ path; query_params; headers ] in
  let suspicious_categories = check_string_against_patterns all_data in
  match suspicious_categories with
  | [] -> None
  | categories -> Some categories
;;

let log_suspicious_request req categories =
  let _src = Logs.Src.create "middleware.security" in
  let request_id = Sihl.Web.Id.find req |> CCOption.value ~default:"-" in
  let uri = req.Rock.Request.target in
  let method_str = req.Rock.Request.meth |> Httpaf.Method.to_string in
  let user_agent =
    Httpaf.Headers.get req.Rock.Request.headers "user-agent"
    |> CCOption.value ~default:"unknown"
  in
  let remote_addr =
    Httpaf.Headers.get req.Rock.Request.headers "x-forwarded-for"
    |> CCOption.or_ ~else_:(Httpaf.Headers.get req.Rock.Request.headers "x-real-ip")
    |> CCOption.value ~default:"unknown"
  in
  Logs.warn (fun m ->
    m
      "Suspicious request detected - ID: %s, Method: %s, URI: %s, Remote: %s, UA: %s, \
       Categories: %s"
      request_id
      method_str
      uri
      remote_addr
      user_agent
      (String.concat ", " categories))
;;

let security_response () =
  let body = "Request blocked for security reasons" in
  Sihl.Web.Response.of_plain_text body
  |> Sihl.Web.Response.set_status `Bad_request
  |> Lwt.return
;;

let security_filter handler req =
  match check_request_security req with
  | None -> handler req
  | Some categories ->
    log_suspicious_request req categories;
    security_response ()
;;

let middleware () =
  Rock.Middleware.create
    ~name:"security.malicious_request_detection"
    ~filter:security_filter
;;

(* Additional function to check POST body content *)
let check_body_security body =
  let%lwt body_string = Sihl.Web.Body.copy body |> Sihl.Web.Body.to_string in
  let suspicious_categories = check_string_against_patterns body_string in
  match suspicious_categories with
  | [] -> Lwt.return None
  | categories -> Lwt.return (Some categories)
;;

let enhanced_security_filter handler req =
  match check_request_security req with
  | Some categories ->
    log_suspicious_request req categories;
    security_response ()
  | None ->
    (* Check POST/PUT body if present *)
    (match req.Rock.Request.meth with
     | `POST | `PUT ->
       let%lwt body_check = check_body_security req.Rock.Request.body in
       (match body_check with
        | Some body_categories ->
          log_suspicious_request req body_categories;
          security_response ()
        | None -> handler req)
     | _ -> handler req)
;;

let enhanced_middleware () =
  Rock.Middleware.create
    ~name:"security.enhanced_malicious_request_detection"
    ~filter:enhanced_security_filter
;;
