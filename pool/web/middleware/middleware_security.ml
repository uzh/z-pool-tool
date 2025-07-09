open CCFun.Infix

let src = Logs.Src.create "middleware.security"

let create_tags req =
  let tags = Pool_context.Logger.Tags.req req in
  Middleware_tenant.tenant_url_of_request req
  |> CCResult.map_or ~default:tags (fun url ->
    Logs.Tag.add Database.Logger.Tags.add_label (Pool_tenant.Url.value url) tags)
;;

let combine_uniq l1 l2 = CCList.append l1 l2 |> CCList.uniq ~eq:CCString.equal

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

let asset_regex =
  let open Re in
  let path_segment = seq [ char '/'; rep1 (alt [ wordc; char '-' ]) ] in
  let uuid_hex n = repn (alt [ digit; rg 'a' 'f'; rg 'A' 'F' ]) n (Some n) in
  let uuid_pattern =
    seq
      [ uuid_hex 8
      ; char '-'
      ; uuid_hex 4
      ; char '-'
      ; uuid_hex 4
      ; char '-'
      ; uuid_hex 4
      ; char '-'
      ; uuid_hex 12
      ]
  in
  let build elements = seq [ bos; rep path_segment; elements; eos ] in
  let filename =
    seq [ rep1 (alt [ wordc; char '-' ]); rep1 (seq [ char '.'; rep1 alnum ]) ]
  in
  let hash_filename =
    seq [ rep1 wordc; char '.'; rep1 (alt [ alnum; char '-' ]); char '.'; rep1 alnum ]
  in
  let static_assets_pattern =
    seq [ str "/assets/"; alt [ filename; hash_filename ] ] |> build
  in
  let custom_assets_pattern =
    seq [ str "/custom/assets/"; opt uuid_pattern; char '/'; filename ] |> build
  in
  let wellknown_pattern =
    seq [ str "/.well-known"; path_segment; char '/'; filename ] |> build
  in
  let pattern =
    alt
    @@ [ custom_assets_pattern; static_assets_pattern; build filename ]
    @ if Sihl.Configuration.is_production () then [] else [ wellknown_pattern ]
  in
  compile pattern
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
  |> function
  | [] -> None
  | categories -> Some categories
;;

let find_query_params uri =
  Uri.query uri
  |> CCList.fold_left
       (fun acc (key, values) ->
          CCList.fold_left (fun acc2 value -> (key ^ "=" ^ value) :: acc2) acc values)
       []
  |> CCString.concat "&"
;;

let extract_request_data req =
  let uri = req.Rock.Request.target |> Uri.of_string in
  let headers =
    req.Rock.Request.headers
    |> Httpaf.Headers.to_list
    |> CCList.map (fun (name, value) -> name ^ ": " ^ value)
    |> String.concat "\n"
  in
  Uri.path uri, find_query_params uri, headers
;;

let log_suspicious_request req categories =
  let user_agent =
    Httpaf.Headers.get req.Rock.Request.headers "user-agent"
    |> CCOption.value ~default:"unknown"
  in
  let remote_addr =
    Httpaf.Headers.get req.Rock.Request.headers "x-forwarded-for"
    |> CCOption.or_ ~else_:(Httpaf.Headers.get req.Rock.Request.headers "X-REAL-IP")
    |> CCOption.or_ ~else_:(Httpaf.Headers.get req.Rock.Request.headers "x-real-ip")
    |> CCOption.value ~default:"unknown"
  in
  Logs.warn ~src (fun m ->
    m
      ~tags:(create_tags req)
      "Suspicious request detected - Remote: %s, Method: %s, URI: %s, UA: %s, \
       Categories: %s"
      remote_addr
      (req.Rock.Request.meth |> Httpaf.Method.to_string)
      req.Rock.Request.target
      user_agent
      (String.concat ", " categories))
;;

let continue handler req = function
  | Some categories ->
    log_suspicious_request req categories;
    handler req
  | None -> handler req
;;

let check_request_security req =
  let path, query_params, headers = extract_request_data req in
  CCString.concat " " [ path; query_params; headers ] |> check_string_against_patterns
;;

let check_body_security { Rock.Request.meth; body; _ } =
  let check =
    Sihl.Web.Body.(copy %> to_string) %> Lwt.map check_string_against_patterns
  in
  match meth with
  | `POST | `PUT -> check body
  | _ -> Lwt.return None
;;

let security_filter' handler req checks =
  let path, query_params, _ = extract_request_data req in
  match Re.execp asset_regex path with
  | true when String.equal query_params "" -> handler req
  | true -> continue handler req (check_string_against_patterns query_params)
  | false ->
    CCList.fold_left
      (fun acc cat ->
         CCOption.(map2 combine_uniq acc cat |> or_ ~else_:acc |> or_ ~else_:cat))
      None
      checks
    |> continue handler req
;;

let security_filter handler req =
  [ check_request_security req ] |> security_filter' handler req
;;

let middleware () =
  Rock.Middleware.create
    ~name:"security.malicious_request_detection"
    ~filter:security_filter
;;

let enhanced_security_filter handler req =
  let%lwt body_security = check_body_security req in
  [ check_request_security req; body_security ] |> security_filter' handler req
;;

let enhanced_middleware () =
  Rock.Middleware.create
    ~name:"security.enhanced_malicious_request_detection"
    ~filter:enhanced_security_filter
;;
