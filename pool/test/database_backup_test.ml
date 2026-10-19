open Database_backup
open CCFun

let get_exn = Test_utils.get_or_failwith

let temp_directory () =
  let path = Filename.temp_file "pool-backup-test-" "" in
  Sys.remove path;
  Sys.mkdir path 0o700;
  path
;;

let touch path = CCIO.with_out path (flip output_string "")
let readdir = Sys.readdir %> CCArray.to_list %> CCList.sort CCString.compare

let parse_sizes () =
  CCList.iter
    (fun (input, expected) ->
       Alcotest.(check (option int) input expected (parse_size input)))
    [ "1048576", Some 1048576
    ; "64K", Some (64 * 1024)
    ; "16m", Some (16 * 1024 * 1024)
    ; " 1G ", Some (1024 * 1024 * 1024)
    ; "0", None
    ; "-1M", None
    ; "16MB", None
    ; "M", None
    ; "", None
    ]
;;

let run_names () =
  let time =
    Ptime.of_date_time ((2026, 10, 19), ((9, 5, 7), 0)) |> CCOption.get_exn_or "time"
  in
  let name = Run.name ~version:"0.13.11-abc/def" time in
  Alcotest.(check string "name" "20261019T090507_0.13.11-abc_def" name);
  Alcotest.(check bool "is run" true (Run.is_run name));
  Alcotest.(check (option string) "version" (Some "0.13.11-abc_def") (Run.version name));
  Alcotest.(
    check
      (option string)
      "incomplete version"
      (Some "0.13.11_incomplete")
      (Run.version "20261019T090507_0.13.11_incomplete"));
  CCList.iter
    (fun name -> Alcotest.(check bool name false (Run.is_run name)))
    [ "logs"
    ; "20261019T090507"
    ; "20261019X090507_0.13.11"
    ; "20261019T090507Z_0.13.11"
    ; "20261399T090507_0.13.11"
    ; "2026101T9090507_0.13.11"
    ]
;;

let remove_all_only_removes_dumps () =
  let directory = temp_directory () in
  let path = Filename.concat directory in
  let run = "20261019T090507_0.13.10" in
  let foreign = "20261019T100000_0.13.11" in
  CCList.iter (fun name -> Sys.mkdir (path name) 0o700) [ run; foreign; "other" ];
  touch (path (Format.asprintf "%s/root.sql" run));
  touch (path (Format.asprintf "%s/econ-test.sql.partial" run));
  touch (path (Format.asprintf "%s/root.sql" foreign));
  touch (path (Format.asprintf "%s/notes.txt" foreign));
  touch (path "other/root.sql");
  Alcotest.(check int "removed runs" 1 (remove_all ~directory ()));
  Alcotest.(check (list string) "remaining" [ foreign; "other" ] (readdir directory));
  Alcotest.(
    check (list string) "foreign file kept" [ "notes.txt" ] (readdir (path foreign)));
  Alcotest.(
    check (list string) "unrelated dir untouched" [ "root.sql" ] (readdir (path "other")))
;;

(* Integration: scratch databases next to the test tenant *)

let scratch_database name =
  let url =
    Sihl.Configuration.read_string "DATABASE_URL_TENANT_ONE"
    |> CCOption.get_exn_or "DATABASE_URL_TENANT_ONE undefined"
    |> Uri.of_string
    |> flip Uri.with_path (Format.asprintf "/%s" name)
    |> Uri.to_string
  in
  Database.create
    (Database.Label.create name |> get_exn)
    (Database.Url.create url |> get_exn)
;;

let test_tenant () =
  Database.Label.create "econ-test" |> get_exn |> Database.Pool.Tenant.find |> get_exn
;;

(* Statements are sent verbatim like the mariadb client does: Caqti's query
   parser doesn't know the backslash escapes of MariaDB string literals *)
let exec_all database statements =
  let open Caqti_request.Infix in
  let request sql =
    Caqti_type.(unit -->. unit) ~oneshot:true (fun _ -> Caqti_query.L sql)
  in
  Database.with_connection database (fun (module Connection : Caqti_lwt.CONNECTION) ->
    Lwt_list.fold_left_s
      (fun result sql ->
         match result with
         | Ok () -> Connection.exec (request sql) ()
         | Error _ -> Lwt.return result)
      (Ok ())
      statements)
  |> Lwt.map (CCResult.map_err Caqti_error.show %> CCResult.get_or_failwith)
;;

let rows database =
  let open Caqti_request.Infix in
  let request =
    {sql|
      SELECT CONCAT_WS('|', id, IFNULL(HEX(note), 'N'), IFNULL(HEX(data), 'N'),
        IFNULL(created_at, 'N'), HEX(combined))
      FROM backup_test
      ORDER BY id
    |sql}
    |> Caqti_type.(unit ->* string) ~oneshot:true
  in
  Database.with_connection database (fun (module Connection : Caqti_lwt.CONNECTION) ->
    Connection.collect_list request ())
  |> Lwt.map (CCResult.map_err Caqti_error.show %> CCResult.get_or_failwith)
;;

let source_statements =
  [ {sql|
      CREATE TABLE backup_test (
        id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
        note TEXT NULL,
        data MEDIUMBLOB NULL,
        created_at TIMESTAMP(3) NULL,
        combined VARCHAR(255) GENERATED ALWAYS AS (UPPER(IFNULL(note, '-'))) STORED
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4
    |sql}
  ; {sql|
      INSERT INTO backup_test (note, data, created_at) VALUES
        ('it''s a "quote" \\ back\\slash', X'00FF0A0D27', '2026-10-19 10:05:07.123'),
        ('line\nbreak;\nand\r\ttab', X'', NULL),
        (NULL, NULL, '1970-01-01 00:00:01'),
        ('Zürich 🧪 日本', X'5C27', '2038-01-19 03:14:07')
    |sql}
    (* more rows than fit into one page *)
  ; {sql|
      INSERT INTO backup_test (note, data)
      WITH RECURSIVE digits (n) AS (SELECT 0 UNION ALL SELECT n + 1 FROM digits WHERE n < 49)
      SELECT CONCAT('row ', a.n * 50 + b.n), UNHEX(MD5(a.n * 50 + b.n))
      FROM digits a CROSS JOIN digits b
    |sql}
    (* two rows that don't fit into one INSERT, one that exceeds it alone *)
  ; {sql|
      INSERT INTO backup_test (note, data) VALUES
        ('wide 1', REPEAT(X'AB', 300000)),
        ('wide 2', REPEAT(X'CD', 300000)),
        ('wider than an INSERT', REPEAT(X'EF', 1000000))
    |sql}
  ]
;;

(* Splits a dump into statements the way the mariadb client would for it:
   values never contain raw newlines, so ";\n" only ends statements *)
let statements_of_dump file =
  CCIO.with_in file CCIO.read_lines_l
  |> CCList.filter (CCString.prefix ~pre:"--" %> not)
  |> CCString.concat "\n"
  |> CCString.split ~by:";\n"
  |> CCList.map CCString.trim
  |> CCList.filter (CCString.is_empty %> not)
;;

let dump_restores_identical_data _ () =
  let max_allowed_packet = 256 * 1024 in
  let names = [ "pool_backup_source"; "pool_backup_target" ] in
  let source = scratch_database "pool_backup_source" in
  let target = scratch_database "pool_backup_target" in
  let drop = CCList.map (Format.asprintf "DROP DATABASE IF EXISTS %s") names in
  let%lwt () =
    drop @ CCList.map (Format.asprintf "CREATE DATABASE %s CHARACTER SET utf8mb4") names
    |> exec_all (test_tenant ())
  in
  Lwt.finalize
    (fun () ->
       let%lwt () = exec_all source source_statements in
       let%lwt file =
         dump ~max_allowed_packet ~directory:(temp_directory ()) source
         |> Lwt.map CCResult.get_or_failwith
       in
       Alcotest.(check int "dump is private" 0o600 (Unix.stat file).Unix.st_perm);
       let statements = statements_of_dump file in
       statements
       |> CCList.filter (CCString.prefix ~pre:"INSERT")
       |> CCList.iter (fun statement ->
         Alcotest.(
           check
             bool
             "INSERT within max_allowed_packet or a single row"
             true
             (CCString.length statement <= max_allowed_packet
              || not (CCString.mem ~sub:"),(" statement))));
       let%lwt () = exec_all target statements in
       let%lwt expected = rows source in
       let%lwt restored = rows target in
       Alcotest.(check int "all rows" 2507 (CCList.length restored));
       Alcotest.(check (list string) "restored rows" expected restored);
       Lwt.return_unit)
    (fun () -> exec_all (test_tenant ()) drop)
;;

let create_dumps_all_registered_databases _ () =
  let directory = temp_directory () in
  let%lwt databases = Database.Pool.registered () in
  let%lwt { directory = run_directory; failed } =
    create ~directory ~version:"1.2.3" databases
  in
  Alcotest.(check (list (pair string string)) "no failures" [] failed);
  Alcotest.(
    check
      (option string)
      "run version"
      (Some "1.2.3")
      (Run.version (Filename.basename run_directory)));
  Alcotest.(
    check
      (list string)
      "one dump per database"
      [ "econ-test.sql"; "root.sql" ]
      (readdir run_directory));
  let root_dump = CCIO.with_in (Filename.concat run_directory "root.sql") CCIO.read_all in
  Alcotest.(
    check
      bool
      "root tenants"
      true
      (CCString.mem ~sub:"INSERT INTO `pool_tenant_databases`" root_dump));
  Lwt.return_unit
;;

let create_marks_failed_run_incomplete _ () =
  let directory = temp_directory () in
  let%lwt { directory = run_directory; failed } =
    create ~directory ~version:"1.2.3" [ scratch_database "pool_backup_missing" ]
  in
  Alcotest.(
    check
      (list string)
      "failed databases"
      [ "pool_backup_missing" ]
      (CCList.map fst failed));
  Alcotest.(
    check bool "incomplete" true (CCString.suffix ~suf:"_incomplete" run_directory));
  Alcotest.(check (list string) "no partial dumps left" [] (readdir run_directory));
  Lwt.return_unit
;;
