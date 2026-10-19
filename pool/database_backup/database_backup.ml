open CCFun

let src = Logs.Src.create "database.backup"

(* Labels and versions end up in file names, everything else is replaced *)
let sanitize =
  CCString.map (function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_') as c -> c
    | _ -> '_')
;;

let directory () =
  match
    Sihl.Configuration.read_string "DATABASE_BACKUP_DIR", Sihl.Configuration.root_path ()
  with
  | Some directory, _ -> directory
  | None, Some root -> Filename.concat root "backups"
  | None, None -> "backups"
;;

module Run = struct
  let incomplete_suffix = "_incomplete"

  let timestamp time =
    let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s:0 time in
    Format.asprintf "%04d%02d%02dT%02d%02d%02d" y m d hh mm ss
  ;;

  let name ~version time = Format.asprintf "%s_%s" (timestamp time) (sanitize version)

  let version name =
    let open CCOption.Infix in
    let* stamp, version = CCString.Split.left ~by:"_" name in
    let* time =
      Scanf.sscanf_opt stamp "%4u%2u%2uT%2u%2u%2u%!" (fun y m d hh mm ss ->
        Ptime.of_date_time ((y, m, d), ((hh, mm, ss), 0)))
      |> CCOption.flatten
    in
    (* Rejects e.g. single digit fields Scanf would accept *)
    if CCString.equal (timestamp time) stamp then Some version else None
  ;;

  let is_run = version %> CCOption.is_some

  let all directory =
    if Sys.file_exists directory && Sys.is_directory directory
    then
      Sys.readdir directory
      |> CCArray.to_list
      |> CCList.filter (fun name ->
        is_run name && Sys.is_directory (Filename.concat directory name))
      |> CCList.sort CCString.compare
    else []
  ;;
end

let rec mkdir_p path =
  if not (Sys.file_exists path)
  then (
    mkdir_p (Filename.dirname path);
    Sys.mkdir path 0o700)
;;

let remove_if_exists path = if Sys.file_exists path then Sys.remove path

module Dump = struct
  open Caqti_request.Infix
  open Lwt_result.Syntax

  type column =
    { name : string
    ; data_type : string
    ; primary_key : bool
    }

  (* An INSERT is finished before it would exceed max_allowed_packet, a
     larger row gets a statement of its own. The default is the default
     --net-buffer-length of mariadb-dump (and thus TablePlus). *)
  let default_max_allowed_packet = (1024 * 1024) - 1025

  (* The driver buffers complete results, so instead of streaming rows like
     mariadb-dump --quick, tables are read in pages of about this size *)
  let page_bytes = 16 * 1024 * 1024
  let first_page_size = 100
  let max_page_size = 10_000
  let identifier name = Format.asprintf "`%s`" (CCString.replace ~sub:"`" ~by:"``" name)

  let binary_types =
    [ "binary"
    ; "varbinary"
    ; "tinyblob"
    ; "blob"
    ; "mediumblob"
    ; "longblob"
    ; "geometry"
    ; "point"
    ; "linestring"
    ; "polygon"
    ; "multipoint"
    ; "multilinestring"
    ; "multipolygon"
    ; "geometrycollection"
    ]
  ;;

  (* The server renders every value as an SQL literal: binary data as hex,
     everything else with QUOTE (which yields NULL for NULL). Newlines are
     escaped, so each INSERT stays on a single line. *)
  let literal { name; data_type; _ } =
    let column = identifier name in
    match CCString.lowercase_ascii data_type with
    | "bit" -> [%string "IFNULL(CAST(%{column} + 0 AS CHAR), 'NULL')"]
    | data_type when CCList.mem ~eq:CCString.equal data_type binary_types ->
      [%string
        {sql|IF(%{column} IS NULL, 'NULL', CONCAT('X''', HEX(%{column}), ''''))|sql}]
    | _ ->
      [%string {sql|REPLACE(REPLACE(QUOTE(%{column}), '\n', '\\n'), '\r', '\\r')|sql}]
  ;;

  let session_requests =
    [ "SET NAMES utf8mb4"; "SET time_zone = '+00:00'" ]
    |> CCList.map Caqti_type.(unit ->. unit)
  ;;

  let database_request = {sql| SELECT DATABASE() |sql} |> Caqti_type.(unit ->! string)

  let tables_request =
    {sql|
      SELECT TABLE_NAME
      FROM information_schema.TABLES
      WHERE TABLE_SCHEMA = DATABASE() AND TABLE_TYPE = 'BASE TABLE'
      ORDER BY TABLE_NAME
    |sql}
    |> Caqti_type.(unit ->* string)
  ;;

  (* Generated columns are recomputed on restore and can't be inserted *)
  let columns_request =
    {sql|
      SELECT COLUMN_NAME, DATA_TYPE, COLUMN_KEY
      FROM information_schema.COLUMNS
      WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = ? AND IS_GENERATED = 'NEVER'
      ORDER BY ORDINAL_POSITION
    |sql}
    |> Caqti_type.(string ->* t3 string string string)
  ;;

  let create_table_request table =
    [%string "SHOW CREATE TABLE %{identifier table}"]
    |> Caqti_type.(unit ->! t2 string string) ~oneshot:true
  ;;

  let rows_sql table columns =
    let values = columns |> CCList.map literal |> CCString.concat ", " in
    [%string
      "SELECT CONCAT('(', CONCAT_WS(',', %{values}), ')') FROM %{identifier table}"]
  ;;

  let header ~label ~database =
    [%string
      {sql|-- Database dump of '%{label}' (database %{database})
SET NAMES utf8mb4;
SET time_zone = '+00:00';
SET FOREIGN_KEY_CHECKS = 0;
SET UNIQUE_CHECKS = 0;
SET SQL_MODE = 'NO_AUTO_VALUE_ON_ZERO';
|sql}]
  ;;

  let footer = "\nSET FOREIGN_KEY_CHECKS = 1;\nSET UNIQUE_CHECKS = 1;\n"

  let iter_ok fcn =
    Lwt_list.fold_left_s
      (fun result item ->
         match result with
         | Ok () -> fcn item
         | Error _ -> Lwt.return result)
      (Ok ())
  ;;

  let table ~max_allowed_packet (module Connection : Caqti_lwt.CONNECTION) out table =
    let* (_ : string), create = Connection.find (create_table_request table) () in
    let* columns = Connection.collect_list columns_request table in
    let columns =
      CCList.map
        (fun (name, data_type, key) ->
           { name; data_type; primary_key = CCString.equal key "PRI" })
        columns
    in
    output_string
      out
      [%string "\nDROP TABLE IF EXISTS %{identifier table};\n%{create};\n"];
    let insert =
      let names =
        columns |> CCList.map (fun { name; _ } -> identifier name) |> CCString.concat ", "
      in
      [%string "INSERT INTO %{identifier table} (%{names}) VALUES "]
    in
    let length = ref 0 in
    let oversized = ref 0 in
    let write row =
      let row_length = CCString.length row in
      if CCString.length insert + row_length > max_allowed_packet then incr oversized;
      if !length > 0 && !length + 1 + row_length > max_allowed_packet
      then (
        output_string out ";\n";
        length := 0);
      if !length = 0
      then (
        output_string out insert;
        length := CCString.length insert + row_length)
      else (
        output_char out ',';
        length := !length + 1 + row_length);
      output_string out row
    in
    let finish () =
      if !length > 0 then output_string out ";\n";
      if !oversized > 0
      then
        Logs.warn ~src (fun m ->
          m
            "%i row(s) of %s exceed max_allowed_packet (%i bytes): restoring them \
             requires a larger max_allowed_packet"
            !oversized
            table
            max_allowed_packet)
    in
    match columns, CCList.filter (fun { primary_key; _ } -> primary_key) columns with
    | [], _ -> Lwt.return_ok ()
    | _, [] ->
      let request =
        rows_sql table columns |> Caqti_type.(unit ->* string) ~oneshot:true
      in
      let* rows = Connection.collect_list request () in
      CCList.iter write rows;
      finish () |> Lwt.return_ok
    | _, keys ->
      (* Pages ordered by the primary key keep the memory usage bounded *)
      let order =
        keys |> CCList.map (fun { name; _ } -> identifier name) |> CCString.concat ", "
      in
      let request =
        [%string "%{rows_sql table columns} ORDER BY %{order} LIMIT ? OFFSET ?"]
        |> Caqti_type.(t2 int int ->* string) ~oneshot:true
      in
      let rec page ~size offset =
        let* rows = Connection.collect_list request (size, offset) in
        CCList.iter write rows;
        if CCList.length rows < size
        then finish () |> Lwt.return_ok
        else (
          (* Sized by the rows just read, so wide rows (e.g. files) come in
             small pages and narrow ones in large *)
          let bytes =
            CCList.fold_left (fun sum row -> sum + CCString.length row) 1 rows
          in
          let size =
            size * page_bytes / bytes |> CCInt.max 1 |> CCInt.min max_page_size
          in
          page ~size (offset + CCList.length rows))
      in
      page ~size:first_page_size 0
  ;;

  (* All reads happen in one REPEATABLE READ transaction, so the dump is a
     consistent snapshot even while the application keeps writing *)
  let database
        ~max_allowed_packet
        ~label
        out
        ((module Connection : Caqti_lwt.CONNECTION) as connection)
    =
    let* () = iter_ok (flip Connection.exec ()) session_requests in
    let* () = Connection.start () in
    let dump () =
      let* database = Connection.find database_request () in
      output_string out (header ~label ~database);
      let* tables = Connection.collect_list tables_request () in
      let* () = iter_ok (table ~max_allowed_packet connection out) tables in
      output_string out footer |> Lwt.return_ok
    in
    let%lwt result = dump () in
    let%lwt (_ : (unit, Caqti_error.t) result) = Connection.rollback () in
    Lwt.return result
  ;;
end

let parse_size size =
  let size = size |> CCString.trim |> CCString.lowercase_ascii in
  let number, factor =
    [ "k", 1024; "m", 1024 * 1024; "g", 1024 * 1024 * 1024 ]
    |> CCList.find_map (fun (suf, factor) ->
      CCString.chop_suffix ~suf size |> CCOption.map (fun number -> number, factor))
    |> CCOption.value ~default:(size, 1)
  in
  CCInt.of_string number
  |> CCOption.filter (fun number -> number > 0)
  |> CCOption.map (( * ) factor)
;;

let open_private path = open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o600 path

(* Streams [source] through a gzip encoder into [target], so the memory usage
   doesn't depend on the dump's size *)
let gzip ~time ~source target =
  let mtime = Ptime.to_float_s time |> Int64.of_float |> Int64.to_int32 in
  CCIO.with_in source (fun input ->
    let output = open_private target in
    Fun.protect
      ~finally:(fun () -> close_out output)
      (fun () ->
         let encoder =
           Gz.Def.encoder
             (`Channel input)
             (`Channel output)
             ~filename:
               (Filename.basename source
                |> CCString.chop_suffix ~suf:".partial"
                |> CCOption.get_or ~default:"dump.sql")
             ~mtime
             Gz.Unix
             ~q:(De.Queue.create 0x4000)
             ~w:(De.Lz77.make_window ~bits:15)
             ~level:6
         in
         let rec encode encoder =
           match Gz.Def.encode encoder with
           | `End (_ : Gz.Def.encoder) -> ()
           (* only returned for manual sources and destinations *)
           | `Await encoder | `Flush encoder -> encode encoder
         in
         encode encoder))
;;

let dump
      ?(max_allowed_packet = Dump.default_max_allowed_packet)
      ?(time = Ptime_clock.now ())
      ~directory
      database
  =
  let label = Database.label database |> Database.Label.value in
  let sql_file =
    Filename.concat
      directory
      (Format.asprintf "%s_%s.sql" (sanitize label) (Run.timestamp time))
  in
  let result_file = Format.asprintf "%s.gz" sql_file in
  let partial_sql = Format.asprintf "%s.partial" sql_file in
  let partial_gzip = Format.asprintf "%s.partial" result_file in
  let cleanup () = CCList.iter remove_if_exists [ partial_sql; partial_gzip ] in
  Logs.info ~src (fun m -> m "Dumping database '%s' to %s" label result_file);
  let run () =
    let out = open_private partial_sql in
    let%lwt result =
      Lwt.finalize
        (fun () ->
           Database.with_connection
             database
             (Dump.database ~max_allowed_packet ~label out))
        (fun () -> close_out out |> Lwt.return)
    in
    match result with
    | Ok () ->
      gzip ~time ~source:partial_sql partial_gzip;
      Sys.rename partial_gzip result_file;
      cleanup ();
      Lwt.return_ok result_file
    | Error err ->
      cleanup ();
      Lwt.return_error (Caqti_error.show err)
  in
  Lwt.catch run (fun exn ->
    cleanup ();
    Lwt.return_error (Printexc.to_string exn))
;;

type outcome =
  { directory : string
  ; failed : (string * string) list
  }

let create ?max_allowed_packet ?(directory = directory ()) ~version databases =
  let time = Ptime_clock.now () in
  let run_directory = Filename.concat directory (Run.name ~version time) in
  mkdir_p directory;
  (* Fails if a run was started within the same second *)
  Sys.mkdir run_directory 0o700;
  let%lwt failed =
    Lwt_list.filter_map_s
      (fun database ->
         let label = Database.label database |> Database.Label.value in
         match%lwt dump ?max_allowed_packet ~time ~directory:run_directory database with
         | Ok (_ : string) -> Lwt.return_none
         | Error err ->
           Logs.err ~src (fun m -> m "Dump of database '%s' failed: %s" label err);
           Lwt.return_some (label, err))
      databases
  in
  match failed with
  | [] -> Lwt.return { directory = run_directory; failed }
  | _ ->
    (* An incomplete run must not count as the backup of this version *)
    let incomplete = Format.asprintf "%s%s" run_directory Run.incomplete_suffix in
    Sys.rename run_directory incomplete;
    Lwt.return { directory = incomplete; failed }
;;

let is_dump_file name =
  [ ".sql"; ".sql.gz"; ".partial" ]
  |> CCList.exists (fun suf -> CCString.suffix ~suf name)
;;

let remove_run directory =
  Sys.readdir directory
  |> CCArray.iter (fun name ->
    if is_dump_file name then Sys.remove (Filename.concat directory name));
  match Sys.readdir directory with
  | [||] ->
    Sys.rmdir directory;
    true
  | _ ->
    Logs.warn ~src (fun m ->
      m "Kept %s: it contains files that were not created by a backup" directory);
    false
;;

let remove_all ?(directory = directory ()) () =
  Run.all directory
  |> CCList.map (Filename.concat directory)
  |> CCList.filter remove_run
  |> CCList.length
;;
