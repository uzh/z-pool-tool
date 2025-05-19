open Format
open Logs

let tag_req = Tag.def "request_id" ~doc:"Rock.Request/Response id" CCString.pp
let tag_ip = Tag.def "request_ip" ~doc:"X-Real-IP" CCString.pp
let tag_database = Database.Logger.Tags.add_label
let tag_user = Tag.def "user" ~doc:"User / Administrator email" CCString.pp
let app_channel = ref None
let error_channel = ref None

let get_log_level () =
  let default = Info in
  Sihl.Configuration.read_string "LOG_LEVEL"
  |> CCOption.map_or ~default (function
    | "debug" -> Debug
    | "error" -> Error
    | "warning" -> Warning
    | _ -> default)
  |> CCOption.return
;;

let logs_dir () =
  match Sihl.Configuration.root_path (), Sihl.Configuration.read_string "LOGS_DIR" with
  | _, Some logs_dir -> logs_dir
  | Some root, None -> root ^ "/logs"
  | None, None -> "logs"
;;

let init_log_file channel log_file =
  match !channel with
  | Some _ -> ()
  | None ->
    let out = open_out_gen [ Open_creat; Open_append; Open_text ] 0o666 log_file in
    channel := Some (out, formatter_of_out_channel out)
;;

let init_out_channels () =
  let logs_dir = logs_dir () in
  init_log_file app_channel (logs_dir ^ "/app.log");
  init_log_file error_channel (logs_dir ^ "/error.log")
;;

let close_log_files () =
  let close_channel = function
    | Some (out, _) ->
      close_out out;
      None
    | None -> None
  in
  app_channel := close_channel !app_channel;
  error_channel := close_channel !error_channel
;;

let () = at_exit close_log_files

(* Adapted from Logs_fmt.pp_header *)
let pp_header ~pp_h ppf (l, h) =
  let open Logs_fmt in
  match l with
  | App -> CCOption.get_or ~default:"APP" h |> pp_h ppf app_style
  | Error -> CCOption.get_or ~default:"ERROR" h |> pp_h ppf err_style
  | Warning -> CCOption.get_or ~default:"WARNING" h |> pp_h ppf warn_style
  | Info -> CCOption.get_or ~default:"INFO" h |> pp_h ppf info_style
  | Debug -> CCOption.get_or ~default:"DEBUG" h |> pp_h ppf debug_style
;;

let pp_exec_header ?(tags = Tag.empty) src =
  let find tag = Tag.find tag tags |> CCOption.get_or ~default:"-" in
  let pp_h ppf style level =
    let now =
      let now = Ptime_clock.now () in
      Ptime.to_rfc3339 ~tz_offset_s:(Utils.Ptime.to_zurich_tz_offset_s now) now
    in
    let pp_styled_values ppf values =
      CCList.iter
        (fun (color, value) -> fprintf ppf "[%a]" Fmt.(styled color string) value)
        values
    in
    let styles =
      [ style, level
      ; `Magenta, Src.name src
      ; `Green, find tag_database
      ; `Red, find tag_ip
      ; `Red, find tag_req
      ; `Cyan, find tag_user
      ]
    in
    fprintf ppf "%s %a: " now pp_styled_values styles
  in
  pp_header ~pp_h
;;

let format_reporter
      ?(pp_header = pp_exec_header)
      ?(app = std_formatter)
      ?(dst = err_formatter)
      ()
  =
  let report src level ~over k msgf =
    let k' _ =
      over ();
      k ()
    in
    msgf
    @@ fun ?header ?tags fmt ->
    let ppf =
      match level with
      | Logs.Error -> dst
      | App | Debug | Info | Warning -> app
    in
    kfprintf k' ppf ("%a" ^^ fmt ^^ "@.") (pp_header ?tags src) (level, header)
  in
  { report }
;;

let rec lwt_file_reporter ?pp_header () =
  let () = init_out_channels () in
  match !app_channel, !error_channel with
  | Some (_, app_fmt), Some (_, error_fmt) ->
    format_reporter ?pp_header ~app:app_fmt ~dst:error_fmt ()
  | (None | Some _), (None | Some _) ->
    init_out_channels ();
    lwt_file_reporter ?pp_header ()
;;

let cli_reporter ?pp_header ?app ?dst () =
  Fmt_tty.setup_std_outputs ();
  format_reporter ?pp_header ?app ?dst ()
;;

let combine r1 r2 =
  let report src level ~over k msgf =
    let v = r1.report src level ~over:(fun () -> ()) k msgf in
    r2.report src level ~over (fun () -> v) msgf
  in
  { report }
;;

let reporter =
  set_level (get_log_level ());
  if Sihl.Configuration.is_production ()
  then lwt_file_reporter ()
  else combine (cli_reporter ()) (lwt_file_reporter ())
;;

let create_logs_dir () =
  if not (Sys.file_exists (logs_dir ())) then Sys.mkdir (logs_dir ()) 0o755
;;

let log_exception ?prefix ~src ~tags =
  let backtrace = Printexc.get_backtrace () in
  let prefix = CCOption.map_or ~default:"" (asprintf "%s: ") prefix in
  let print ?(error_type = "Exception") error_name =
    err ~src (fun m ->
      m ~tags "%s%s caught: %s, Backtrace: %s" prefix error_type error_name backtrace)
  in
  function
  | Caqti_error.(Exn #load_or_connect as exn) ->
    print ~error_type:"Caqti error" (Printexc.to_string exn)
  | Pool_message.Error.(Exn exn) -> print (Pool_message.Error.show exn)
  | exn -> print (Printexc.to_string exn)
;;
