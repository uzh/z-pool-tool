let tag_req : Sihl.Web.Request.t Logs.Tag.def =
  let pp (f : Format.formatter) (req : Sihl.Web.Request.t) =
    let id = Sihl.Web.Id.find req |> CCOption.value ~default:"none" in
    Format.fprintf f "%s" id
  in
  Logs.Tag.def "request" ~doc:"A Sihl request" pp
;;

let req (req : Sihl.Web.Request.t) : Logs.Tag.set =
  Logs.Tag.(empty |> add tag_req req)
;;

let empty : Logs.Tag.set = Logs.Tag.empty
let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green
let source_style = `Magenta

let pp_header ~pp_h ppf (l, h) =
  match l with
  | Logs.App ->
    (match h with
     | None -> ()
     | Some h -> Fmt.pf ppf "[%a] " Fmt.(styled app_style string) h)
  | Logs.Error ->
    pp_h
      ppf
      err_style
      (match h with
       | None -> "ERROR"
       | Some h -> h)
  | Logs.Warning ->
    pp_h
      ppf
      warn_style
      (match h with
       | None -> "WARNING"
       | Some h -> h)
  | Logs.Info ->
    pp_h
      ppf
      info_style
      (match h with
       | None -> "INFO"
       | Some h -> h)
  | Logs.Debug ->
    pp_h
      ppf
      debug_style
      (match h with
       | None -> "DEBUG"
       | Some h -> h)
;;

let pp_source = Fmt.(styled source_style string)
let pp_context = Fmt.(styled `Green string)
let pp_id = Fmt.(styled `Red string)

let pp_exec_header tags src =
  let open CCOption.Infix in
  let pp_h ppf style h =
    let id =
      tags
      >>= Logs.Tag.find tag_req
      >>= Sihl.Web.Id.find
      >|= Format.sprintf "%s"
      |> CCOption.value ~default:"-"
    in
    let context =
      tags
      >>= Logs.Tag.find tag_req
      >|= Pool_context.find
      >>= CCResult.to_opt
      >|= Pool_context.show_log
      |> CCOption.value ~default:"-"
    in
    let src = Logs.Src.name src in
    let now = Ptime_clock.now () |> Ptime.to_rfc3339 in
    Fmt.pf
      ppf
      "%s [%a] [%a] [%a] [%a]: "
      now
      Fmt.(styled style string)
      h
      pp_source
      src
      pp_id
      id
      pp_context
      context
  in
  pp_header ~pp_h
;;

let format_reporter
  ?(pp_header = pp_exec_header)
  ?(app = Format.std_formatter)
  ?(dst = Format.err_formatter)
  ()
  =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    msgf
    @@ fun ?header ?tags fmt ->
    let ppf = if level = Logs.App then app else dst in
    Format.kfprintf
      k
      ppf
      ("%a" ^^ fmt ^^ "@.")
      (pp_header tags src)
      (level, header)
  in
  { Logs.report }
;;

let get_log_level () =
  match Sihl.Configuration.read_string "LOG_LEVEL" with
  | Some "debug" -> Some Logs.Debug
  | Some "error" -> Some Logs.Error
  | Some "warning" -> Some Logs.Warning
  | _ -> Some Logs.Info
;;

let logs_dir () =
  match
    Sihl.Configuration.root_path (), Sihl.Configuration.read_string "LOGS_DIR"
  with
  | _, Some logs_dir -> logs_dir
  | Some root, None -> root ^ "/logs"
  | None, None -> "logs"
;;

let lwt_file_reporter ?(pp_header = pp_exec_header) () =
  let logs_dir = logs_dir () in
  let buf () =
    let b = Buffer.create 512 in
    ( b
    , fun () ->
        let m = Buffer.contents b in
        Buffer.reset b;
        m )
  in
  let app, app_flush = buf () in
  let err, err_flush = buf () in
  let report src level ~over k msgf =
    let k _ = k () in
    let write () =
      let name =
        match level with
        | Logs.Error -> logs_dir ^ "/error.log"
        | Logs.App | Logs.Debug | Logs.Info | Logs.Warning ->
          logs_dir ^ "/app.log"
      in
      let%lwt log =
        Lwt_io.open_file
          ~flags:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
          ~perm:0o777
          ~mode:Lwt_io.Output
          name
      in
      let%lwt () =
        match level with
        | Logs.Error -> Lwt_io.write log (err_flush ())
        | Logs.App | Logs.Debug | Logs.Info | Logs.Warning ->
          Lwt_io.write log (app_flush ())
      in
      Lwt_io.close log
    in
    let unblock () =
      over ();
      Lwt.return_unit
    in
    (write ()) [%lwt.finally unblock ()] |> Lwt.ignore_result;
    msgf
    @@ fun ?header ?tags fmt ->
    let ppf =
      match level with
      | Logs.Error -> Format.formatter_of_buffer err
      | Logs.App | Logs.Debug | Logs.Info | Logs.Warning ->
        Format.formatter_of_buffer app
    in
    Format.kfprintf
      k
      ppf
      ("%a" ^^ fmt ^^ "@.")
      (pp_header tags src)
      (level, header)
  in
  { Logs.report }
;;

let cli_reporter ?(pp_header = pp_exec_header) ?app ?dst () =
  Fmt_tty.setup_std_outputs ();
  format_reporter ~pp_header ?app ?dst ()
;;

let combine r1 r2 =
  let report src level ~over k msgf =
    let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
    r2.Logs.report src level ~over (fun () -> v) msgf
  in
  { Logs.report }
;;

let reporter =
  Logs.set_level (get_log_level ());
  let r1 = lwt_file_reporter () in
  let r2 = cli_reporter () in
  combine r1 r2
;;

let create_logs_dir () =
  if not (Sys.file_exists (logs_dir ())) then Sys.mkdir (logs_dir ()) 0x640
;;
