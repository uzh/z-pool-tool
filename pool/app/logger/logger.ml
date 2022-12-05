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

let cli_reporter ?(pp_header = pp_exec_header) ?app ?dst () =
  Fmt_tty.setup_std_outputs ();
  format_reporter ~pp_header ?app ?dst ()
;;

let reporter = cli_reporter ()
