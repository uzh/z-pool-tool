(* Based on
   https://github.com/rgrinberg/opium/blob/master/opium/src/middlewares/middleware_logger.ml *)
include Sexplib0

let src = Logs.Src.create "middleware.logger"

module Exn = struct
  type t = exn

  let sexp_of_t = Sexp_conv.sexp_of_exn
  let pp fmt t = Sexp.pp fmt t
  let to_string t = Printexc.to_string t
end

module String = struct
  include StringLabels

  let rec check_prefix s ~prefix len i =
    i = len || (s.[i] = prefix.[i] && check_prefix s ~prefix len (i + 1))
  ;;

  let is_prefix s ~prefix =
    let len = length s in
    let prefix_len = length prefix in
    len >= prefix_len && check_prefix s ~prefix prefix_len 0
  ;;
end

let body_to_string ?(content_type = "text/plain") ?(max_len = 1000) body =
  let lhs, rhs =
    match CCString.split_on_char '/' content_type with
    | [ lhs; rhs ] -> lhs, rhs
    | _ -> "application", "octet-stream"
  in
  match lhs, rhs with
  | "text", _ | "application", "json" | "application", "x-www-form-urlencoded"
    ->
    let%lwt s = Opium.Body.copy body |> Opium.Body.to_string in
    if CCString.length s > max_len
    then
      Lwt.return
      @@ CCString.sub s 0 (min (CCString.length s) max_len)
      ^ CCFormat.asprintf
          " [truncated %d characters]"
          (String.length s - max_len)
    else Lwt.return s
  | _ -> Lwt.return ("<" ^ content_type ^ ">")
;;

let request_to_string (request : Opium.Request.t) =
  let open Opium in
  let open Request in
  let content_type = Request.content_type request in
  let%lwt body_string = body_to_string ?content_type request.body in
  Lwt.return
  @@ Format.asprintf
       "%s %s %s\n%s\n\n%s\n%!"
       (Method.to_string request.meth)
       request.target
       (Version.to_string request.version)
       (Headers.to_string request.headers)
       body_string
;;

let response_to_string (response : Opium.Response.t) =
  let open Opium in
  let open Response in
  let content_type = Response.content_type response in
  let%lwt body_string = body_to_string ?content_type response.body in
  Lwt.return
  @@ Format.asprintf
       "%a %a %s\n%a\n%s\n%!"
       Version.pp_hum
       response.version
       Status.pp_hum
       response.status
       (Option.value ~default:"" response.reason)
       Headers.pp_hum
       response.headers
       body_string
;;

let respond handler req =
  let open Opium in
  let tags = Pool_context.Logger.Tags.req req in
  let time_f f =
    let t1 = Mtime_clock.now () in
    let x = f () in
    let t2 = Mtime_clock.now () in
    let span = Mtime.span t1 t2 in
    span, x
  in
  let f () = handler req in
  let span, response_lwt = time_f f in
  let%lwt response = response_lwt in
  let code = response.Response.status |> Status.to_string in
  Logs.info ~src (fun m -> m ~tags "Responded %s in %a" code Mtime.Span.pp span);
  let%lwt response_string = response_to_string response in
  Logs.debug ~src (fun m -> m ~tags "%s" response_string);
  Lwt.return response
;;

let logger =
  let open Opium in
  let filter handler req =
    let tags = Pool_context.Logger.Tags.req req in
    let meth = Method.to_string req.Request.meth in
    let uri = req.Request.target |> Uri.of_string |> Uri.path_and_query in
    Logs.info ~src (fun m -> m ~tags "%s %S" meth uri);
    let%lwt request_string = request_to_string req in
    Logs.debug ~src (fun m -> m ~tags "%s" request_string);
    Lwt.catch
      (fun () -> respond handler req)
      (fun exn ->
        Logs.err ~src (fun m -> m ~tags "%s" (Exn.to_string exn));
        Logs.err ~src (fun m -> m ~tags "%s" (Printexc.get_backtrace ()));
        Lwt.fail exn)
  in
  Rock.Middleware.create ~name:"Logger" ~filter
;;
