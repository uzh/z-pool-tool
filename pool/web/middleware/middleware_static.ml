open Lwt.Syntax

module Body = struct
  include Rock.Body

  let log_src = Logs.Src.create "opium.body.of_file"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  exception Isnt_a_file

  let of_file fname =
    let open Lwt.Syntax in
    (* TODO: allow buffer size to be configurable *)
    let bufsize = 4096 in
    Lwt.catch
      (fun () ->
         let* s = Lwt_unix.stat fname in
         let* () =
           if Unix.(s.st_kind <> S_REG) then Lwt.fail Isnt_a_file else Lwt.return_unit
         in
         let* ic =
           Lwt_io.open_file
             ~buffer:(Lwt_bytes.create bufsize)
             ~flags:[ Unix.O_RDONLY ]
             ~mode:Lwt_io.input
             fname
         in
         let+ size = Lwt_io.length ic in
         let stream =
           Lwt_stream.from (fun () ->
             Lwt.catch
               (fun () ->
                  let+ b = Lwt_io.read ~count:bufsize ic in
                  match b with
                  | "" -> None
                  | buf -> Some buf)
               (fun exn ->
                  Log.warn (fun m ->
                    m "Error while reading file %s. %s" fname (Printexc.to_string exn));
                  Lwt.return_none))
         in
         Lwt.on_success (Lwt_stream.closed stream) (fun () ->
           Lwt.async (fun () -> Lwt_io.close ic));
         Some (of_stream ~length:size stream))
      (fun e ->
         match[@warning "-4"] e with
         | Isnt_a_file | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return None
         | exn ->
           Logs.err (fun m ->
             m "Unknown error while serving file %s. %s" fname (Printexc.to_string exn));
           Lwt.fail exn)
  ;;
end

let default_etag ~local_path fname =
  let fpath = Filename.concat local_path fname in
  let* exists = Lwt_unix.file_exists fpath in
  if exists
  then
    let* stat = Lwt_unix.stat fpath in
    let hash =
      Marshal.to_string stat.Unix.st_mtime []
      |> Digestif.MD5.digest_string
      |> Digestif.MD5.to_raw_string
      |> Base64.encode_exn
    in
    Lwt.return_some hash
  else Lwt.return_none
;;

let m ~local_path ?uri_prefix ?headers ?(etag_of_fname = default_etag ~local_path) () =
  let read fname =
    let chop_query_string s =
      match String.index_opt s '?' with
      | Some idx -> String.sub s 0 idx
      | None -> s
    in
    let fname = fname |> chop_query_string in
    let* body = Body.of_file (Filename.concat local_path fname) in
    match body with
    | None -> Lwt.return (Error `Not_found)
    | Some body -> Lwt.return (Ok body)
  in
  Opium.Middleware.static ~read ?uri_prefix ?headers ~etag_of_fname ()
;;

let middleware =
  let local_path =
    Option.value (Sihl.Configuration.read_string "PUBLIC_DIR") ~default:"./public"
  in
  let internal_uri_prefix =
    Option.value (Sihl.Configuration.read_string "PUBLIC_URI_PREFIX") ~default:"/assets"
  in
  m ~local_path ~uri_prefix:internal_uri_prefix
;;
