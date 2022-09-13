module Database = Pool_database
module Id = Pool_common.Id
module File = Pool_common.File

let import_dir = "/tmp/pool/import"

let raise_if_failed msg process_result =
  match process_result with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
    let msg = Format.asprintf "Process exited with exit code %d %s" n msg in
    failwith msg
  | Unix.WSIGNALED n ->
    let msg = Format.asprintf "Process was killed by a signal %d %s" n msg in
    failwith msg
  | Unix.WSTOPPED n ->
    let msg = Format.asprintf "Process was stopped by a signal %d %s" n msg in
    failwith msg
;;

let prepare_import_directory () =
  Logs.debug (fun m -> m "IMPORT: Making sure directory exist");
  let message = Format.asprintf "while creating directory %s" import_dir in
  Lwt_process.exec ("", [| "mkdir"; "-p"; import_dir |])
  |> Lwt.map (raise_if_failed message)
;;

let save_files allow_list req =
  let%lwt () = prepare_import_directory () in
  let files = Hashtbl.create ~random:true 5 in
  let assocs = Hashtbl.create ~random:true 5 in
  let callback ~name ~filename string =
    if CCString.equal filename ""
       || not (CCList.mem ~eq:CCString.equal name allow_list)
    then Lwt.return_unit
    else (
      let filename = Filename.basename filename in
      let write file =
        string
        |> CCString.length
        |> Lwt_unix.write_string file string 0
        |> Lwt.map ignore
      in
      match Hashtbl.find_opt files filename with
      | Some file -> write file
      | None ->
        let%lwt file =
          Lwt_unix.openfile
            (Filename.concat import_dir filename)
            Unix.[ O_CREAT; O_TRUNC; O_WRONLY; O_NONBLOCK ]
            0o600
        in
        Hashtbl.add files filename file;
        Hashtbl.add assocs name filename;
        write file)
  in
  let%lwt _ = Sihl.Web.Request.to_multipart_form_data_exn ~callback req in
  CCList.map
    (fun (name, filename) -> name, Filename.concat import_dir filename)
    (Hashtbl.fold (fun k v acc -> (k, v) :: acc) assocs [])
  |> Lwt.return
;;

let remove_imported_file filename =
  Logs.debug (fun m -> m "IMPORT: Remove imported file");
  Lwt_process.exec ("", [| "rm"; "-f"; filename |])
  |> Lwt.map (raise_if_failed ("while deleting file " ^ filename))
;;

let load_file filename =
  let open CCResult in
  let ic = open_in filename in
  let filesize = in_channel_length ic in
  let data = Bytes.create filesize in
  let* mime = File.Mime.of_filename filename in
  really_input ic data 0 filesize;
  let data = Bytes.to_string data in
  close_in ic;
  Ok (filesize, mime, data)
;;

let file_to_storage_add pool filename =
  let open Lwt_result.Syntax in
  let* filesize, mime, data = load_file filename |> Lwt_result.lift in
  let asset_id = Id.(create () |> value) in
  let file =
    Sihl_storage.
      { id = asset_id
      ; filename = Filename.basename filename
      ; filesize
      ; mime = File.Mime.to_string mime
      }
  in
  let base64 = Base64.encode_exn data in
  let%lwt _ =
    Service.Storage.upload_base64 ~ctx:(Pool_tenant.to_ctx pool) file base64
  in
  let%lwt () = remove_imported_file filename in
  file.Sihl_storage.id |> Lwt.return_ok
;;

let multipart_form_data_to_urlencoded list =
  let fields = Hashtbl.create ~random:true (CCList.length list) in
  let () =
    CCList.iter
      (fun (k, v) ->
        match Hashtbl.find_opt fields k with
        | None -> Hashtbl.add fields k [ v ]
        | Some lst -> Hashtbl.replace fields k (CCList.cons v lst))
      list
  in
  fields |> Hashtbl.to_seq |> CCList.of_seq
;;

let upload_files pool allow_list req =
  let open Lwt_result.Syntax in
  let open Utils.Lwt_result.Infix in
  save_files allow_list req
  >|> Lwt_list.map_s (fun (k, v) ->
        let* id = file_to_storage_add pool v in
        Lwt.return_ok (k, id))
  ||> CCResult.flatten_l
;;

let update_files pool files req =
  let%lwt filenames = save_files (CCList.map fst files) req in
  let update_asset key id =
    let filename = CCList.assoc_opt ~eq:CCString.equal key filenames in
    match filename with
    | Some filename ->
      (match load_file filename with
       | Ok (filesize, mime, data) ->
         let ctx = Pool_tenant.to_ctx pool in
         let%lwt file = Service.Storage.find ~ctx id in
         let updated_file =
           let open Sihl_storage in
           file
           |> set_filename_stored (Filename.basename filename)
           |> set_filesize_stored filesize
           |> set_mime_stored (File.Mime.to_string mime)
         in
         let base64 = Base64.encode_exn data in
         let%lwt _ = Service.Storage.update_base64 ~ctx updated_file base64 in
         let%lwt () = remove_imported_file filename in
         Lwt.return_some (Ok id)
       | Error err -> Lwt.return_some (Error err))
    | None -> Lwt.return_none
  in
  let%lwt result =
    Lwt_list.filter_map_p
      (fun (key, assets_id) -> update_asset key assets_id)
      files
  in
  result |> CCList.all_ok |> Lwt.return
;;
