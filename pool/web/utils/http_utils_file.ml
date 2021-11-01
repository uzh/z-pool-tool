module Database = Pool_common.Database
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
  Lwt_process.exec ("", [| "mkdir"; "-p"; import_dir |])
  |> Lwt.map (raise_if_failed ("while creating directory " ^ import_dir))
;;

let save_files allow_list req =
  let%lwt _ = prepare_import_directory () in
  let files = Hashtbl.create ~random:true 5 in
  let assocs = Hashtbl.create ~random:true 5 in
  let callback ~name ~filename string =
    if String.equal filename ""
       || not (CCList.mem ~eq:CCString.equal name allow_list)
    then Lwt.return_unit
    else (
      let filename = Filename.basename filename in
      let write file =
        string
        |> String.length
        |> Lwt_unix.write_string file string 0
        |> Lwt.map ignore
      in
      match Hashtbl.find_opt files filename with
      | Some file -> write file
      | None ->
        let%lwt file =
          Lwt_unix.openfile
            (Format.asprintf "%s/%s" import_dir filename)
            Unix.[ O_CREAT; O_TRUNC; O_WRONLY; O_NONBLOCK ]
            0o600
        in
        Hashtbl.add files filename file;
        Hashtbl.add assocs name filename;
        write file)
  in
  let%lwt _ = Sihl.Web.Request.to_multipart_form_data_exn ~callback req in
  Lwt.return
    (List.map
       (fun (name, filename) ->
         name, Format.asprintf "%s/%s" import_dir filename)
       (Hashtbl.fold (fun k v acc -> (k, v) :: acc) assocs []))
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
  let () = really_input ic data 0 filesize in
  let data = Bytes.to_string data in
  let () = close_in ic in
  Ok (filesize, mime, data)
;;

let file_to_storage_add filename =
  let open Lwt_result.Syntax in
  let* filesize, mime, data = load_file filename |> Lwt_result.lift in
  let asset_id = Id.create () |> Id.value in
  let file =
    Sihl_storage.
      { id = asset_id
      ; filename = Filename.basename filename
      ; filesize
      ; mime = File.Mime.to_string mime
      }
  in
  let base64 = Base64.encode_exn data in
  let%lwt _ = Service.Storage.upload_base64 file ~base64 in
  let%lwt _ = remove_imported_file filename in
  Lwt_result.return file.Sihl_storage.id
;;

let multipart_form_data_to_urlencoded (list : (string * string) list) =
  CCList.map (fun (k, v) -> k, [ v ]) list
;;

let upload_files allow_list req =
  let%lwt filenames = save_files allow_list req in
  let%lwt filenames =
    let open Lwt_result.Syntax in
    Lwt_list.map_s
      (fun (k, v) ->
        let* id = file_to_storage_add v in
        Lwt_result.return (k, Pool_common.Id.of_string id))
      filenames
  in
  let filenames = filenames |> CCResult.flatten_l in
  match filenames with
  | Error err -> Lwt.return_error err
  | Ok filenames -> Ok filenames |> Lwt_result.lift
;;

let update_files files req =
  let%lwt filenames = save_files (CCList.map fst files) req in
  let callback ~name:_ ~filename _ =
    if String.equal filename "" then Lwt.return_unit else Lwt.return_unit
  in
  let%lwt _ = Sihl.Web.Request.to_multipart_form_data_exn ~callback req in
  let update_asset key id =
    let filename = CCList.assoc_opt ~eq:CCString.equal key filenames in
    match filename with
    | Some filename ->
      let%lwt filedata = load_file filename |> Lwt_result.lift in
      (match filedata with
      | Ok (filesize, mime, data) ->
        let%lwt file =
          Service.Storage.find
            ~ctx:[ "pool", Database.root |> Database.Label.value ]
            ~id
        in
        let updated_file =
          let open Sihl_storage in
          file
          |> set_filename_stored (Filename.basename filename)
          |> set_filesize_stored filesize
          |> set_mime_stored (File.Mime.to_string mime)
        in
        let base64 = Base64.encode_exn data in
        let%lwt _ = Service.Storage.update_base64 updated_file ~base64 in
        let%lwt _ = remove_imported_file filename in
        Lwt.return_some (Ok id)
      | Error err -> Lwt.return_some (Error err))
    | None -> Lwt.return_none
  in
  let%lwt result =
    Lwt_list.filter_map_p
      (fun (key, assets_id) -> update_asset key assets_id)
      files
  in
  CCList.all_ok result |> Lwt.return
;;
