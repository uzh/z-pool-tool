(* TODO [timhub]: make it env variable? *)
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

let upload_files req =
  let%lwt _ = prepare_import_directory () in
  let files = Hashtbl.create ~random:true 5 in
  let assocs = Hashtbl.create ~random:true 5 in
  let callback ~name ~filename string =
    if String.equal filename ""
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
  let ic = open_in filename in
  let filesize = in_channel_length ic in
  let data = Bytes.create filesize in
  let () = really_input ic data 0 filesize in
  let data = Bytes.to_string data in
  let () = close_in ic in
  filesize, data
;;

let file_to_storage_add filename =
  let filesize, data = load_file filename in
  let asset_id = Pool_common.Id.create () |> Pool_common.Id.value in
  let file =
    Sihl_storage.
      { id = asset_id
      ; filename = Filename.basename filename
      ; filesize
      ; mime = "application/pdf" (* TODO [timhub]: select correct filetype *)
      }
  in
  let base64 = Base64.encode_exn data in
  let%lwt _ = Service.Storage.upload_base64 file ~base64 in
  let%lwt _ = remove_imported_file filename in
  Lwt.return file.Sihl_storage.id
;;

let multipart_form_data_to_urlencoded req =
  let%lwt multipart_encoded = Sihl.Web.Request.to_multipart_form_data_exn req in
  let%lwt filenames = upload_files req in
  let%lwt filenames =
    Lwt_list.map_s
      (fun (k, v) ->
        let%lwt id = file_to_storage_add v in
        Lwt.return (k, id))
      filenames
  in
  CCList.map (fun (k, v) -> k, [ v ]) (filenames @ multipart_encoded)
  |> Lwt.return
;;
