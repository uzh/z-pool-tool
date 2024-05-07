include Sihl.Contract.Storage

let log_src = Logs.Src.create "pool.storage"

module Logs = (val Logs.src_log log_src : Logs.LOG)

let find_opt = Repo.get_file

let find label id =
  let%lwt file = Repo.get_file label id in
  match file with
  | None ->
    raise (Sihl.Contract.Storage.Exception ("File not found with id " ^ id))
  | Some file -> Lwt.return file
;;

let delete label id =
  let%lwt file = find label id in
  let blob_id = file.Sihl.Contract.Storage.blob in
  let%lwt () = Repo.delete_file label file.file.id in
  Repo.delete_blob label blob_id
;;

let upload_base64 label ?id file base64 =
  let blob_id = CCOption.value id ~default:(Uuidm.v `V4 |> Uuidm.to_string) in
  let%lwt blob =
    match Base64.decode base64 with
    | Error (`Msg msg) ->
      Logs.err (fun m ->
        m "Could not upload base64 content of file %a" pp_file file);
      raise (Sihl.Contract.Storage.Exception msg)
    | Ok blob -> Lwt.return blob
  in
  let%lwt () = Repo.insert_blob label ~id:blob_id blob in
  let stored_file = Sihl.Contract.Storage.{ file; blob = blob_id } in
  let%lwt () = Repo.insert_file label stored_file in
  Lwt.return stored_file
;;

let update_base64 label file base64 =
  let blob_id = file.Sihl.Contract.Storage.blob in
  let%lwt blob =
    match Base64.decode base64 with
    | Error (`Msg msg) ->
      Logs.err (fun m ->
        m "Could not upload base64 content of file %a" pp_stored file);
      raise (Sihl.Contract.Storage.Exception msg)
    | Ok blob -> Lwt.return blob
  in
  let%lwt () = Repo.update_blob label ~id:blob_id blob in
  let%lwt () = Repo.update_file label file in
  Lwt.return file
;;

let download_data_base64_opt label file =
  let blob_id = file.Sihl.Contract.Storage.blob in
  let%lwt blob = Repo.get_blob label blob_id in
  match CCOption.map Base64.encode blob with
  | Some (Error (`Msg msg)) ->
    Logs.err (fun m ->
      m "Could not get base64 content of file %a" pp_stored file);
    raise (Sihl.Contract.Storage.Exception msg)
  | Some (Ok blob) -> Lwt.return @@ Some blob
  | None -> Lwt.return None
;;

let download_data_base64 label file =
  let blob_id = file.Sihl.Contract.Storage.blob in
  let%lwt blob = Repo.get_blob label blob_id in
  match CCOption.map Base64.encode blob with
  | Some (Error (`Msg msg)) ->
    Logs.err (fun m ->
      m "Could not get base64 content of file %a" pp_stored file);
    raise (Sihl.Contract.Storage.Exception msg)
  | Some (Ok blob) -> Lwt.return blob
  | None ->
    raise
      (Sihl.Contract.Storage.Exception
         (Format.asprintf "File data not found for file %a" pp_stored file))
;;

let start () = Lwt.return ()
let stop () = Lwt.return ()

let lifecycle =
  Sihl.Container.create_lifecycle
    "storage"
    ~dependencies:(fun () -> [ Pool_database.lifecycle ])
    ~start
    ~stop
;;

let register () =
  Repo.register_migration ();
  Repo.register_cleaner ();
  Sihl.Container.Service.create lifecycle
;;
