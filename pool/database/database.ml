include Entity
include Service
module Logger = Logger
module Caqti_encoders = Caqti_encoders
module Repo = Repo

let show_error_with_log = Pools.with_log

let test_and_create url label =
  let%lwt connection =
    let uri = url |> Uri.of_string in
    Caqti_lwt_unix.with_connection uri (fun (_ : Caqti_lwt.connection) ->
      Lwt_result.return ())
  in
  match connection with
  | Ok () -> create label url |> Lwt.return_ok
  | Error (_ : Caqti_error.load_or_connect) ->
    Lwt_result.fail Pool_message.(Error.Invalid Field.DatabaseUrl)
;;
