open Sexplib.Std

type t =
  { error : string list
  ; warning : string list
  ; success : string list
  ; info : string list
  }
[@@deriving eq, show, yojson, sexp]

let empty = { error = []; warning = []; success = []; info = [] }
let set_success txts message = { message with success = txts }
let set_warning txts message = { message with warning = txts }
let set_error txts message = { message with error = txts }
let set_info txts message = { message with info = txts }
let get_error message = message.error
let get_warning message = message.warning
let get_success message = message.success
let get_info message = message.info

let of_string str =
  let json =
    try Some (Yojson.Safe.from_string str) with
    | _ -> None
  in
  match json with
  | Some json ->
    (match of_yojson json with
    | Ok result -> Some result
    | Error _ -> None)
  | None -> None
;;

let to_string t = to_yojson t |> Yojson.Safe.to_string

let set ?(error = []) ?(warning = []) ?(success = []) ?(info = []) res =
  let message =
    empty
    |> set_error error
    |> set_warning warning
    |> set_success success
    |> set_info info
  in
  let message = to_string message in
  (* We use alerts for all messages *)
  Sihl.Web.Flash.set_alert message res
;;
