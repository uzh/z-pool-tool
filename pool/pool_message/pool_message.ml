open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv
module Field = Field
module Control = Pool_message_control
module Error = Pool_message_error
module Success = Pool_message_success

module Warning = struct
  type t = Warning of string [@@deriving eq, show, yojson, variants, sexp_of]
end

module Info = struct
  type t = Info of string [@@deriving eq, show, yojson, variants, sexp_of]
end

type t =
  | Message of string
  | PageNotFoundMessage
[@@deriving eq, show, yojson, variants, sexp_of]

module Collection = struct
  type t =
    { error : Error.t list
    ; warning : Warning.t list
    ; success : Success.t list
    ; info : Info.t list
    }
  [@@deriving eq, show, yojson, sexp_of]

  let empty = { error = []; warning = []; success = []; info = [] }
  let set_success txts message = { message with success = txts }
  let set_warning txts message = { message with warning = txts }
  let set_error txts message = { message with error = txts }
  let set_info txts message = { message with info = txts }

  let of_string str =
    let json =
      try Some (Yojson.Safe.from_string str) with
      | _ -> None
    in
    match json with
    | Some json -> Some (t_of_yojson json)
    | None -> None
  ;;

  let to_string t = yojson_of_t t |> Yojson.Safe.to_string
end

let field_message prefix field suffix =
  Format.asprintf "%s %s %s" prefix field suffix
  |> CCString.trim
  |> CCString.capitalize_ascii
;;

let handle_sihl_login_error = function
  | `Incorrect_password | `Does_not_exist -> Error.Invalid Field.Password
;;

let handle_ppx_yojson_err (exn, yojson) =
  let msg =
    Format.asprintf
      "Yojson_conv error: %s\n\nAffected yojson: %s"
      (Printexc.to_string exn)
      ([%show: Yojson.Safe.t] yojson)
  in
  Error Error.(NotHandled msg)
;;

let handle_json_parse_err str =
  let msg = Format.asprintf "Json parse error: %s" str in
  Error Error.(InvalidJson msg)
;;

let to_conformist_error error_list =
  CCList.map (fun (name, _, msg) -> name |> Field.read, msg) error_list
  |> Error.conformist
;;

let add_field_query_params url params =
  let open CCList in
  let open Uri in
  map (CCPair.map_fst Field.show) params
  |> add_query_params' (of_string url)
  |> fun uri ->
  with_query uri (query uri |> rev |> uniq ~eq:Utils.equal_key |> rev)
  |> to_string
;;
