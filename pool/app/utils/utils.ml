module Countries = Countries
module Database = Database
module LanguageCodes = Language_codes
module Lwt_result = Lwt_trace
module Crypto = Crypto

let todo _ = failwith "todo"
let fcn_ok fcn m = m |> fcn |> CCResult.pure
let equal_key (a, _) (b, _) = CCString.equal a b
let ppx_printer m fmt _ = Format.pp_print_string fmt m

module Url = struct
  let public_host =
    let open CCOption in
    let decode_host url =
      let uri = url |> Uri.of_string in
      match Uri.host uri, Uri.port uri with
      | Some host, None -> Some host
      | Some host, Some port -> Some (Format.asprintf "%s:%d" host port)
      | None, _ -> None
    in
    Sihl.Configuration.read_string "PUBLIC_URL" >>= decode_host
  ;;
end

module Bool = struct
  let handled_true_values = [ "on"; "checked"; "true" ]

  let to_result err value =
    match value with
    | true -> Ok ()
    | false -> Error err
  ;;

  let of_string s = CCList.mem ~eq:CCString.equal s handled_true_values
  let to_string = Bool.to_string
end

module Html = struct
  (* placed here due to circular dependency between email and http_utils
     library *)
  let handle_line_breaks finally_fcn str =
    let open Tyxml.Html in
    finally_fcn
    @@
    match
      str
      |> CCString.split ~by:"\n"
      |> CCList.map (CCString.split ~by:"\\n")
      |> CCList.flatten
    with
    | [] -> []
    | head :: tail ->
      CCList.fold_left
        (fun html str -> html @ [ br (); txt str ])
        [ txt head ]
        tail
  ;;
end
