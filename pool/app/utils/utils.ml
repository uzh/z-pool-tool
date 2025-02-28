module Countries = Countries
module LanguageCodes = Language_codes
module PhoneCodes = Phone_codes
module Json = Json
module Message = Message

module Lwt_result : sig
  module Infix : sig
    val ( let* )
      :  ('a, 'b) Lwt_result.t
      -> ('a -> ('c, 'b) Lwt_result.t)
      -> ('c, 'b) Lwt_result.t

    val ( >|> ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
    val ( ||> ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

    val ( >>= )
      :  ('a, 'b) Lwt_result.t
      -> ('a -> ('c, 'b) Lwt_result.t)
      -> ('c, 'b) Lwt_result.t

    val ( >== )
      :  ('a, 'b) Lwt_result.t
      -> ('a -> ('c, 'b) result)
      -> ('c, 'b) Lwt_result.t

    val ( >> ) : ('a, 'b) Lwt_result.t -> ('c, 'b) Lwt_result.t -> ('c, 'b) Lwt_result.t
    val ( |>> ) : ('a, 'b) Lwt_result.t -> ('a -> 'c Lwt.t) -> ('c, 'b) Lwt_result.t
    val ( >|+ ) : ('a, 'b) Lwt_result.t -> ('a -> 'c) -> ('c, 'b) Lwt_result.t
    val ( >|- ) : ('a, 'b) Lwt_result.t -> ('b -> 'c) -> ('a, 'c) Lwt_result.t
  end

  val map_error : ('a -> 'b) -> ('c, 'a) Lwt_result.t -> ('c, 'b) Lwt_result.t
end =
  Lwt_trace

module Crypto = Crypto

let flat_unit (_ : unit list) = ()
let todo _ = failwith "todo"
let equal_key (a, _) (b, _) = CCString.equal a b
let ppx_printer m fmt _ = Format.pp_print_string fmt m

let bool_to_result_not err = function
  | true -> Error err
  | false -> Ok ()
;;

let group_tuples data =
  let open CCOption in
  let open Hashtbl in
  let tbl = create 20 in
  data
  |> CCList.iter (fun (key, item) ->
    find_opt tbl key >|= CCList.cons item |> value ~default:[ item ] |> replace tbl key)
  |> CCFun.const (fold (fun key items acc -> (key, items) :: acc) tbl [])
;;

let remove_whitespaces =
  let open Re in
  replace_string (space |> compile) ~by:""
;;

let ignore_res = function
  | Ok () -> ()
  | Error _ -> ()
;;

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
  let handled_false_values = [ "off"; "false" ]

  let to_result err value =
    match value with
    | true -> Ok ()
    | false -> Error err
  ;;

  let of_string s = CCList.mem ~eq:CCString.equal s handled_true_values

  let of_string_opt s =
    let eq = CCString.equal in
    let mem = CCList.mem ~eq s in
    if mem handled_true_values
    then Some true
    else if mem handled_false_values
    then Some false
    else None
  ;;

  let to_string = Bool.to_string
end

module Html = struct
  open Tyxml.Html

  (* placed here due to circular dependency between email and http_utils library *)
  let handle_line_breaks finally_fcn str =
    finally_fcn
    @@
    match
      str |> CCString.split ~by:"\n" |> CCList.flat_map (CCString.split ~by:"\\n")
    with
    | [] -> []
    | head :: tail ->
      CCList.fold_left (fun html str -> html @ [ br (); txt str ]) [ txt head ] tail
  ;;

  let concat_html ?(by = br ()) (elements : [> `P | `Div ] elt list) =
    let rec folder html = function
      | [] -> html
      | hd :: [] -> hd :: html
      | hd :: tl -> folder ([ by; hd ] @ html) tl
    in
    elements |> CCList.rev |> folder []
  ;;
end
