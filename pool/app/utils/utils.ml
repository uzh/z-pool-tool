include One_of_n
module Countries = Countries
module Stringify = Stringify
module Database = Database
module Time = Time

let todo _ = failwith "todo"

module Lwt_result = struct
  include Lwt_result

  module Infix = struct
    include Infix

    let ( >> ) m k = m >>= fun _ -> k
    let ( |>> ) = Lwt_result.bind_lwt
    let ( >|> ) = Lwt.bind
  end
end

let handle_conformist_error (err : Conformist.error list) =
  String.concat
    "\n"
    (List.map (fun (m, _, k) -> Format.asprintf "%s: %s" m k) err)
;;

let schema_decoder create_fcn msg l =
  let open CCResult in
  l
  |> CCList.head_opt
  |> CCOpt.to_result (Format.asprintf "Undefined %s" msg)
  >>= create_fcn
;;

module Url = struct
  let create_public_url path =
    let public_url = Sihl.Configuration.read_string "PUBLIC_URL" in
    match public_url with
    | None -> failwith "PUBLIC_URL not found in configuration"
    | Some public_url -> Format.asprintf "%s%s" public_url path
  ;;
end
