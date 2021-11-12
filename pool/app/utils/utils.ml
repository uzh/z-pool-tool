module Countries = Countries
module Database = Database

let todo _ = failwith "todo"

module Lwt_result = struct
  include Lwt_result

  module Infix = struct
    include Infix

    let ( >== ) = Lwt_result.bind_result
    let ( >> ) m k = m >>= fun _ -> k
    let ( |>> ) = Lwt_result.bind_lwt
    let ( >|> ) = Lwt.bind
    let ( ||> ) m k = Lwt.map k m
  end
end

module Url = struct
  let create_public_url path =
    let public_url = Sihl.Configuration.read_string "PUBLIC_URL" in
    match public_url with
    | None -> failwith "PUBLIC_URL not found in configuration"
    | Some public_url -> Format.asprintf "%s%s" public_url path
  ;;
end

let bool_to_result err value =
  match value with
  | true -> Ok ()
  | false -> Error err
;;
