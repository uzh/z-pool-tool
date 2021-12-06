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
  let to_result err value =
    match value with
    | true -> Ok ()
    | false -> Error err
  ;;
end
