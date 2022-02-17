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

module OneOf2 = struct
  type ('a, 'b) t =
    | One of 'a
    | Two of 'b
  [@@deriving variants]
end

module OneOf3 = struct
  type ('a, 'b, 'c) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
  [@@deriving variants]
end

module OneOf4 = struct
  type ('a, 'b, 'c, 'd) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
  [@@deriving variants]
end

module OneOf5 = struct
  type ('a, 'b, 'c, 'd, 'e) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
  [@@deriving variants]
end

module OneOf6 = struct
  type ('a, 'b, 'c, 'd, 'e, 'f) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
    | Six of 'f
  [@@deriving variants]
end

module OneOf7 = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
    | Six of 'f
    | Seven of 'g
  [@@deriving variants]
end

module OneOf8 = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
    | Six of 'f
    | Seven of 'g
    | Eight of 'h
  [@@deriving variants]
end
