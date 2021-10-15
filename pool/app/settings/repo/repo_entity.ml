open Entity

module Language = struct
  include Language

  type t = Language.t list [@@deriving eq, show, yojson]

  let to_string m = m |> to_yojson |> Yojson.Safe.to_string
  let of_string m = m |> Yojson.Safe.from_string |> of_yojson

  let t =
    let encode m = Ok (m |> to_string) in
    let decode = of_string in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module EmailSuffix = struct
  include EmailSuffix

  type t = EmailSuffix.t list [@@deriving eq, show, yojson]

  let to_string m = m |> to_yojson |> Yojson.Safe.to_string
  let of_string m = m |> Yojson.Safe.from_string |> of_yojson

  let t =
    let encode m = Ok (m |> to_string) in
    let decode = of_string in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Setting = struct
  include Setting

  type t = EmailSuffix.t list [@@deriving eq, show, yojson]

  let to_string m = m |> to_yojson |> Yojson.Safe.to_string
  let of_string m = m |> Yojson.Safe.from_string |> of_yojson

  let t =
    let encode m = Ok (m |> to_string) in
    let decode = of_string in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end
