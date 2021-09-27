module Url = struct
  type t = string [@@deriving eq, show]

  let create server =
    if String.length server <= 0
    then Error "Invalid database url!"
    else Ok server
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ l ])
      "database_url"
  ;;
end

module Label = struct
  type t = string [@@deriving eq, show]

  let create label =
    if String.length label <= 0 || String.contains label ' '
    then Error "Invalid database label!"
    else Ok label
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ l ])
      "database_label"
  ;;

  let value m = m
end

type t =
  { url : Url.t
  ; label : Label.t
  }
[@@deriving eq, show]

let create url label = Ok { url; label }

let add_pool model =
  Sihl.Database.add_pool
    ~pool_size:
      (Sihl.Configuration.read_string "DATABASE_POOL_SIZE"
      |> CCFun.flip CCOpt.bind CCInt.of_string
      |> CCOpt.value ~default:10)
    model.label
    model.url
;;
