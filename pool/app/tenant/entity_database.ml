module Url = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create server =
    if String.length server <= 0
    then Error "Invalid database url!"
    else Ok server
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "database_url"
  ;;
end

module Label = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create label =
    if String.length label <= 0 || String.contains label ' '
    then Error "Invalid database label!"
    else Ok label
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "database_label"
  ;;
end

type t =
  { url : Url.t
  ; label : Label.t
  }
[@@deriving eq, show]

let create url label = Ok { url; label }
