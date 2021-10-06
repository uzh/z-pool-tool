module Url = struct
  type t = string [@@deriving eq, show]

  let create url =
    if String.length url <= 0 then Error "Invalid database url!" else Ok url
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

  let value m = m

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
end

type t =
  { url : Url.t
  ; label : Label.t
  }
[@@deriving eq, show]

let create url label = Ok { url; label }
