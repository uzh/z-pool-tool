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

module User = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create server =
    if String.length server <= 0
    then Error "Invalid database user!"
    else Ok server
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "database_user"
  ;;
end

type t =
  { url : Url.t
  ; user : User.t
  }
[@@deriving eq, show]

let create url user = Ok { url; user }
