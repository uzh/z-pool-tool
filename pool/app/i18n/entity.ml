module Common = Pool_common

module Key = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create key =
    if String.length key <= 0 then Error "Invalid key!" else Ok key
  ;;

  let schema () =
    Conformist.custom (fun l -> l |> List.hd |> create) (fun l -> [ l ]) "key"
  ;;
end

module Content = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create content =
    if String.length content <= 0 then Error "Invalid content!" else Ok content
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ l ])
      "content"
  ;;
end

type t =
  { id : Common.Id.t
  ; key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }
[@@deriving eq, show]

let create key language content =
  { id = Common.Id.create (); key; language; content }
;;
