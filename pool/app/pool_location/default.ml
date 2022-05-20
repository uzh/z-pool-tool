open Entity

type default = t list [@@deriving eq, show]

let get_or_failwith = Pool_common.Utils.get_or_failwith

let default_values =
  [ ( "Online"
    , Some "Online experiment location"
    , Address.Virtual
    , None
    , Status.Active
    , [] )
  ]
  |> CCList.map (fun (label, description, address, link, status, files) ->
         create label description address link status files)
;;
