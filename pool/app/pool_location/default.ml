open Entity

type default = t list [@@deriving eq, show]

let get_or_failwith = Pool_common.Utils.get_or_failwith

let default_values =
  [ "Online", Some "Online experiment location", None, None, Status.Active ]
  |> CCList.map (fun (label, description, address, link, status) ->
         let address =
           match address with
           | Some (institution, room, building, street, zip, city) ->
             Address.Mail.create institution room building street zip city
             |> get_or_failwith
             |> Address.physical
           | None -> Address.Virtual
         in
         create label description address link status [] |> get_or_failwith)
;;
