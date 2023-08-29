open Entity

type default = t list [@@deriving eq, show]

let get_or_failwith = Pool_common.Utils.get_or_failwith

let default_values =
  let description =
    Pool_common.Language.all
    |> CCList.map (fun lang -> lang, "Online experiment location")
    |> CCOption.pure
  in
  [ "Online", None, None, Status.Active ]
  |> CCList.map (fun (label, address, link, status) ->
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
