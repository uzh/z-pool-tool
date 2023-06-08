let get_or_failwith = Pool_common.Utils.get_or_failwith

let create pool =
  let open CCFun in
  let data = [ "SNS"; "TNU" ] in
  let events =
    let open Organisational_unit in
    data
    |> CCList.map (fun name ->
         let m = name |> Name.of_string |> create in
         Organisational_unit.Created m)
  in
  let%lwt () = Lwt_list.iter_s (Organisational_unit.handle_event pool) events in
  Lwt.return_unit
;;
