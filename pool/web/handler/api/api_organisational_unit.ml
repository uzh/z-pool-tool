open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.api.organisational_unit"
let json_list (lst : Yojson.Safe.t list) : Yojson.Safe.t = `List lst

let index req =
  let result { Pool_context.database_label; _ } =
    let%lwt res =
      Organisational_unit.all database_label ()
      ||> CCList.map Organisational_unit.yojson_of_t
      ||> json_list
    in
    Lwt_result.return res
  in
  result |> Api_utils.respond req
;;
