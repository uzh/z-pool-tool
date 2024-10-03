open Utils.Lwt_result.Infix

let src = Logs.Src.create "handler.api.organisational_unit"

let index req =
  let result { Pool_context.database_label; _ } =
    Organisational_unit.all database_label ()
    ||> CCList.map Organisational_unit.yojson_of_t
    ||> (fun json -> `List json)
    |> Lwt_result.ok
  in
  result |> Api_utils.respond req
;;
