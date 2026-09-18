type form_param =
  | Experiment of Experiment.t
  | Template of Filter.t option

let database_connection_from_req req fnc =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  req |> find |> Lwt_result.lift >>= CCFun.flip connection fnc

