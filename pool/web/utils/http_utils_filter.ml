type form_param =
  | Experiment of Experiment.t
  | Template of Filter.t option

let database_label_from_req req =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  req |> find |> Lwt_result.lift >|+ fun { database_label; _ } -> database_label
;;
