let render_params ?cb data text =
  let replace str k v =
    let regexp = Str.regexp @@ "{" ^ k ^ "}" in
    Str.global_replace regexp v str
  in
  let rec render data value =
    match data with
    | [] -> value
    | (k, v) :: data ->
      (match cb with
       | None -> v
       | Some cb -> v |> cb)
      |> fun v -> render data @@ replace value k v
  in
  render data text
;;
