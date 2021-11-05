let message = Pool_common_error.message

let schema_decoder create_fcn err l =
  let create_fcn m = create_fcn m |> CCResult.map_err message in
  let open CCResult in
  l
  |> CCList.head_opt
  |> CCOpt.to_result (Pool_common_error.Undefined err |> message)
  >>= create_fcn
;;
