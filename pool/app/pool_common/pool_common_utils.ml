let schema_decoder create_fcn err l =
  let open CCResult in
  let open Locales_en in
  let create_fcn m = create_fcn m |> CCResult.map_err error_to_string in
  l
  |> CCList.head_opt
  |> CCOpt.to_result (Entity_message.Undefined err |> error_to_string)
  >>= create_fcn
;;
