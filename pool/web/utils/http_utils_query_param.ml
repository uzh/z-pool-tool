let to_string = Pool_common.Message.field_name

let find_lang req =
  let open CCOption.Infix in
  Sihl.Web.Request.query (to_string Pool_common.Message.Language) req
  >>= fun l ->
  l
  |> CCString.uppercase_ascii
  |> Pool_common.Language.of_string
  |> CCOption.of_result
;;

let add_query_params path params =
  Format.asprintf
    "%s?%s"
    path
    (CCList.map
       (fun (key, value) -> Format.asprintf "%s=%s" (to_string key) value)
       params
    |> CCString.concat "&")
;;
