let middleware () =
  let filter handler req =
    let root_path =
      Sihl.Configuration.read_string "PREFIX_PATH"
      |> CCOption.value ~default:""
      |> Format.asprintf "%s/"
    in
    let uri = req.Opium.Request.target |> Uri.of_string in
    let uri =
      uri
      |> Uri.path
      |> (fun path ->
      if CCString.equal root_path path
      then path (* don't drop root *)
      else path |> CCString.rdrop_while (Char.equal '/'))
      |> Uri.with_path uri
    in
    let req = Opium.Request.{ req with target = Uri.to_string uri } in
    handler req
  in
  Rock.Middleware.create ~name:"trailing_slash" ~filter
;;
