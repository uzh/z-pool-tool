(* Put custom Opium middlewares here *)

let trailing_slash () =
  let filter handler req =
    let uri = req.Opium.Request.target |> Uri.of_string in
    let root_uri =
      Sihl.Configuration.read_string "PREFIX_PATH"
      |> CCOpt.value ~default:""
      |> Format.asprintf "%s/"
      |> Uri.of_string
    in
    let uri =
      uri
      |> Uri.path
      |> (fun path ->
           if Uri.equal root_uri uri
           then path (* don't drop root *)
           else path |> CCString.rdrop_while (Char.equal '/'))
      |> Uri.with_path uri
    in
    let req = Opium.Request.{ req with target = Uri.to_string uri } in
    handler req
  in
  Rock.Middleware.create ~name:"trailing_slash" ~filter
;;
