(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/web_id.ml *)

let key : string Opium.Context.key =
  Opium.Context.Key.create ("id", Sexplib.Std.sexp_of_string)
;;

let find req = Opium.Context.find key req.Opium.Request.env

let set id req =
  let env = req.Opium.Request.env in
  let env = Opium.Context.add key id env in
  { req with env }
;;

let middleware ?(id = fun () -> Pool_core.Random.base64 64) () =
  let filter handler req =
    match Opium.Request.header "x-request-id" req with
    | Some request_id -> handler (set request_id req)
    | None ->
      let request_id = id () in
      handler (set request_id req)
  in
  Rock.Middleware.create ~name:"id" ~filter
;;
