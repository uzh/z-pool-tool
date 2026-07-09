(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/web_static.ml *)

let middleware () =
  let local_path =
    Option.value (Pool_core.Configuration.read_string "PUBLIC_DIR") ~default:"./public"
  in
  let internal_uri_prefix =
    Option.value
      (Pool_core.Configuration.read_string "PUBLIC_URI_PREFIX")
      ~default:"/assets"
  in
  let uri_prefix = Web.externalize_path internal_uri_prefix in
  Opium.Middleware.static_unix ~local_path ~uri_prefix ()
;;
