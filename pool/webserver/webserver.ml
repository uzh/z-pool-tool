(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   This module corresponds to the [Sihl.Web] facade of
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/sihl.ml
   The unused [Htmx], [Rest], [Migration] and [Error] middlewares and helpers have been
   removed. *)

include Web
module Http = Web_http

module Request = struct
  include Opium.Request

  let bearer_token req =
    match Opium.Request.header "authorization" req with
    | Some authorization ->
      (try Some (Scanf.sscanf authorization "Bearer %s" (fun b -> b)) with
       | _ -> None)
    | None -> None
  ;;
end

module Response = Opium.Response
module Cookie = Opium.Cookie
module Body = Opium.Body
module Router = Opium.Router
module Route = Opium.Route

module Csrf = struct
  let find_exn = Web_csrf.find_exn
end

module Flash = struct
  let find_alert = Web_flash.find_alert
  let set_alert = Web_flash.set_alert
  let find = Web_flash.find
  let set = Web_flash.set
end

module Id = struct
  let find = Web_id.find
end

module Session = struct
  let find = Web_session.find
  let set = Web_session.set
  let set_value = Web_session.set_value
  let update_or_set_value = Web_session.update_or_set_value
end

module Middleware = struct
  let csrf = Web_csrf.middleware
  let flash = Web_flash.middleware
  let id = Web_id.middleware
  let static_file = Web_static.middleware
end
