open Sihl.Web

let site_middlewares =
  [ Middleware.csrf
      ~not_allowed_handler:(fun _ ->
        (* NOTE: Add specific "not allowed" page if requirements change, as of
           now showing a general error message is fine *)
        let site = Page.Utils.error_general () in
        Lwt.return @@ Sihl.Web.Response.of_html site)
      ()
  ; Middleware.flash ()
  ]
;;

let global_middlewares =
  List.concat
    [ [ Middleware.id ()
      ; Middleware.error ()
      ; Middleware.trailing_slash ()
      ; Middleware.static_file ()
      ; Middleware.migration Service.Migration.pending_migrations
      ; Opium.Middleware.content_length
      ; Opium.Middleware.etag
      ; Opium.Middleware.method_override
      ]
    ; site_middlewares
    ]
;;

module Public = struct
  let routes = [ get "/" Handler.Public.index ]
end

let router = choose [ choose ~middlewares:global_middlewares Public.routes ]
