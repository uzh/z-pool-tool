open Entity

type email_layout =
  { link : string
  ; logo_src : string
  ; logo_alt : string
  }
[@@deriving eq, show { with_path = false }]

let create_public_url pool_url path =
  path
  |> Sihl.Web.externalize_path
  |> Format.asprintf "http://%s%s" (Pool_tenant.Url.value pool_url)
;;

let prepend_root_directory pool url =
  match Pool_database.Label.equal pool Pool_database.root with
  | true -> Format.asprintf "/root%s" url
  | false -> url
;;

let layout_from_tenant (tenant : Pool_tenant.t) =
  let open Pool_tenant in
  let logo_src =
    tenant.logos
    |> Logos.value
    |> CCList.head_opt
    |> CCOption.map_or
         ~default:""
         CCFun.(Pool_common.File.path %> create_public_url tenant.url)
  in
  let logo_alt = tenant.title |> Title.value |> Format.asprintf "Logo %s" in
  let link = tenant.url |> Url.value |> Format.asprintf "http://%s" in
  { link; logo_src; logo_alt }
;;

let root_layout () =
  let open CCOption in
  let root_url =
    Sihl.Configuration.read_string "PUBLIC_URL"
    >>= CCFun.(Pool_tenant.Url.create %> CCOption.of_result)
  in
  let logo_src =
    root_url
    >|= (fun url -> create_public_url url "assets/images/root_logo.svg")
    |> value ~default:""
  in
  let logo_alt = "Logo Pool Tool" in
  let link = root_url >|= Pool_tenant.Url.value |> value ~default:"" in
  { link; logo_alt; logo_src }
;;

let create_layout = function
  | Tenant tenant -> layout_from_tenant tenant
  | Root -> root_layout ()
;;

let layout_params layout =
  [ "logoSrc", layout.logo_src
  ; "logoAlt", layout.logo_alt
  ; "logoHref", layout.link
  ]
;;

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ~indent:true ()) html
;;

let combine_html language html_title =
  let open Tyxml.Html in
  let email_header =
    let pool_title = "Pool Tool" in
    head
      (title
         (txt
            ((CCOption.map_or ~default:pool_title (fun title ->
                CCString.concat " - " [ title; pool_title ]))
               html_title)))
      [ meta
          ~a:
            [ a_http_equiv "Content-Type"
            ; a_content "text/html; charset=UTF-8"
            ]
          ()
      ; meta
          ~a:
            [ a_name "viewport"
            ; a_content "width=device-width, initial-scale=1"
            ]
          ()
      ; meta ~a:[ a_http_equiv "X-UA-Compatible"; a_content "IE=edge" ] ()
      ; style
          ~a:[ a_mime_type "text/css" ]
          [ Unsafe.data
              {css| body { font-family:
   BlinkMacSystemFont,-apple-system,Segoe UI,Roboto,Oxygen,Ubuntu,Cantarell,Fira
   Sans,Droid Sans,Helvetica Neue,Helvetica,Arial,sans-serif; line-height: 1.4;
   } |css}
          ]
      ]
  in
  let email_body =
    body
      ~a:[ a_style "margin:0; padding:0;" ]
      [ div
          ~a:[ a_style "margin: 1em 1em 1em 1em; max-width: 50em;" ]
          [ section
              ~a:[ a_style "margin-bottom: 1em;" ]
              [ a
                  ~a:[ a_href "{logoHref}" ]
                  [ img
                      ~src:"{logoSrc}"
                      ~alt:"{logoAlt}"
                      ~a:
                        [ a_style "width: 300px; height: auto; max-width: 100%;"
                        ]
                      ()
                  ]
              ]
          ; section
              ~a:[ a_style "padding-top: 1em; color: #383838;" ]
              [ txt "{emailText}" ]
          ; footer
              ~a:[ a_style "margin-top: 4em;" ]
              [ div ~a:[ a_style "text-align:center" ] [ p [ txt "Copyright" ] ]
              ]
          ]
      ]
  in
  html
    ~a:[ a_lang (Pool_common.Language.show language) ]
    email_header
    email_body
  |> html_to_string
;;
