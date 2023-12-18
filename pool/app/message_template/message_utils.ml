open Entity

type email_layout =
  { link : string
  ; logo_alt : string
  ; logo_src : string
  ; site_title : string
  }
[@@deriving eq, show { with_path = false }]

let create_public_url pool_url path =
  path
  |> Sihl.Web.externalize_path
  |> Format.asprintf "https://%s%s" (Pool_tenant.Url.value pool_url)
;;

let create_public_url_with_params pool_url path params =
  params
  |> Pool_common.Message.add_field_query_params path
  |> create_public_url pool_url
;;

let prepend_root_directory pool url =
  match Pool_database.is_root pool with
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
  let link = tenant.url |> Url.value |> Format.asprintf "https://%s" in
  let site_title = tenant.title |> Title.value in
  { link; logo_src; logo_alt; site_title }
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
  let logo_alt = "Logo Z-Pool-Tool" in
  let link = root_url >|= Pool_tenant.Url.value |> value ~default:"" in
  let site_title = "Z-Pool-Tool" in
  { link; logo_alt; logo_src; site_title }
;;

let create_layout = function
  | Tenant tenant -> layout_from_tenant tenant
  | Root -> root_layout ()
;;

let layout_params layout =
  [ "logoSrc", layout.logo_src
  ; "logoAlt", layout.logo_alt
  ; "logoHref", layout.link
  ; "siteTitle", layout.site_title
  ]
;;

let line_breaks_to_html str =
  str
  |> CCString.split ~by:"\n"
  |> fun lst ->
  lst |> CCList.flat_map (CCString.split ~by:"\\n") |> CCString.concat "<br>"
;;

let render_params ?cb data text =
  let replace str k v =
    let regexp = Str.regexp @@ "{" ^ k ^ "}" in
    Str.global_replace regexp v str
  in
  let rec render data value =
    match data with
    | [] -> value
    | (k, v) :: data ->
      (match cb with
       | None -> v
       | Some cb -> v |> cb)
      |> fun v -> render data @@ replace value k v
  in
  render data text
;;

let render_email_params params ({ Sihl_email.text; html; subject; _ } as email) =
  Sihl_email.
    { email with
      subject = render_params params subject
    ; text = render_params params text
    ; html = html |> CCOption.map (render_params ~cb:line_breaks_to_html params)
    }
;;

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ~indent:true ()) html
;;

let combine_html language html_title =
  let open Tyxml.Html in
  let current_year = () |> Ptime_clock.now |> Ptime.to_year in
  let email_header =
    head
      (title (txt (CCOption.value ~default:"" html_title)))
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
              {css| body { font-family:sans-serif, Arial; line-height: 1.4; } |css}
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
              [ div
                  ~a:[ a_style "text-align:center" ]
                  [ p
                      [ txt
                          (Format.asprintf
                             "Copyright © %i {siteTitle}"
                             current_year)
                      ]
                  ]
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

let find_template_by_language templates lang =
  let open Pool_common in
  CCList.find_opt
    (fun { language; _ } -> Language.equal language lang)
    templates
  |> (function
        | None -> templates |> CCList.head_opt
        | Some template -> Some template)
  |> CCOption.map (fun ({ language; _ } as t) -> language, t)
  |> CCOption.to_result (Message.NotFound Field.MessageTemplate)
;;

let with_default_language sys_langs language =
  let default = Settings.default_language_of_list sys_langs in
  sys_langs
  |> CCList.find_opt Pool_common.Language.(equal language)
  |> CCOption.value ~default
;;

let contact_language sys_langs (contact : Contact.t) =
  match contact.Contact.language with
  | None -> Settings.default_language_of_list sys_langs
  | Some language -> with_default_language sys_langs language
;;

let experiment_or_contact_lang sys_langs contact = function
  | Some experiment_language ->
    with_default_language sys_langs experiment_language
  | None -> contact_language sys_langs contact
;;

let experiment_message_language
  sys_langs
  ({ Experiment.language; _ } : Experiment.t)
  contact
  =
  experiment_or_contact_lang sys_langs contact language
;;

let public_experiment_message_language
  sys_langs
  ({ Experiment.Public.language; _ } : Experiment.Public.t)
  contact
  =
  experiment_or_contact_lang sys_langs contact language
;;
