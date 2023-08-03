open Tyxml.Html

let assets = function
  | `GlobalStylesheet -> "/assets/index.css"
  | `TenantStylesheet -> "/custom/assets/index.css"
  | `IndexJs -> "/assets/index.js"
  | `AdminJs -> "/assets/admin.js"
  | `RootFavicon -> "/assets/images/favicon.png"
;;

let charset = meta ~a:[ a_charset "utf8" ] ()
let body_tag_classnames = [ "height-100"; "flexcolumn" ]

let main_tag children =
  main [ div ~a:[ a_class [ "inset-xl"; "vertical" ] ] children ]
;;

let viewport =
  meta
    ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ]
    ()
;;

let favicon path = link ~rel:[ `Icon ] ~href:path ()

let js_script_tag (file : [ `IndexJs | `AdminJs ]) =
  let source_path = assets file |> Http_utils.externalized_path_with_version in
  script ~a:[ a_src source_path; a_defer () ] (txt "")
;;

let css_link_tag (file : [ `GlobalStylesheet | `TenantStylesheet ]) =
  link
    ~rel:[ `Stylesheet ]
    ~href:(Http_utils.externalized_path_with_version (assets file))
    ()
;;

module App = struct
  let app_name = "Z-Pool-Tool"

  let create_title query_language title =
    let path =
      Http_utils.path_with_language query_language "/index"
      |> Sihl.Web.externalize_path
    in
    div ~a:[ a_class [ "app-title" ] ] [ a ~a:[ a_href path ] [ txt title ] ]
  ;;

  let header ?(children = []) query_language title =
    header
      ~a:
        [ a_class
            [ "inset"
            ; "flexrow"
            ; "justify-between"
            ; "align-center"
            ; "bg-grey-light"
            ; "border-bottom"
            ]
        ]
      [ create_title query_language title; div children ]
  ;;

  let version = Format.asprintf "Z-Pool-Tool %s" Version.to_string

  let combine_footer_fragments fragments =
    let separator = span [ txt "|" ] in
    let rec combine html = function
      | [] -> html
      | hd :: tl ->
        (html
         @
         if CCList.length tl > 0
         then [ span [ hd ]; separator ]
         else [ span [ hd ] ])
        |> fun html -> combine html tl
    in
    combine [] fragments
  ;;

  let root_footer =
    let html = [ txt app_name; txt version ] |> combine_footer_fragments in
    footer
      ~a:
        [ a_class
            [ "inset"
            ; "flexrow"
            ; "flex-gap"
            ; "justify-center"
            ; "bg-grey-light"
            ; "border-top"
            ; "push"
            ]
        ]
      html
  ;;
end
