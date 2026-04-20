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

let main_tag ?(announcement = txt "") children =
  main
    [ announcement
    ; div ~a:[ a_class [ "inset-xl"; "sm-inset-lg"; "vertical" ] ] children
    ]
;;

let viewport =
  meta ~a:[ a_name "viewport"; a_content "width=device-width, initial-scale=1" ] ()
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

  let create_title query_parameters title =
    let path =
      Http_utils.url_with_field_params query_parameters "/index"
      |> Sihl.Web.externalize_path
    in
    a ~a:[ a_href path; a_class [ "app-title" ] ] [ txt title ]
  ;;

  let navbar ?(children = []) query_parameters title =
    header
      ~a:[ a_class [ "app-header" ] ]
      [ div
          ~a:[ a_class [ "safety-margin"; "trim"; "flexcolumn"; "justify-between" ] ]
          [ div
              ~a:[ a_class [ "app-header-top" ] ]
              [ create_title query_parameters title; div children ]
          ]
      ]
  ;;

  let version = Format.asprintf "Z-Pool-Tool %s" Version.to_string

  let create_footer ?(app_name = app_name) ?(fragments = []) () =
    let separator = span ~a:[ a_class [ "hidden-mobile" ] ] [ txt "|" ] in
    let base_class = [ "flexrow"; "flex-gap" ] in
    let classnames =
      base_class @ [ "safety-margin"; "justify-center"; "flexcolumn-mobile" ]
    in
    let app_id =
      [ app_name; "|"; version ]
      |> CCList.map (fun text -> span [ txt text ])
      |> span ~a:[ a_class base_class ]
    in
    div
      ~a:[ a_class [ "inset"; "vertical"; "border-top" ] ]
      [ div ~a:[ a_class classnames ] (app_id :: CCList.intersperse separator fragments) ]
  ;;
end
