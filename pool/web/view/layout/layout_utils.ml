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

  let create_title query_parameters title =
    let path =
      Http_utils.url_with_field_params query_parameters "/index"
      |> Sihl.Web.externalize_path
    in
    div ~a:[ a_class [ "app-title" ] ] [ a ~a:[ a_href path ] [ txt title ] ]
  ;;

  let navbar ?(children = []) query_parameters title =
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
      [ create_title query_parameters title; div children ]
  ;;

  let version = Format.asprintf "Z-Pool-Tool %s" Version.to_string

  let combine_footer_fragments
    ?(column_mobile = false)
    ?(classnames = [])
    fragments
    =
    let classnames = [ "flexrow"; "flex-gap" ] @ classnames in
    let classnames =
      if column_mobile then "flexcolumn-mobile" :: classnames else classnames
    in
    let separator =
      let text = [ txt "|" ] in
      if column_mobile
      then span ~a:[ a_class [ "hidden-mobile" ] ] text
      else span text
    in
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
    combine [] fragments |> div ~a:[ a_class classnames ]
  ;;

  let root_footer =
    let html = [ txt app_name; txt version ] |> combine_footer_fragments in
    footer
      ~a:
        [ a_class
            [ "inset"; "justify-center"; "bg-grey-light"; "border-top"; "push" ]
        ]
      [ html ]
  ;;
end
