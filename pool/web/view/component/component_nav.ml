open Tyxml.Html

let make_tabs html links =
  let nav =
    CCList.map
      (fun (label, url, active) ->
        if active
        then span ~a:[ a_class [ "active" ] ] [ txt label ]
        else a ~a:[ a_href (Sihl.Web.externalize_path url) ] [ txt label ])
      links
    |> nav ~a:[ a_class [ "tab-nav"; "flexrow"; "flex-gap" ] ]
  in
  div [ nav; div ~a:[ a_class [ "tab-body" ] ] html ]
;;

let tab_navigation language links active html =
  let open Pool_common in
  CCList.map
    (fun (label, url) ->
      let is_active =
        active
        |> CCOption.map_or ~default:false (fun active ->
             I18n.equal_nav_link active label)
      in
      let label = Utils.nav_link_to_string language label in
      label, url, is_active)
    links
  |> make_tabs html
;;
