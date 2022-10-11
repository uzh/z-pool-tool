open Tyxml.Html

let make_subnav links =
  CCList.map
    (fun (label, url, active) ->
      if active
      then span ~a:[ a_class [ "color-primary" ] ] [ txt label ]
      else a ~a:[ a_href (Sihl.Web.externalize_path url) ] [ txt label ])
    links
  |> nav ~a:[ a_class [ "sub-nav"; "flexrow"; "flex-gap" ] ]
;;

let subnav language links base_url active =
  let open Pool_common in
  CCList.map
    (fun (label, url) ->
      let is_active =
        active
        |> CCOption.map_or ~default:false (fun active ->
             I18n.equal_nav_link active label)
      in
      let label = Utils.nav_link_to_string language label in
      let url = Format.asprintf "%s/%s" base_url url in
      label, url, is_active)
    links
  |> make_subnav
;;
