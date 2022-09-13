open Tyxml.Html

let subnav language links base_url active =
  let open Pool_common in
  CCList.map
    (fun (label, url) ->
      let is_active =
        active
        |> CCOption.map_or ~default:false (fun active ->
             I18n.equal_nav_link active label)
      in
      let classnames = [] in
      let link_label = txt (Utils.nav_link_to_string language label) in
      if is_active
      then span ~a:[ a_class ([ "color-primary" ] @ classnames) ] [ link_label ]
      else
        a
          ~a:
            [ a_href
                (Sihl.Web.externalize_path
                   (Format.asprintf "%s/%s" base_url url))
            ; a_class classnames
            ]
          [ link_label ])
    links
  |> nav ~a:[ a_class [ "sub-nav"; "flexrow"; "flex-gap" ] ]
;;
