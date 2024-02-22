open CCFun
open Entity

let create ?active_navigation language links html =
  let open Pool_common in
  CCList.map
    (fun { NavElement.label; url; _ } ->
      let is_active =
        active_navigation
        |> CCOption.map_or ~default:false (flip I18n.equal_nav_link label)
      in
      let label = Utils.nav_link_to_string language label in
      label, url, is_active)
    links
  |> Component.Navigation.make_tabs html
;;
