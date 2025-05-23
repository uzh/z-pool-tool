open CCFun
open Tyxml.Html
open Entity
module CommonUtils = Pool_common.Utils

let filter_items ?(any_id = true) ?validate ?actor ?(guardian = []) items =
  let open Guard in
  match validate, actor with
  | (None | Some false), _ -> items
  | Some true, None -> []
  | Some true, Some actor ->
    let rec filter_nav items =
      CCList.filter_map
        (fun ({ NavElement.validation; children; _ } as element) ->
           let with_children () =
             let children = filter_nav children in
             Some NavElement.{ element with children }
           in
           match validation with
           | AlwaysOn -> with_children ()
           | OnChildren ->
             (match filter_nav children with
              | [] -> None
              | children -> Some NavElement.{ element with children })
           | Set validation_set ->
             (try
                let self =
                  Persistence.PermissionOnTarget.validate_set
                    ~any_id
                    guardian
                    Pool_message.Error.authorization
                    validation_set
                    actor
                in
                match self with
                | Ok () when CCList.is_empty children -> Some element
                | Ok () -> with_children ()
                | Error _ -> None
              with
              | _ -> None))
        items
    in
    filter_nav items
;;

let rec build_nav_links
          ?(layout = Horizonal)
          ?active_navigation
          ?(first_level = true)
          language
          query_params
          { NavElement.url; label; icon; children; _ }
  =
  let rec find_is_active (children : NavElement.t list_wrap) : bool =
    let is_active url =
      active_navigation |> CCOption.map_or ~default:false (CCString.equal url)
    in
    CCList.fold_left
      (fun init { NavElement.url; children; _ } ->
         match url with
         | None -> init || find_is_active children
         | Some url -> init || is_active url || find_is_active children)
      (CCOption.map_or ~default:false is_active url)
      children
  in
  let is_active = find_is_active children in
  let label =
    let label_elt = label |> CommonUtils.nav_link_to_string language |> txt in
    CCOption.map_or
      ~default:[ label_elt ]
      (fun icon ->
         [ span ~a:[ a_class [ "has-icon" ] ] [ Icon.to_html icon; span [ label_elt ] ] ])
      icon
  in
  let nav_link : [< Html_types.li_content_fun ] elt list_wrap =
    let classnames = [ "nav-link" ] in
    match is_active, url with
    | true, _ | false, None -> [ span ~a:[ a_class classnames ] label ]
    | _, Some url ->
      [ a
          ~a:
            [ a_href (Http_utils.externalize_path_with_params query_params url)
            ; a_class classnames
            ]
          label
      ]
  in
  match children with
  | [] -> if is_active then li ~a:[ a_class [ "active" ] ] nav_link else li nav_link
  | children ->
    let parent_attrs, list_attrs =
      match layout with
      | Vertical -> [], [ a_class [ "children" ] ]
      | Horizonal ->
        let parent_class =
          if first_level then [ "has-dropdown" ] else [ "has-dropdown"; "right" ]
        in
        [ a_class parent_class ], [ a_class [ "dropdown" ] ]
    in
    let build_rec =
      CCList.map
        (build_nav_links
           ~layout
           ~first_level:false
           ?active_navigation
           language
           query_params)
      %> ul ~a:list_attrs
    in
    nav_link @ [ build_rec children ] |> li ~a:parent_attrs
;;

let create_nav
      { Pool_context.query_parameters; language; guardian; _ }
      items
      ?any_id
      ?validate
      ?actor
      ?active_navigation
      layout
  =
  let nav_links =
    filter_items ?any_id ?validate ?actor ~guardian items
    |> CCList.map (build_nav_links ~layout ?active_navigation language query_parameters)
  in
  [ nav ~a:[ a_class [ "main-nav" ] ] [ ul nav_links ] ]
;;

let i18n_links languages query_parameters active_language layout =
  let open Pool_message in
  let field = Field.Language in
  let link_classes = [ "nav-link" ] in
  let nav_class =
    match layout with
    | Vertical -> []
    | Horizonal -> [ "secondary-nav"; "inset-lg"; "left" ]
  in
  let to_html language =
    let lang = Language.show language in
    let query_parameters =
      (field, lang) :: CCList.remove_assoc ~eq:Field.equal field query_parameters
    in
    if Language.equal language active_language
    then li ~a:[ a_class [ "active" ] ] [ span ~a:[ a_class link_classes ] [ txt lang ] ]
    else
      li
        [ a
            ~a:
              [ a_href (Http_utils.url_with_field_params query_parameters "")
              ; a_class link_classes
              ]
            [ txt lang ]
        ]
  in
  [ languages |> CCList.map to_html |> ul ~a:[ a_class nav_class ] ] |> nav
;;

let create_nav_with_language_switch
      ({ Pool_context.language; query_parameters; _ } as context)
      elements
      available_languages
      ?actor:_
      ?active_navigation
      layout
  =
  let language_switch = i18n_links available_languages query_parameters language in
  let main = create_nav ~validate:false ?active_navigation context elements layout in
  main @ [ language_switch layout ]
;;

let create_desktop_nav fcn =
  fcn Horizonal
  |> div ~a:[ a_class [ "hidden-mobile"; "flexrow"; "flex-gap-lg"; "align-center" ] ]
;;

let create_mobile_nav ~toggle_id ?title navigation =
  let overlay navigation =
    let title =
      title
      |> CCOption.map_or ~default:(txt "") (fun title ->
        h2 ~a:[ a_class [ "word-wrap-break" ] ] [ txt title ])
    in
    div
      ~a:[ a_id toggle_id; a_class [ "mobile-nav"; "bg-white" ] ]
      [ div ~a:[ a_class [ "mobile-nav-inner"; "fade-in" ] ] (title :: navigation) ]
  in
  navigation Vertical |> overlay
;;

let make_mobile_nav_open_toggle toggle_id =
  let open Icon in
  span
    ~a:
      [ a_class [ "icon-lg" ]
      ; a_user_data "action" "open"
      ; a_user_data "overlay-nav" toggle_id
      ]
    [ to_html MenuOutline ]
;;

let make_mobile_header navigation =
  let toggle_id = "mobile-nav" in
  let icons =
    let open Icon in
    [ make_mobile_nav_open_toggle toggle_id
    ; span
        ~a:
          [ a_user_data "action" "close"
          ; a_class [ "icon-lg"; "hidden" ]
          ; a_aria "hidden" [ "true" ]
          ]
        [ to_html Close ]
    ]
    |> div ~a:[ a_id "mobile-nav-toggle" ]
  in
  div
    ~a:[ a_class [ "push"; "mobile-only" ] ]
    [ icons; create_mobile_nav ~toggle_id navigation ]
;;
