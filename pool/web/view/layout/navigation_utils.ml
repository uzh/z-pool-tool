open CCFun
open Tyxml.Html
open Entity
module CommonUtils = Pool_common.Utils

let filter_items ?validate ?actor database_label items =
  let open Guard in
  match validate, actor with
  | (None | Some false), _ -> Lwt.return items
  | Some true, None -> Lwt.return []
  | Some true, Some actor ->
    let rec filter_nav items =
      Lwt_list.filter_map_s
        (fun ({ Element.validation_set; children; _ } as element) ->
          try
            let%lwt self =
              Persistence.validate
                ~any_id:true
                database_label
                validation_set
                actor
            in
            match self with
            | Ok () when CCList.is_empty children -> Lwt.return_some element
            | Ok () ->
              let%lwt children = filter_nav children in
              Lwt.return_some Element.{ element with children }
            | Error _ -> Lwt.return_none
          with
          | _ -> Lwt.return_none)
        items
    in
    filter_nav items
;;

let rec build_nav_links
  ?(mobile = false)
  ?active_navigation
  language
  query_language
  { Element.url; label; icon; children; _ }
  =
  let rec find_is_active (children : Element.t list_wrap) : bool =
    let is_active url =
      active_navigation |> CCOption.map_or ~default:false (CCString.equal url)
    in
    CCList.fold_left
      (fun init { Element.url; children; _ } ->
        init || is_active url || find_is_active children)
      (is_active url)
      children
  in
  let is_active = find_is_active children in
  let label =
    let label_elt = label |> CommonUtils.nav_link_to_string language |> txt in
    CCOption.map_or
      ~default:[ label_elt ]
      (fun icon ->
        [ span
            ~a:[ a_class [ "has-icon" ] ]
            [ Icon.to_html icon; span [ label_elt ] ]
        ])
      icon
  in
  let nav_link : [< Html_types.li_content_fun ] elt list_wrap =
    let classnames =
      let base = [ "nav-link" ] in
      if is_active then "active" :: base else base
    in
    if is_active || CCList.is_empty children |> not
    then [ span ~a:[ a_class classnames ] label ]
    else
      [ a
          ~a:
            [ a_href (Http_utils.externalize_path_with_lang query_language url)
            ; a_class classnames
            ]
          label
      ]
  in
  match children with
  | [] -> li nav_link
  | children ->
    let parent_attrs, list_attrs =
      (* NOTE: Desktop UI only supports one nested navigation group *)
      if mobile
      then [], [ a_class [ "children" ] ]
      else [ a_class [ "has-dropdown" ] ], [ a_class [ "dropdown" ] ]
    in
    let build_rec =
      CCList.map
        (build_nav_links ~mobile ?active_navigation language query_language)
      %> ul ~a:list_attrs
    in
    nav_link @ [ build_rec children ] |> li ~a:parent_attrs
;;

let create_main
  items
  ?validate
  ?actor
  ?active_navigation
  database_label
  language
  query_language
  mobile
  =
  let open Utils.Lwt_result.Infix in
  let%lwt nav_links =
    filter_items ?validate ?actor database_label items
    ||> CCList.map
          (build_nav_links ~mobile ?active_navigation language query_language)
  in
  let nav = [ nav ~a:[ a_class [ "main-nav" ] ] [ ul nav_links ] ] in
  Lwt.return
  @@
  if mobile
  then [ div ~a:[ a_class [ "grow"; "flexcolumn"; "justify-center" ] ] nav ]
  else nav
;;

let i18n_links languages active_language mobile =
  let open Pool_common.Message in
  let link_classes = [ "nav-link" ] in
  let nav_class =
    if mobile
    then [ "language-nav"; "gap"; "flexrow"; "flex-gap"; "justify-center" ]
    else [ "main-nav" ]
  in
  let to_html =
    (fun language ->
      let lang = Language.show language in
      if Language.equal language active_language
      then span ~a:[ a_class ("active" :: link_classes) ] [ txt lang ]
      else (
        let query_param =
          [ Field.Language, lang |> CCString.lowercase_ascii ]
        in
        a
          ~a:
            [ a_href (add_field_query_params "" query_param)
            ; a_class link_classes
            ]
          [ txt lang ]))
    |> CCList.map
  in
  languages |> to_html |> nav ~a:[ a_class nav_class ]
;;

let with_language_switch
  elements
  available_languages
  ?actor:_
  ?active_navigation
  database_label
  active_language
  query_language
  mobile
  =
  let open Utils.Lwt_result.Infix in
  let language_switch = i18n_links available_languages active_language in
  let create_main items =
    create_main
      items
      ~validate:false
      ?active_navigation
      database_label
      active_language
      query_language
  in
  create_main elements mobile ||> CCList.cons (language_switch mobile)
;;

let create_desktop fcn =
  let open Utils.Lwt_result.Infix in
  fcn false ||> div ~a:[ a_class [ "desktop-nav"; "flexrow"; "flex-gap" ] ]
;;

let create_mobile app_title navigation =
  let open Utils.Lwt_result.Infix in
  let id = "navigation-overlay" in
  let label =
    Icon.to_html
    %> CCList.pure
    %> div ~a:[ a_user_data "modal" id; a_class [ "icon-lg" ] ]
  in
  let overlay navigation =
    div
      ~a:[ a_id id; a_class [ "fullscreen-overlay"; "mobile-nav"; "bg-white" ] ]
      [ div
          ~a:[ a_class [ "flexcolumn"; "full-height" ] ]
          [ header
              ~a:[ a_class [ "flexrow"; "justify-between"; "align-center" ] ]
              [ app_title; label Icon.Close ]
          ; div
              ~a:[ a_class [ "fade-in"; "inset"; "flexcolumn"; "grow" ] ]
              navigation
          ]
      ]
  in
  navigation true
  ||> fun items ->
  div
    ~a:[ a_class [ "mobile-nav-wrapper" ] ]
    [ label Icon.MenuOutline; overlay items ]
;;
