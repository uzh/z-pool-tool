open Tyxml.Html

let mail_to_html ?(highlight_first_line = true) mail =
  let open Pool_location.Address.Mail in
  let { institution; room; building; street; zip; city } = mail in
  let building_room =
    match building with
    | Some building ->
      Format.asprintf "%s %s" (room |> Room.value) (building |> Building.value)
    | None -> room |> Room.value
  in
  let city_zip =
    Format.asprintf "%s %s" (city |> City.value) (zip |> Zip.value)
  in
  let base = [ building_room; street |> Street.value; city_zip ] in
  (match institution with
   | Some institution -> CCList.cons (institution |> Institution.value) base
   | None -> base)
  |> CCList.foldi
       (fun html index str ->
         let str = str |> txt in
         match index with
         | 0 ->
           CCList.pure (if highlight_first_line then strong [ str ] else str)
         | _ -> html @ [ br (); str ])
       []
  |> span
;;

let address_to_html
  ?(highlight_first_line = true)
  language
  (location_address : Pool_location.Address.t)
  =
  let open Pool_location.Address in
  match location_address with
  | Virtual ->
    [ txt
        (Pool_common.(
           Utils.field_to_string language Pool_common.Message.Field.Virtual)
        |> CCString.capitalize_ascii)
    ]
    |> fun html ->
    (match highlight_first_line with
     | true -> strong html
     | false -> span html)
  | Physical mail -> mail_to_html ~highlight_first_line mail
;;

let location_to_html ?(public = false) language (location : Pool_location.t) =
  let open Pool_location in
  let title =
    [ strong [ txt (location.name |> Name.show) ] ] |> p |> CCOption.pure
  in
  let address =
    [ address_to_html ~highlight_first_line:false language location.address ]
    |> p
    |> CCOption.pure
  in
  let status =
    match public with
    | true -> None
    | false ->
      [ span
          [ txt
              (Format.asprintf
                 "%s: %s"
                 (Pool_common.(
                    Utils.field_to_string language Message.Field.Status)
                 |> CCString.capitalize_ascii)
                 (location.status |> Status.show))
          ]
      ]
      |> p
      |> CCOption.pure
  in
  let link =
    CCOption.map
      (fun l ->
        p
          [ a
              ~a:[ a_href (l |> Link.value); a_target "_blank" ]
              [ txt (l |> Link.value) ]
          ])
      location.link
  in
  [ title; address; status; link ]
  |> CCList.filter_map CCFun.id
  |> div ~a:[ a_class [ "stack-sm" ] ]
;;

let form_title ?(level = `H1) language field m =
  let open Pool_common in
  let text =
    let open Message in
    (if CCOption.is_none m then Create (Some field) else Update (Some field))
    |> Utils.control_to_string language
    |> txt
  in
  match level with
  | `H1 -> h1 ~a:[ a_class [ "heading-1" ] ] [ text ]
  | `H2 -> h1 ~a:[ a_class [ "heading-2" ] ] [ text ]
;;

let terms_and_conditions_label language id =
  let open Pool_common.Language in
  let terms lang =
    Pool_common.(Utils.field_to_string lang Message.Field.TermsAndConditions)
    |> txt
    |> CCList.pure
    |> a ~a:[ a_href "#"; a_user_data "modal" id ]
  in
  match language with
  | En -> [ txt "I have read the "; terms En; txt " and I agree with them. *" ]
  | De ->
    [ txt "Ich habe die "
    ; terms De
    ; txt " gelesen und bin damit einverstanden. *"
    ]
;;
