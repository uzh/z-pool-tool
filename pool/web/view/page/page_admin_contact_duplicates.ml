open Tyxml.Html
module Input = Component.Input
module Icon = Component.Icon

let index contact possible_duplicates =
  let open Duplicate_contacts in
  let open Contact in
  let button dup_id =
    Http_utils.Url.Admin.contact_duplicate_path (id contact) ~id:dup_id ()
    |> Input.link_as_button ~icon:Icon.Eye
  in
  let table =
    possible_duplicates
    |> CCList.map (fun { id; contact; score; _ } ->
      [ a
          ~a:
            [ a_href
                (Http_utils.Url.Admin.contact_path ~id:(Contact.id contact) ())
            ]
          [ txt (fullname contact) ]
      ; CCFloat.to_string score |> txt
      ; button id
      ]
      |> CCList.map (fun html -> td [ html ])
      |> tr)
    |> table ~a:[ a_class [ "table"; "table-striped" ] ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 [ txt (fullname contact) ]; table ]
;;

let show
  ?(merge = true)
  { Pool_context.language; user; _ }
  (target_contact, target_fields)
  (duplicate, duplicate_fields)
  =
  let title =
    Pool_common.(
      Utils.control_to_string
        language
        Pool_message.(Control.Manage Field.Duplicate))
  in
  let highlighted = [ a_class [ "bg-red-lighter" ] ] in
  let label = label ~a:[ a_class [ "flexrow"; "flex-gap" ] ] in
  let duplicate_contact = duplicate.Duplicate_contacts.contact in
  let field_to_string =
    Pool_common.Utils.field_to_string_capitalized language
  in
  let make_radio ~name contact =
    let value = Contact.(id contact |> Id.value) in
    input
      ~a:[ a_name name; a_value value; a_input_type `Radio; a_required () ]
      ()
  in
  let field_rows =
    let open Custom_field in
    target_fields
    |> CCList.map (fun target_field ->
      let radio = make_radio ~name:(Public.id target_field |> Id.value) in
      let head = Public.name_value language target_field in
      let find = CCList.find (fun f -> Public.id f = Public.id target_field) in
      let to_html = Component.CustomField.answer_to_html user language in
      let duplicate_field = find duplicate_fields in
      let attrs =
        if Public.equal_answer target_field duplicate_field
        then highlighted
        else []
      in
      let cells =
        if merge
        then
          [ td [ label [ radio target_contact; to_html target_field ] ]
          ; td [ label [ radio duplicate_contact; to_html duplicate_field ] ]
          ]
        else [ td [ to_html target_field ]; td [ to_html duplicate_field ] ]
      in
      th [ txt head ] :: cells |> tr ~a:attrs)
  in
  let cells =
    let open Pool_user in
    let open Contact in
    let open CCFun.Infix in
    let map_or = CCOption.map_or ~default:"" in
    Pool_message.
      [ Field.(Firstname), firstname %> Firstname.value
      ; Field.(Lastname), lastname %> Lastname.value
      ; Field.(EmailAddress), email_address %> EmailAddress.value
      ; Field.(CellPhone), cell_phone %> map_or CellPhone.value
      ]
  in
  let table =
    let id_row =
      let cell contact = td [ txt Contact.(id contact |> Id.value) ] in
      [ th [ txt (field_to_string Pool_message.Field.Id) ]
      ; cell target_contact
      ; cell duplicate_contact
      ]
      |> tr
    in
    let rows =
      cells
      |> CCList.map (fun (field, fnc) ->
        let target_value = fnc target_contact in
        let duplicate_value = fnc duplicate_contact in
        let attr =
          if target_value = duplicate_value && target_value != ""
          then highlighted
          else []
        in
        let cells =
          if merge
          then (
            let radio = make_radio ~name:(Pool_message.Field.show field) in
            [ td [ label [ radio target_contact; txt target_value ] ]
            ; td [ label [ radio duplicate_contact; txt duplicate_value ] ]
            ])
          else [ td [ txt target_value ]; td [ txt duplicate_value ] ]
        in
        th [ txt (field_to_string field) ] :: cells |> tr ~a:attr)
    in
    (id_row :: rows) @ field_rows |> table ~a:[ a_class [ "table"; "striped" ] ]
  in
  let body =
    if merge
    then
      form
        ~a:[ a_method `Post; a_action "#" ]
        [ p
            [ Pool_common.(
                Utils.hint_to_string language I18n.MergeContacts
                |> Http_utils.add_line_breaks)
            ]
        ; table
        ; div
            ~a:[ a_class [ "gap"; "flexrow"; "justify-end" ] ]
            [ Input.submit_element language Pool_message.Control.(Save None) ()
            ]
        ]
    else table
  in
  div ~a:[ a_class [ "trim"; "safety-margin" ] ] [ h1 [ txt title ]; body ]
;;
