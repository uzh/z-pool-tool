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
  { Pool_context.language; user; _ }
  (target_contact, target_fields)
  (duplicate, duplicate_fields)
  =
  let duplicate_contact = duplicate.Duplicate_contacts.contact in
  let show_field = Pool_common.Utils.field_to_string_capitalized language in
  let field_cells =
    let open Custom_field in
    target_fields
    |> CCList.map (fun field ->
      let head = Public.name_value language field in
      let answer fields =
        CCList.find_opt (fun f -> Public.id f = Public.id field) fields
        |> CCOption.map_or
             ~default:(txt "")
             (Component.CustomField.answer_to_html user language)
      in
      head, answer)
  in
  let cells : (string * (Contact.t -> uri)) list_wrap =
    let open Pool_user in
    let open Contact in
    let open CCFun.Infix in
    let map_or = CCOption.map_or ~default:"" in
    Pool_message.
      [ Field.(show_field Id), id %> Id.value
      ; Field.(show_field Firstname), firstname %> Firstname.value
      ; Field.(show_field Lastname), lastname %> Lastname.value
      ; Field.(show_field EmailAddress), email_address %> EmailAddress.value
      ; Field.(show_field CellPhone), cell_phone %> map_or CellPhone.value
      ]
  in
  let table =
    let rows =
      cells
      |> CCList.map (fun (head, fnc) ->
        [ th [ txt head ]
        ; td [ fnc target_contact |> txt ]
        ; td [ fnc duplicate_contact |> txt ]
        ]
        |> tr)
    in
    let field_rows =
      field_cells
      |> CCList.map (fun (head, fnc) ->
        [ th [ txt head ]
        ; td [ fnc target_fields ]
        ; td [ fnc duplicate_fields ]
        ]
        |> tr)
    in
    rows @ field_rows |> table ~a:[ a_class [ "table"; "striped" ] ]
  in
  div ~a:[ a_class [ "trim"; "safety-margin" ] ] [ h1 [ txt "HEDLLO" ]; table ]
;;
