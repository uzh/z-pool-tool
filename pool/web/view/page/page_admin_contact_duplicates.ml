open Tyxml.Html
module Input = Component.Input
module Icon = Component.Icon
module Field = Pool_message.Field
module Url = Http_utils.Url.Admin

let list { Pool_context.language; _ } ?contact (possible_duplicates, query) =
  let open Contact in
  let open Duplicate_contacts in
  let url =
    Uri.of_string
    @@
    match contact with
    | Some contact -> Url.contact_duplicate_path (id contact) ()
    | None -> Url.duplicate_path ()
  in
  let data_table =
    Component.DataTable.create_meta ?filter:filterable_by url query language
  in
  let button id =
    Http_utils.Url.Admin.duplicate_path ~id ()
    |> Input.link_as_button ~icon:Icon.Eye
  in
  let cols =
    let base = [ `column column_score; `empty ] in
    let field_to_string =
      Pool_common.Utils.field_to_string_capitalized language
    in
    if CCOption.is_some contact
    then `custom (field_to_string Field.Contact |> txt) :: base
    else
      [ `custom (field_to_string Field.name |> txt)
      ; `custom (field_to_string Field.name |> txt)
      ]
      @ base
  in
  let row ({ id; contact_a; contact_b; score; ignored; _ } : t) =
    let attrs =
      if Ignored.value ignored then [ a_class [ "bg-red-lighter" ] ] else []
    in
    let contact_link contact =
      a
        ~a:
          [ a_href
              (Http_utils.Url.Admin.contact_path ~id:(Contact.id contact) ())
          ]
        [ txt (fullname contact) ]
    in
    let base = [ CCFloat.to_string score |> txt; button id ] in
    let columns =
      match contact with
      | Some contact ->
        let duplicate =
          let open Contact in
          if Id.equal (id contact_a) (id contact) then contact_b else contact_a
        in
        [ contact_link duplicate ]
      | None -> [ contact_link contact_a; contact_link contact_b ]
    in
    columns @ base |> CCList.map CCFun.(CCList.return %> td) |> tr ~a:attrs
  in
  Component.DataTable.make
    ~target_id:"duplicate-list"
    ~cols
    ~row
    data_table
    possible_duplicates
;;

let index ({ Pool_context.language; _ } as context) ?contact possible_duplicates
  =
  let title =
    match contact with
    | None ->
      Pool_common.(Utils.nav_link_to_string language I18n.ManageDuplicates)
    | Some contact -> Contact.fullname contact
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 [ txt title ]; list context ?contact possible_duplicates ]
;;

let show
  { Pool_context.language; user; csrf; _ }
  fields
  (contact_a, fields_a)
  (contact_b, fields_b)
  duplicate
  =
  let open Duplicate_contacts in
  let is_merge = Ignored.value duplicate.ignored |> not in
  let title =
    Pool_common.(
      Utils.control_to_string
        language
        Pool_message.(Control.Manage Field.Duplicate))
  in
  let highlighted = [ a_class [ "bg-red-lighter" ] ] in
  let label = label ~a:[ a_class [ "flexrow"; "flex-gap" ] ] in
  let field_to_string =
    Pool_common.Utils.field_to_string_capitalized language
  in
  let make_radio ~name contact =
    let value = Contact.(id contact |> Id.value) in
    input
      ~a:[ a_name name; a_value value; a_input_type `Radio; a_required () ]
      ()
  in
  let path ?suffix () =
    Http_utils.Url.Admin.duplicate_path ~id:duplicate.id ?suffix ()
    |> Sihl.Web.externalize_path
  in
  let title =
    if is_merge
    then (
      let ignore_btn =
        form
          ~a:[ a_action (path ~suffix:"ignore" ()); a_method `Post ]
          Input.
            [ csrf_element csrf ()
            ; submit_element
                ~submit_type:`Success
                language
                Pool_message.Control.(Ignore (Some Field.Duplicate))
                ()
            ]
      in
      div
        ~a:
          [ a_class [ "flexrow"; "justify-between"; "align-center"; "flex-gap" ]
          ]
        [ div [ h1 [ txt title ] ]; ignore_btn ])
    else
      h1
        ~a:[ a_class [ "has-icon" ] ]
        [ txt title; Icon.(to_html CheckmarkCircleOutline) ]
  in
  let field_rows =
    let open Custom_field in
    fields
    |> CCList.map (fun custom_field ->
      let radio = make_radio ~name:(id custom_field |> Id.value) in
      let head = name_value language custom_field in
      let find = CCList.find_opt (fun f -> Public.id f = id custom_field) in
      let to_html =
        CCOption.map_or
          ~default:(txt "")
          (Component.CustomField.answer_to_html user language)
      in
      let field_a = find fields_a in
      let field_b = find fields_b in
      let attrs =
        let equal =
          match field_a, field_b with
          | Some a, Some b -> Public.equal_answer a b
          | _, _ -> false
        in
        if equal then highlighted else []
      in
      let cells =
        if is_merge
        then
          [ td [ label [ radio contact_a; to_html field_a ] ]
          ; td [ label [ radio contact_b; to_html field_b ] ]
          ]
        else [ td [ to_html field_a ]; td [ to_html field_b ] ]
      in
      th [ txt head ] :: cells |> tr ~a:attrs)
  in
  let cells =
    let open Pool_user in
    let open Contact in
    let open CCFun.Infix in
    let map_or = CCOption.map_or in
    let default = "" in
    Pool_message.
      [ Field.Id, id %> Id.value
      ; Field.Firstname, firstname %> Firstname.value
      ; Field.Lastname, lastname %> Lastname.value
      ; Field.EmailAddress, email_address %> EmailAddress.value
      ; Field.CellPhone, cell_phone %> map_or ~default CellPhone.value
      ; ( Field.Language
        , fun c -> c.language |> map_or ~default Pool_common.Language.show )
      ]
  in
  let table =
    let rows =
      cells
      |> CCList.map (fun (field, fnc) ->
        let value_a = fnc contact_a in
        let value_b = fnc contact_b in
        let attr =
          if value_a = value_b && value_a != "" then highlighted else []
        in
        let cells =
          if is_merge
          then (
            let radio = make_radio ~name:(Pool_message.Field.show field) in
            [ td [ label [ radio contact_a; span [ txt value_a ] ] ]
            ; td [ label [ radio contact_b; span [ txt value_b ] ] ]
            ])
          else [ td [ txt value_a ]; td [ txt value_b ] ]
        in
        th [ txt (field_to_string field) ] :: cells |> tr ~a:attr)
    in
    rows @ field_rows |> table ~a:[ a_class [ "table"; "striped" ] ]
  in
  let body =
    if is_merge
    then
      div
        [ p
            [ Pool_common.(
                Utils.hint_to_string language I18n.MergeContacts
                |> Http_utils.add_line_breaks)
            ]
        ; form
            ~a:
              [ a_method `Post
              ; a_action
                  (Http_utils.Url.Admin.duplicate_path
                     ~suffix:"merge"
                     ~id:duplicate.id
                     ()
                   |> Sihl.Web.externalize_path)
              ]
            [ Input.csrf_element csrf ()
            ; table
            ; div
                ~a:[ a_class [ "gap"; "flexrow"; "justify-end" ] ]
                [ Input.submit_element
                    language
                    Pool_message.Control.(Save None)
                    ()
                ]
            ]
        ]
    else table
  in
  div ~a:[ a_class [ "trim"; "safety-margin" ] ] [ title; body ]
;;
