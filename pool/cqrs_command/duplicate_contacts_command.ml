open Duplicate_contacts

let src = Logs.Src.create "duplicate_contacts.cqrs"

module Ignore : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  let handle ?(tags = Logs.Tag.empty) t =
    Logs.info ~src (fun m -> m "Handle command Ignore" ~tags);
    Ok [ Ignored t |> Pool_event.duplicate_contacts ]
  ;;
end

module Merge : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> (string * string list) list
    -> t
    -> Custom_field.Public.t list * Custom_field.Public.t list
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  let handle ?(tags = Logs.Tag.empty) urlencoded duplicate (fields_a, fields_b) =
    Logs.info ~src (fun m -> m "Handle command Merge" ~tags);
    let open CCResult.Infix in
    let open Pool_message in
    let open Duplicate_contacts in
    let select_contact id =
      let id = Contact.Id.of_string id in
      if Contact.Id.equal id (Contact.id duplicate.contact_a)
      then duplicate.contact_a
      else duplicate.contact_b
    in
    let select_fields id =
      let id = Contact.Id.of_string id in
      if Contact.Id.equal id (Contact.id duplicate.contact_a)
      then fields_a
      else fields_b
    in
    let* selected_contact =
      let open CCOption in
      CCList.assoc_opt ~eq:( = ) Field.(show Id) urlencoded
      >>= CCList.head_opt
      |> map select_contact
      |> to_result (Pool_message.Error.NotFound Field.Id)
    in
    let* hardcoded =
      let open CCOption in
      CCList.map
        (fun (field, create) ->
          CCList.assoc_opt ~eq:( = ) (Field.show field) urlencoded
          >>= CCList.head_opt
          |> to_result (Pool_message.Error.NotFound field)
          |> Result.map select_contact
          |> CCResult.map create)
        read_hardcoded
      |> CCList.all_ok
    in
    let* custom_fields =
      let open CCResult in
      let open Custom_field in
      let to_result = CCOption.to_result in
      let error = Pool_message.(Error.NotFound Field.CustomFieldAnswer) in
      let get_id = Public.id in
      CCList.map
        (fun field ->
          let id = get_id field in
          CCOption.bind
            (CCList.assoc_opt ~eq:( = ) (Id.value id) urlencoded)
            CCList.head_opt
          |> to_result error
          >|= select_fields
          >>= fun fields ->
          CCList.find_opt (fun f -> Id.equal (get_id f) (get_id field)) fields
          |> to_result error)
        fields_a
      |> CCList.all_ok
    in
    let open Contact in
    let contact =
      CCList.fold_left
        (fun contact field ->
          match field with
          | Lastname name -> set_lastname contact name
          | Firstname name -> set_firstname contact name
          | EmailAddress email -> set_email_address contact email
          | Language language -> set_language contact language
          | CellPhone cell_phone -> set_cellphone contact cell_phone)
        selected_contact
        hardcoded
    in
    let custom_field_events =
      CCList.map
        (fun field ->
          Custom_field.AnswerOverridden (field, contact)
          |> Pool_event.custom_field)
        custom_fields
    in
    Ok ((Contact.Updated contact |> Pool_event.contact) :: custom_field_events)
  ;;
end
