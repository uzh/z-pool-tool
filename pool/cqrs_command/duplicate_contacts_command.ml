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
    -> Custom_field.t list
    -> Custom_field.Public.t list * Custom_field.Public.t list
    -> (Duplicate_contacts.merge, Pool_message.Error.t) result
end = struct
  let handle
        ?(tags = Logs.Tag.empty)
        urlencoded
        { contact_a; contact_b; _ }
        custom_fields
        (fields_a, fields_b)
    =
    Logs.info ~src (fun m -> m "Handle command Merge" ~tags);
    let open CCResult.Infix in
    let open Pool_message in
    let open Duplicate_contacts in
    let select_contact_by_id id =
      let id = Contact.Id.of_string id in
      if Contact.Id.equal id (Contact.id contact_a)
      then contact_a
      else contact_b
    in
    let select_fields id =
      let id = Contact.Id.of_string id in
      if Contact.Id.equal id (Contact.id contact_a) then fields_a else fields_b
    in
    let* selected_contact =
      let open CCOption in
      CCList.assoc_opt ~eq:( = ) Field.(show EmailAddress) urlencoded
      >>= CCList.head_opt
      |> map select_contact_by_id
      |> to_result (Pool_message.Error.NotFound Field.EmailAddress)
    in
    let merged_contact =
      let email =
        CCFun.(Contact.email_address %> Pool_user.EmailAddress.value)
      in
      if CCString.equal (email selected_contact) (email contact_a)
      then contact_b
      else contact_a
    in
    let* hardcoded =
      let open CCOption in
      CCList.map
        (fun (field, create) ->
           CCList.assoc_opt ~eq:( = ) (Field.show field) urlencoded
           >>= CCList.head_opt
           |> to_result (Pool_message.Error.NotFound field)
           |> Result.map select_contact_by_id
           |> CCResult.map create)
        read_hardcoded
      |> CCList.all_ok
    in
    let* custom_fields =
      let open CCResult in
      let open Custom_field in
      let to_result = CCOption.to_result in
      let error = Pool_message.(Error.NotFound Field.CustomFieldAnswer) in
      CCList.map
        (fun (field : t) ->
           let id = id field in
           CCOption.bind
             (CCList.assoc_opt ~eq:( = ) (Id.value id) urlencoded)
             CCList.head_opt
           |> to_result error
           >|= select_fields
           >>= fun fields ->
           CCList.find_opt (fun f -> Id.equal (Public.id f) id) fields
           (* TODO: An empty answer should be crated, not found *)
           |> to_result error)
        custom_fields
      |> CCList.all_ok
    in
    let open Contact in
    let selected_contact =
      CCList.fold_left
        (fun contact field ->
           match field with
           | Lastname name -> set_lastname contact name
           | Firstname name -> set_firstname contact name
           | Language language -> set_language contact language
           | CellPhone cell_phone -> set_cellphone contact cell_phone)
        selected_contact
        hardcoded
    in
    Ok
      Duplicate_contacts.
        { contact = selected_contact
        ; merged_contact
        ; kept_fields = custom_fields
        }
  ;;
end
