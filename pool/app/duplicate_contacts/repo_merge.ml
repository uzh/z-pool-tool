open Entity

let id_select_fragment = Pool_common.Id.sql_select_fragment
let id_value_fragment = Pool_common.Id.sql_value_fragment

let merge pool { contact; merged_contact; kept_fields } =
  let _ = merged_contact in
  let update_contact connection =
    let open Contact in
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec Repo.update_request (to_write contact)
  in
  let update_user connection =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec Pool_user.Repo.update_request contact.Contact.user
  in
  let store_custom_answers =
    let open Custom_field in
    let entity_uuid = Contact.(id contact |> Id.to_common) in
    kept_fields
    |> CCList.map (fun field connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      Repo.override_answer ~entity_uuid field
      |> function
      | `Clear (req, arg) -> Connection.exec req arg
      | `Override (req, arg) -> Connection.exec req arg)
  in
  Database.transaction_iter
    pool
    ([ update_contact; update_user ] @ store_custom_answers)
  |> Lwt_result.ok
;;
