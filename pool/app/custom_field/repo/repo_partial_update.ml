open Entity
module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

let update_sihl_user pool ?firstname ?lastname contact =
  let open CCOption in
  let ctx = Pool_tenant.to_ctx pool in
  let given_name = firstname <+> contact.Contact.user.Sihl_user.given_name in
  let name = lastname <+> contact.Contact.user.Sihl_user.name in
  Service.User.update ~ctx ?given_name ?name contact.Contact.user
;;

let update_sql column_fragment =
  let base = {sql| UPDATE pool_contacts SET profile_updated_at = $2, |sql} in
  let where_fragment =
    {sql| WHERE user_uuid = UNHEX(REPLACE($1, '-', '')) |sql}
  in
  Format.asprintf "%s %s %s" base column_fragment where_fragment
;;

let upsert_answer_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_custom_field_answers (
      uuid,
      custom_field_uuid,
      entity_uuid,
      value,
      version
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', '')),
      UNHEX(REPLACE($3, '-', '')),
      $4,
      $5
    )
    ON DUPLICATE KEY UPDATE
      value = VALUES(value),
      version = VALUES(version)
    |sql}
  |> Repo_entity_answer.Write.t ->. Caqti_type.unit
;;

let upsert_admin_answer_request =
  let open Caqti_request.Infix in
  {sql|
  INSERT INTO pool_custom_field_answers (
    uuid,
    custom_field_uuid,
    entity_uuid,
    admin_value,
    admin_version
  ) VALUES (
    UNHEX(REPLACE($1, '-', '')),
    UNHEX(REPLACE($2, '-', '')),
    UNHEX(REPLACE($3, '-', '')),
    $4,
    $5
  )
  ON DUPLICATE KEY UPDATE
    admin_value = VALUES(admin_value),
    admin_version = VALUES(admin_version)
  |sql}
  |> Repo_entity_answer.Write.t ->. Caqti_type.unit
;;

let clear_answer_request is_admin =
  let open Caqti_request.Infix in
  let open Pool_common.Repo in
  let where =
    {sql|
    WHERE
      custom_field_uuid = UNHEX(REPLACE($1, '-', ''))
        AND
      entity_uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
  in
  let update =
    match is_admin with
    | false ->
      {sql|
      UPDATE pool_custom_field_answers
        SET
          value = NULL,
          version = version + 1
    |sql}
    | true ->
      {sql|
      UPDATE pool_custom_field_answers
        SET
          admin_value = NULL,
          admin_version = admin_version + 1
    |sql}
  in
  Format.asprintf "%s %s" update where
  |> Caqti_type.(tup2 Id.t Id.t) ->. Caqti_type.unit
;;

let clear_answer pool ~is_admin ~field_id ~entity_uuid () =
  Utils.Database.exec
    (Database.Label.value pool)
    (clear_answer_request is_admin)
    (field_id, entity_uuid)
;;

let map_or ~clear fnc = function
  | Some value -> fnc value
  | None -> clear ()
;;

let value_to_store is_admin answer =
  let open Answer in
  let open CCOption in
  answer
  >>= fun ({ Answer.id; _ } as a) ->
  let pair = CCPair.make id in
  match is_admin with
  | true -> a.admin_value >|= pair
  | false -> a.value >|= pair
;;

let upsert_answer pool user entity_uuid t =
  let is_admin = Pool_context.user_is_admin user in
  let option_id = Entity.SelectOption.Public.show_id in
  let open Entity.Public in
  let field_id = id t in
  let clear = clear_answer pool ~is_admin ~field_id ~entity_uuid in
  let version = version t in
  let update_answer id value =
    let request =
      if is_admin then upsert_admin_answer_request else upsert_answer_request
    in
    Repo_entity_answer.Write.of_entity id field_id entity_uuid value version
    |> Utils.Database.exec (Database.Label.value pool) request
  in
  match t with
  | Boolean (_, answer) ->
    answer
    |> value_to_store is_admin
    |> map_or ~clear (fun (id, value) ->
         update_answer id (Utils.Bool.to_string value))
  | MultiSelect (_, _, answer) ->
    answer
    |> value_to_store is_admin
    |> map_or ~clear (fun (id, value) ->
         value
         |> CCList.map (fun { Entity.SelectOption.Public.id; _ } -> id)
         |> Repo_entity.yojson_of_multi_select_answer
         |> Yojson.Safe.to_string
         |> update_answer id)
  | Number (_, answer) ->
    answer
    |> value_to_store is_admin
    |> map_or ~clear (fun (id, value) ->
         update_answer id (CCInt.to_string value))
  | Select (_, _, answer) ->
    answer
    |> value_to_store is_admin
    |> map_or ~clear (fun (id, value) -> update_answer id (option_id value))
  | Text (_, answer) ->
    answer
    |> value_to_store is_admin
    |> map_or ~clear (fun (id, value) -> update_answer id value)
;;

let update pool user (field : PartialUpdate.t) (contact : Contact.t) =
  let open Entity in
  let base_caqti = Pool_common.Repo.Id.t in
  let dyn =
    Dynparam.empty
    |> Dynparam.add base_caqti (contact |> Contact.id)
    |> Dynparam.add Caqti_type.ptime (Ptime_clock.now ())
  in
  let update_user_table (dyn, sql) =
    let open Caqti_request.Infix in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let update_request = sql |> update_sql |> pt ->. Caqti_type.unit in
    Utils.Database.exec (pool |> Pool_database.Label.value) update_request pv
  in
  let open PartialUpdate in
  match field with
  | Firstname (version, value) ->
    let%lwt (_ : Sihl_user.t) =
      update_sihl_user
        pool
        ~firstname:(value |> Pool_user.Firstname.value)
        contact
    in
    ( dyn |> Dynparam.add Pool_common.Repo.Version.t version
    , {sql|
          firstname_version = $3
        |sql} )
    |> update_user_table
  | Lastname (version, value) ->
    let%lwt (_ : Sihl_user.t) =
      update_sihl_user
        pool
        ~lastname:(value |> Pool_user.Lastname.value)
        contact
    in
    ( dyn |> Dynparam.add Pool_common.Repo.Version.t version
    , {sql|
            lastname_version = $3
          |sql} )
    |> update_user_table
  | Paused (version, value) ->
    ( dyn
      |> Dynparam.add Caqti_type.bool (value |> Pool_user.Paused.value)
      |> Dynparam.add Pool_common.Repo.Version.t version
    , {sql|
              paused = $3,
              paused_version = $4
            |sql}
    )
    |> update_user_table
  | Language (version, value) ->
    ( dyn
      |> Dynparam.add Caqti_type.(option Pool_common.Repo.Language.t) value
      |> Dynparam.add Pool_common.Repo.Version.t version
    , {sql|
                language = $3,
                language_version = $4
              |sql}
    )
    |> update_user_table
  | Custom field -> (upsert_answer pool user (Contact.id contact)) field
;;
