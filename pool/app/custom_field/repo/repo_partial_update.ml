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

let clear_answer_request =
  let open Caqti_request.Infix in
  let open Pool_common.Repo in
  {sql|
    UPDATE pool_custom_field_answers
      SET
        value = NULL,
        version = version + 1
    WHERE
      custom_field_uuid = UNHEX(REPLACE($1, '-', ''))
    AND
      entity_uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
  |> Caqti_type.(tup2 Id.t Id.t) ->. Caqti_type.unit
;;

let clear_answer pool field_id entity_uuid () =
  Utils.Database.exec
    (Database.Label.value pool)
    clear_answer_request
    (field_id, entity_uuid)
;;

let map_or ~clear fnc = function
  | Some value -> fnc value
  | None -> clear ()
;;

let upsert_answer pool entity_uuid t =
  let option_id = Entity.SelectOption.Public.show_id in
  let open Entity.Public in
  let field_id = id t in
  let clear = clear_answer pool field_id entity_uuid in
  let version = version t in
  let update_answer id value =
    Repo_entity_answer.Write.of_entity id field_id entity_uuid value version
    |> Utils.Database.exec (Database.Label.value pool) upsert_answer_request
  in
  let open Entity.Answer in
  match t with
  | Boolean (_, answer) ->
    answer
    |> map_or ~clear (fun { id; value; _ } ->
         update_answer id (Utils.Bool.to_string value))
  | MultiSelect (_, _, answer) ->
    answer
    |> map_or ~clear (fun { id; value; _ } ->
         value
         |> CCList.map (fun { Entity.SelectOption.Public.id; _ } -> id)
         |> fun ids ->
         Repo_entity.yojson_of_multi_select_answer ids
         |> Yojson.Safe.to_string
         |> update_answer id)
  | Number (_, answer) ->
    answer
    |> map_or ~clear (fun { id; value; _ } ->
         update_answer id (CCInt.to_string value))
  | Select (_, _, answer) ->
    answer
    |> map_or ~clear (fun { id; value; _ } ->
         update_answer id (value |> option_id))
  | Text (_, answer) ->
    answer |> map_or ~clear (fun { id; value; _ } -> update_answer id value)
;;

let update pool (field : PartialUpdate.t) (contact : Contact.t) =
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
  | Custom field -> (upsert_answer pool (Contact.id contact)) field
;;
