open Entity
module Dynparam = Database.Dynparam

let update_user pool ?firstname ?lastname contact =
  User.update pool ?firstname ?lastname contact.Contact.user
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
  Format.asprintf "%s %s" update where |> Caqti_type.(t2 Id.t Id.t ->. unit)
;;

let clear_answer pool ~is_admin ~field_id ~entity_uuid () =
  Database.exec pool (clear_answer_request is_admin) (field_id, entity_uuid)
;;

let map_or ~clear fnc = function
  | Some value -> fnc value
  | None -> clear ()
;;

let value_to_store is_admin answer =
  let open CCOption in
  answer
  >>= fun { Answer.id; admin_value; value; _ } ->
  let pair = CCPair.make id in
  match is_admin with
  | true -> admin_value >|= pair
  | false -> value >|= pair
;;

let upsert_answer pool is_admin entity_uuid t =
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
    |> Database.exec pool request
  in
  match t with
  | Boolean (_, answer) ->
    answer
    |> value_to_store is_admin
    |> map_or ~clear (fun (id, value) ->
      update_answer id (Utils.Bool.to_string value))
  | Date (_, answer) ->
    answer
    |> value_to_store is_admin
    |> map_or ~clear (fun (id, value) ->
      update_answer id (Ptime.date_to_string value))
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

let override_answer_request =
  (* The uuid is generated to make sure the answer is created for the new entity *)
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_custom_field_answers (
      uuid,
      custom_field_uuid,
      entity_uuid,
      value,
      admin_value,
      version
    ) VALUES (
      UNHEX(REPLACE(UUID(), '-', '')),
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', '')),
      $3,
      $4,
      $5
    )
    ON DUPLICATE KEY UPDATE
      value = VALUES(value),
      admin_value = VALUES(admin_value),
      version = VALUES(version)
    |sql}
  |> Repo_entity_answer.Override.t ->. Caqti_type.unit
;;

let clear_answer_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_custom_field_answers
    WHERE entity_uuid = UNHEX(REPLACE($1, '-', ''))
    AND custom_field_uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> Caqti_type.(t2 Pool_common.Repo.Id.t Pool_common.Repo.Id.t)
     ->. Caqti_type.unit
;;

let override_values to_string (answer : 'a Answer.t option) =
  let open Answer in
  let open CCOption.Infix in
  match answer with
  | None -> None
  | Some { value; admin_value; _ } ->
    Some (value >|= to_string, admin_value >|= to_string)
;;

(* TODO: Does not really belong to partial update *)
let override_answer ~entity_uuid (t : Public.t) =
  let field_id = Public.id t in
  let version = Public.version t in
  let override = function
    | None -> `Clear (clear_answer_request, (entity_uuid, field_id))
    | Some (value, admin_value) ->
      let override =
        Repo_entity_answer.Override.of_entity
          field_id
          entity_uuid
          value
          admin_value
          version
      in
      `Override (override_answer_request, override)
  in
  match t with
  | Public.Boolean (_, answer) ->
    let to_string = Utils.Bool.to_string in
    override_values to_string answer |> override
  | Public.Date (_, answer) ->
    let to_string = Ptime.date_to_string in
    override_values to_string answer |> override
  | Public.Number (_, answer) ->
    let to_string = CCInt.to_string in
    override_values to_string answer |> override
  | Public.MultiSelect (_, _, answer) ->
    let to_string options =
      options
      |> CCList.map (fun { Entity.SelectOption.Public.id; _ } -> id)
      |> Repo_entity.yojson_of_multi_select_answer
      |> Yojson.Safe.to_string
    in
    override_values to_string answer |> override
  | Public.Select (_, _, answer) ->
    let to_string = Entity.SelectOption.Public.show_id in
    override_values to_string answer |> override
  | Public.Text (_, answer) -> override_values CCFun.id answer |> override
;;

let update pool user (field : PartialUpdate.t) (contact : Contact.t) =
  let open Entity in
  let is_admin = Pool_context.user_is_admin user in
  let dyn =
    Dynparam.empty
    |> Dynparam.add Contact.Repo.Id.t (contact |> Contact.id)
    |> Dynparam.add Caqti_type.ptime (Ptime_clock.now ())
  in
  let update_user_table (dyn, sql) =
    let open Caqti_request.Infix in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let update_request = sql |> update_sql |> pt ->. Caqti_type.unit in
    Database.exec pool update_request pv
  in
  let open PartialUpdate in
  match field with
  | Firstname (version, firstname) ->
    let%lwt (_ : Pool_user.t) = update_user pool ~firstname contact in
    ( dyn |> Dynparam.add Pool_common.Repo.Version.t version
    , {sql| firstname_version = $3 |sql} )
    |> update_user_table
  | Lastname (version, lastname) ->
    let%lwt (_ : Pool_user.t) = update_user pool ~lastname contact in
    ( dyn |> Dynparam.add Pool_common.Repo.Version.t version
    , {sql| lastname_version = $3 |sql} )
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
  | Custom field ->
    (upsert_answer pool is_admin Contact.(id contact |> Id.to_common)) field
;;
