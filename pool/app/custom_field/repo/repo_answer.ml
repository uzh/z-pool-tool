open Entity

let clear_answer_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_custom_field_answers
    WHERE entity_uuid = UNHEX(REPLACE($1, '-', ''))
    AND custom_field_uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> Caqti_type.(t2 Pool_common.Repo.Id.t Pool_common.Repo.Id.t) ->. Caqti_type.unit
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

let override_values to_string (answer : 'a Answer.t option) =
  let open Answer in
  let open CCOption.Infix in
  match answer with
  | None -> None
  | Some { value; admin_value; _ } -> Some (value >|= to_string, admin_value >|= to_string)
;;

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
