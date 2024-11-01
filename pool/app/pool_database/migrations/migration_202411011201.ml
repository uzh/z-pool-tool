let create_custom_field_answer_index =
  Database.Migration.Step.create
    ~label:"create custom_field_answer index"
    {sql|
      CREATE INDEX entity_uuid_custom_field_uuid ON pool_custom_field_answers(entity_uuid, custom_field_uuid);
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202411011201" |> add_step create_custom_field_answer_index)
;;
