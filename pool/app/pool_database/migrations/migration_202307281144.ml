let add_prompt_on_registration_flag_to_custom_fields =
  Database.Migration.Step.create
    ~label:"create pool admins table"
    {sql|
      ALTER TABLE pool_custom_fields
      ADD COLUMN prompt_on_registration boolean NOT NULL DEFAULT 0 AFTER admin_input_only
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202307281144"
    |> add_step add_prompt_on_registration_flag_to_custom_fields)
;;
