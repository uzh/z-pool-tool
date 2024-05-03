let add_external_data_required =
  Database.Migration.Step.create
    ~label:"add promoted contacts table"
    {sql|
      ALTER TABLE pool_experiments
      ADD COLUMN external_data_required boolean DEFAULT false AFTER allow_uninvited_signup
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202308041330" |> add_step add_external_data_required)
;;
