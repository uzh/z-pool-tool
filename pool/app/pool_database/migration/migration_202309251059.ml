let add_external_data_show_flag =
  Sihl.Database.Migration.create_step
    ~label:"add flag to show external data id link"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN show_external_data_id_links boolean DEFAULT 0 AFTER external_data_required
    |sql}
;;

let set_show_external_data_id_links_default_values =
  Sihl.Database.Migration.create_step
    ~label:"add flag to show external data id link"
    {sql|
    UPDATE
      pool_experiments
    SET
      show_external_data_id_links = 1
    WHERE
      external_data_required = 1
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309251059"
    |> add_step add_external_data_show_flag
    |> add_step set_show_external_data_id_links_default_values)
;;
