let add_num_no_shows_to_contacts =
  Database.Migration.Step.create
    ~label:"add num_no_shows to contacts"
    {sql|
        ALTER TABLE pool_contacts
          ADD COLUMN num_no_shows INT UNSIGNED DEFAULT 0 AFTER num_show_ups
    |sql}
;;

let add_no_show_to_assignments =
  Database.Migration.Step.create
    ~label:"add no_show to assignments"
    {sql|
      ALTER TABLE pool_assignments
        ADD COLUMN no_show BOOLEAN NULL AFTER show_up
    |sql}
;;

let set_no_show_value =
  Database.Migration.Step.create
    ~label:"set no show value"
    {sql|
      UPDATE pool_assignments SET
        no_show = IF(participated = 1 OR show_up = 1, 0, IF(show_up = 0, 1, NULL))
    |sql}
;;

let drop_show_up_column =
  Database.Migration.Step.create
    ~label:"drop show up column"
    {sql|
      ALTER TABLE pool_assignments
        DROP COLUMN show_up
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202303291025"
    |> add_step add_num_no_shows_to_contacts
    |> add_step add_no_show_to_assignments
    |> add_step set_no_show_value
    |> add_step drop_show_up_column)
;;
