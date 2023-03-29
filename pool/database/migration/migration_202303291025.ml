let add_no_show_count_to_contacts =
  Sihl.Database.Migration.create_step
    ~label:"add no show count to contacts"
    {sql|
      BEGIN NOT ATOMIC
        ALTER TABLE pool_contacts
          ADD COLUMN num_no_shows INT UNSIGNED DEFAULT 0 AFTER num_show_ups;

        ALTER TABLE pool_assignments
          ADD COLUMN no_show BOOLEAN NULL AFTER show_up;

        UPDATE pool_assignments SET
          no_show = IF(participated = 1 OR show_up = 1, 0, IF(show_up = 0, 1, NULL));

        ALTER TABLE pool_assignments
          DROP COLUMN show_up;
      END;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202303291025" |> add_step add_no_show_count_to_contacts)
;;
