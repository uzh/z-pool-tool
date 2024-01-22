let add_show_on_close_screen_flag =
  Sihl.Database.Migration.create_step
    ~label:"rename experiment description"
    {sql|
      ALTER TABLE pool_custom_fields
        ADD COLUMN show_on_session_close_screen boolean DEFAULT false AFTER position
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202401191048" |> add_step add_show_on_close_screen_flag)
;;
