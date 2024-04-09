let add_show_on_session_detail_screen_flag =
  Database.Migration.create_step
    ~label:"add show on session detail screen flag"
    {sql|
      ALTER TABLE pool_custom_fields
        ADD COLUMN show_on_session_detail_screen boolean DEFAULT false AFTER show_on_session_close_screen
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202402201330" |> add_step add_show_on_session_detail_screen_flag)
;;
