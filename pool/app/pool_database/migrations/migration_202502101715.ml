let change_error_code_of_text_message_dlr =
  Database.Migration.Step.create
    ~label:"change error code of text message dlr"
    {sql|
      ALTER TABLE pool_text_message_dlr MODIFY COLUMN error_code VARCHAR(16);
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202502101715" |> add_step change_error_code_of_text_message_dlr)
;;
