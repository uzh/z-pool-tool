let remove_greetings_text =
  Database.Migration.Step.create
    ~label:"remove unused greetings_text from i18n table"
    {sql|
      DELETE FROM pool_i18n
      WHERE i18n_key = 'greetings_text'
    |sql}
;;

let migration () =
  Database.Migration.(empty "202607071300" |> add_step remove_greetings_text)
;;
