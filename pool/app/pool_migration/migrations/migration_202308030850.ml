let add_promoted_contacts_table =
  Database.Migration.create_step
    ~label:"add promoted contacts table"
    {sql|CREATE TABLE `pool_contacts_promoted` LIKE `pool_contacts`|sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202308030850" |> add_step add_promoted_contacts_table)
;;
