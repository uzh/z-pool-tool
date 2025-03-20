let add_default_page_scripts =
  Database.Migration.Step.create
    ~label:"drop_falsy_unique_constraints_user"
    [%string
      {sql|
        INSERT INTO pool_tenant_page_scripts (uuid, location) VALUES 
          (UNHEX(REPLACE(UUID(), '-', '')), 'head'),
          (UNHEX(REPLACE(UUID(), '-', '')), 'body') 
        ON DUPLICATE KEY UPDATE
          id = id;
      |sql}]
;;

let migration () =
  Database.Migration.(empty "202503201246" |> add_step add_default_page_scripts)
;;
