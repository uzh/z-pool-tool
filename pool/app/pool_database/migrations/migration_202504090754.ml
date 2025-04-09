let wrap_existing_scripts =
  Database.Migration.Step.create
    ~label:"wrap existing scripts in script tags"
    [%string
      {sql|
        UPDATE pool_tenant_page_scripts
        SET
          script = CONCAT('<script>', script, '</script>')
        WHERE
          script IS NOT NULL;
      |sql}]
;;

let migration () =
  Database.Migration.(empty "202504090754" |> add_step wrap_existing_scripts)
;;
