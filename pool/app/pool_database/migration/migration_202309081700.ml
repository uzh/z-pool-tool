let update_location_descriptions =
  Sihl.Database.Migration.create_step
    ~label:"update location descriptions"
    {sql|
      UPDATE pool_locations
      SET description = CASE
          WHEN description LIKE '[[[%]]' THEN description
          ELSE CONCAT('[[["EN"],"', REPLACE(description, '"', '\\"'), '"],[["DE"],"', REPLACE(description, '"', '\\"'), '"]]')
      END
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309081700" |> add_step update_location_descriptions)
;;
