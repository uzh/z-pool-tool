let add_uuid_foreignkey_column_to_sessions =
  Database.Migration.Step.create
    ~label:"add uuid foreignkey column to sessions"
    {sql|
      ALTER TABLE pool_sessions
        ADD COLUMN location_uuid binary(16) AFTER location_id
    |sql}
;;

let set_uuid_foreignkey_on_sessions =
  Database.Migration.Step.create
    ~label:"set uuid foreignkey on sessions"
    {sql|
      UPDATE pool_sessions SET
        location_uuid = (SELECT uuid FROM pool_locations WHERE pool_locations.id = pool_sessions.location_id)
    |sql}
;;

let add_fk_contraint_to_sessions =
  Database.Migration.Step.create
    ~label:"add unique contraint to waiting_list"
    {sql|
      ALTER TABLE pool_sessions
        ADD CONSTRAINT fk_pool_sessions_locations
        FOREIGN KEY (location_uuid) REFERENCES pool_locations(uuid)
    |sql}
;;

let drop_location_id_from_sessions =
  Database.Migration.Step.create
    ~label:"drop location id from sessions"
    {sql|
      ALTER TABLE pool_sessions
        DROP COLUMN location_id
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202306021512"
    |> add_step add_uuid_foreignkey_column_to_sessions
    |> add_step set_uuid_foreignkey_on_sessions
    |> add_step add_fk_contraint_to_sessions
    |> add_step drop_location_id_from_sessions)
;;
