let add_marked_as_deleted_to_waitinglist =
  Database.Migration.Step.create
    ~label:"add marked_as_deleted flat to pool_waiting_list"
    {sql|
      ALTER TABLE pool_waiting_list
        ADD COLUMN marked_as_deleted boolean DEFAULT 0 AFTER comment
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202407081355" |> add_step add_marked_as_deleted_to_waitinglist)
;;
