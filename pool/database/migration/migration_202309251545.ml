open Sihl.Database.Migration

let add_process_to_mailing =
  create_step
    ~label:"add process to mailing"
    {sql|
      ALTER TABLE pool_mailing
        ADD COLUMN process VARCHAR(30) DEFAULT 'new_invitation' AFTER distribution
    |sql}
;;

let migration () = empty "202309251545" |> add_step add_process_to_mailing
