open Sihl.Database.Migration

let add_process_to_mailing =
  create_step
    ~label:"add process to mailing"
    {sql|
      ALTER TABLE pool_mailing
        ADD COLUMN process VARCHAR(30) DEFAULT 'new_invitation' AFTER distribution
    |sql}
;;

let add_resent_count_to_invitation =
  create_step
    ~label:"add resent count to invitation"
    {sql|
      ALTER TABLE pool_invitations
        ADD COLUMN resent_count int(10) DEFAULT 0 AFTER resent_at
    |sql}
;;

let migration () =
  empty "202309251545"
  |> add_step add_process_to_mailing
  |> add_step add_resent_count_to_invitation
;;
