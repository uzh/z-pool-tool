let add_webhook_urls_to_signup_codes =
  Database.Migration.Step.create
    ~label:"add webhook urls to signup codes"
    {sql|
      ALTER TABLE pool_signup_codes
        ADD COLUMN signed_up_webhook_url VARCHAR(2048) NULL AFTER verification_count,
        ADD COLUMN email_verified_webhook_url VARCHAR(2048) NULL AFTER signed_up_webhook_url
    |sql}
;;

let migration () =
  Database.Migration.(empty "202611161200" |> add_step add_webhook_urls_to_signup_codes)
;;
