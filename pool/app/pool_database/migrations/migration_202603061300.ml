let add_smtp_rate_limit_and_capacity =
  Database.Migration.Step.create
    ~label:"Add rate_limit and invitation_capacity to pool_smtp"
    {sql|
      ALTER TABLE pool_smtp
        ADD COLUMN system_account boolean DEFAULT false
          AFTER default_account,
        ADD COLUMN internal_regex TEXT
          COMMENT 'Optional regex pattern to match recipient emails for this SMTP account'
          AFTER system_account,
        ADD COLUMN rate_limit INT UNSIGNED NOT NULL DEFAULT 86400
          COMMENT 'Max emails per hour for this SMTP account'
          AFTER internal_regex,
        ADD COLUMN invitation_capacity TINYINT(3) UNSIGNED NOT NULL DEFAULT 80
          COMMENT 'Percentage of rate_limit reserved for invitation emails (0-100)'
          AFTER rate_limit
    |sql}
;;

let migration () =
  Database.Migration.(empty "202603061300" |> add_step add_smtp_rate_limit_and_capacity)
;;
