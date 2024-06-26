let add_limit_to_mailing =
  Database.Migration.Step.create
    ~label:"add limit to mailing"
    {sql|
      ALTER TABLE pool_mailing
        ADD COLUMN `limit` integer NOT NULL DEFAULT 0 AFTER rate
    |sql}
;;

let change_mailing_rate_to_limit =
  Database.Migration.Step.create
    ~label:"change mailing rate to limit"
    {sql|
      UPDATE pool_mailing
      SET
        `limit` = ROUND(TIMESTAMPDIFF(MINUTE, `start`, `end`) / 60 * rate),
        `start` = `start`
      WHERE rate != 0 AND `start` < `end`;
    |sql}
;;

let drop_rate_from_mailing =
  Database.Migration.Step.create
    ~label:"drop rate from mailing"
    {sql|
      ALTER TABLE pool_mailing
      DROP COLUMN rate
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202310191345"
    |> add_step add_limit_to_mailing
    |> add_step change_mailing_rate_to_limit
    |> add_step drop_rate_from_mailing)
;;
