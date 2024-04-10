let rename_experiment_description =
  Database.Migration.Step.create
    ~label:"rename experiment description"
    {sql|
      ALTER TABLE pool_experiments
        RENAME COLUMN description TO internal_description
    |sql}
;;

let add_internal_experiment_description =
  Database.Migration.Step.create
    ~label:"add internal experiment description"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN public_description text AFTER internal_description
    |sql}
;;

let modify_session_description =
  Database.Migration.Step.create
    ~label:"modify session description"
    {sql|
      ALTER TABLE pool_sessions
        CHANGE COLUMN description internal_description text
    |sql}
;;

let rename_session_limitations =
  Database.Migration.Step.create
    ~label:"rename session limitations"
    {sql|
      ALTER TABLE pool_sessions
        RENAME COLUMN limitations TO public_description
    |sql}
;;

let replace_message_template_experiment_description_elements =
  Database.Migration.Step.create
    ~label:"replace message template experiment description elements"
    {sql|
      UPDATE
        pool_message_templates
      SET
        email_text_html = REPLACE(email_text_html, '{experimentDescription}', '{experimentPublicDescription}'),
        email_text_plain = REPLACE(email_text_plain, '{experimentDescription}', '{experimentPublicDescription}'),
        sms_text = REPLACE(sms_text, '{experimentDescription}', '{experimentPublicDescription}')
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202401120946"
    |> add_step rename_experiment_description
    |> add_step add_internal_experiment_description
    |> add_step modify_session_description
    |> add_step rename_session_limitations
    |> add_step replace_message_template_experiment_description_elements)
;;
