let update_inactive_contact_message_template_de =
  Database.Migration.Step.create
    ~label:"Update inactive contact message template DE"
    {sql| 
        UPDATE pool_message_templates
          SET
            email_text_html='<h4>Grüezi {name},</h4><p>Sie haben sich seit dem {lastLogin} nicht mehr in Ihren Account eingeloggt.&nbsp;</p><p>Falls Sie sich nicht wieder einloggen, wird Ihr Account pausiert und Sie werden an keine weiteren Experimente mehr eingeladen.&nbsp;</p><p>Hier einloggen: {siteUrl}&nbsp;</p><p>Freundliche Grüsse&nbsp;<br>{siteTitle}</p>', 
            email_text_plain='Grüezi {name},\r\n\r\nSie haben sich seit dem {lastLogin} nicht mehr in Ihren Account eingeloggt. \r\n\r\nFalls Sie sich nicht wieder einloggen, wird Ihr Account pausiert und Sie werden an keine weiteren Experimente mehr eingeladen. \r\n\r\nHier einloggen: {siteUrl}\r\n\r\nFreundliche Grüsse \r\n{siteTitle}'
          WHERE 
            label = 'inactive_contact_warning' 
            AND language = 'DE'
            AND created_at = updated_at
        |sql}
;;

let update_inactive_contact_message_template_en =
  Database.Migration.Step.create
    ~label:"Update inactive contact message template EN"
    {sql|
      UPDATE pool_message_templates
      SET
        email_text_html = '<h4>Dear {name},</h4><p>You have not logged into your account since {lastLogin}.</p><p>If you do not log in again, your account will be paused and you will not be invited to any further experiments.&nbsp;</p><p>Log in here: {siteUrl}&nbsp;</p><p>Yours sincerely,<br>{siteTitle}</p>',
        email_text_plain = 'Dear {name},\r\n\r\nYou have not logged into your account since {lastLogin}.\r\n\r\nIf you do not log in again, your account will be paused and you will not be invited to any further experiments. \r\n\r\nLog in here: {siteUrl}\r\n\r\nYours sincerely,\r\n{siteTitle}'
      WHERE
        label = 'inactive_contact_warning'
        AND language = 'EN'
        AND created_at = updated_at
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202508141040"
    |> add_step update_inactive_contact_message_template_de
    |> add_step update_inactive_contact_message_template_en)
;;
