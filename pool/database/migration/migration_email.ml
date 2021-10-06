let update_email_template_password_reset =
  Sihl.Database.Migration.create_step
    ~label:"update email template password reset"
    {|
INSERT INTO email_templates(uuid, label, content_text, content_html)
VALUES
  (UNHEX(REPLACE('602baf34-7341-4807-864c-01e9b21e1d1a','-','')),
  'password_reset',
  'Dear {username},

You recently requested to reset your password for your account.
Follow the link below to reset it.

{resetUrl}

If you did not request a password reset, please ignore this email or
reply to let us know. This password reset is only valid for the next
hour.

If the above link does not work, please copy the following link into your browser manually: {resetUrl}

Yours sincerely,
Pool Tool',
  '
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <title></title>
  <style type="text/css">
  </style>
</head>
<body style="margin:0; padding:0;">
  <div style="margin: 1em 1em 1em 1em; max-width: 50em;">
    <section style="margin-bottom: 1em;">
        <p>Logo</p>
    </section>
    <section style="padding-top: 1em; color: #383838;">
<h4>Dear {username},</h4>
<p>
You recently requested to reset your password for your account.
Follow this <a href="{resetUrl}"> link </a> to reset it.
</p>
<p>
If you did not request a password reset, please ignore this email or reply to let us know. This password reset is only valid for the next hour.
</p>
<p>
Yours sincerely,
<br/>
Pool Tool
</p>
    </section>
    <footer style="margin-top: 4em;">
      <center>
        <small>Copyright</small>
      </center>
    </footer>
  </div>
</body>
</html>
')
|}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "emails" |> add_step update_email_template_password_reset)
;;
