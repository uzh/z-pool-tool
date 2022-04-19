type email_template =
  { label : string
  ; language : string
  ; text : string
  ; html : string
  }

type default = email_template list

let password_reset =
  let html =
    {html|
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
  |html}
  in
  let text =
    {|
Dear {name},

You recently requested to reset your password for your account.
Follow the link below to reset it.

{resetUrl}

If you did not request a password reset, please ignore this email or
reply to let us know. This password reset is only valid for the next
hour.

If the above link does not work, please copy the following link into your browser manually: {resetUrl}

Yours sincerely,
Pool Tool
|}
  in
  { label = "password_reset"; language = "EN"; text; html }
;;

let email_verification =
  let html =
    {html|
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <title>d</title>
  <style type="text/css">
  </style>
</head>
<body style="margin:0; padding:0;">
  <div style="margin: 1em 1em 1em 1em; max-width: 50em;">
    <section style="margin-bottom: 1em;">
        <p>Logo</p>
    </section>
    <section style="padding-top: 1em; color: #383838;">
<h4>Dear {name},</h4>
<p>
You recently added a (new) email address to your account.
Follow this <a href="{verificationUrl}"> link </a> to activate it.
</p>
<p>
If this action wasn`t performed by you, please ignore this email or reply to let us know.
</p>
<p>
If the above link does not work, please copy the following link into your browser manually: {verificationUrl}
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
  |html}
  in
  let text =
    {|
Dear {name},

You recently added a (new) email address to your account.
Follow the link below to activate it.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.

Yours sincerely,
Pool Tool
|}
  in
  { label = "email_verification"; language = "EN"; text; html }
;;

let change_password =
  let html =
    {html|
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
You recently changed your password for your account.
</p>
<p>
If you did not change your password, please get in contact with us.
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
  |html}
  in
  let text =
    {|
Dear {name},

You recently changed your password for your account.

If you did not change your password, please get in contact with us.

Yours sincerely,
Pool Tool
|}
  in
  { label = "password_change"; language = "EN"; text; html }
;;

let signup_verification =
  let html =
    {html|
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html lang="en">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <title>d</title>
  <style type="text/css">
  </style>
</head>
<body style="margin:0; padding:0;">
  <div style="margin: 1em 1em 1em 1em; max-width: 50em;">
    <section style="margin-bottom: 1em;">
        <p>Logo</p>
    </section>
    <section style="padding-top: 1em; color: #383838;">
<h4>Dear {name},</h4>
<p>
Thank your for sigin up for the Pool Tool.
Follow this <a href="{verificationUrl}"> link </a>to activate your account.
</p>
<p>
If this action wasn`t performed by you, please ignore this email or reply to let us know.
</p>
<p>
If the above link does not work, please copy the following link into your browser manually: {verificationUrl}
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
  |html}
  in
  let text =
    {|
Dear {name},

Thank your for sigin up for the Pool Tool.
Follow the link below to activate your account.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.

Yours sincerely,
Pool Tool
|}
  in
  { label = "signup_verification"; language = "EN"; text; html }
;;

let default_values =
  [ password_reset; email_verification; change_password; signup_verification ]
;;
