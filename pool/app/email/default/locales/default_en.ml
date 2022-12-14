open Entity
open Default_entity
open Default_utils

let salutation =
  let open Tyxml.Html in
  h4 [ txt "Dear {name}," ]
;;

let complimentary_close =
  let open Tyxml.Html in
  p [ txt "Yours sincerely,"; br (); txt "Pool Tool" ]
;;

let add_salutation_to_text =
  Format.asprintf "Dear {name},\n\n%s\n\nYours sincerely,\nPool Tool"
;;

let password_reset =
  let label = TemplateLabel.PasswordReset in
  let language = Pool_common.Language.En in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p
        [ txt "You recently requested to reset your password for your account."
        ; br ()
        ; txt "Follow this"
        ; a ~a:[ a_href "{resetUrl}" ] [ txt " link " ]
        ; txt "to reset it."
        ]
    ; p
        [ txt
            "If you did not request a password reset, please ignore this email \
             or reply to let us know. This password reset is only valid for \
             the next hour."
        ]
    ; p
        [ txt
            "If the above link doesn't work, please copy the following link \
             into your browser manually: {resetUrl}"
        ]
    ; complimentary_close
    ]
    |> combine_html language (Some "Password reset")
    |> html_to_string
  in
  let text =
    {|
You recently requested to reset your password for your account.
Follow the link below to reset it.

{resetUrl}

If you did not request a password reset, please ignore this email or
reply to let us know. This password reset is only valid for the next
hour.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;

let email_verification =
  let label = TemplateLabel.EmailVerification in
  let language = Pool_common.Language.En in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p
        [ txt "You recently added a new email address to your account."
        ; br ()
        ; txt "Follow this"
        ; a ~a:[ a_href "{verificationUrl}" ] [ txt " link " ]
        ; txt "to activate it."
        ]
    ; p
        [ txt
            "If this action wasn`t performed by you, please ignore this email \
             or reply to let us know."
        ]
    ; p
        [ txt
            "If the above link does not work, please copy the following link \
             into your browser manually: {verificationUrl}"
        ]
    ; complimentary_close
    ]
    |> combine_html language (Some "New email address")
    |> html_to_string
  in
  let text =
    {|
You recently added a new email address to your account.
Follow the link below to activate it.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;

let password_change =
  let label = TemplateLabel.PasswordChange in
  let language = Pool_common.Language.En in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p [ txt {|You recently changed your password for your account.|} ]
    ; p
        [ txt
            "If you did not change your password, please get in contact with \
             us."
        ]
    ; complimentary_close
    ]
    |> combine_html language (Some "Password reset")
    |> html_to_string
  in
  let text =
    {|
You recently changed your password for your account.

If you did not change your password, please get in contact with us.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;

let signup_verification =
  let label = TemplateLabel.SignUpVerification in
  let language = Pool_common.Language.En in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p
        [ txt "Thank your for sigin up for the Pool Tool."
        ; br ()
        ; txt "Follow this"
        ; a ~a:[ a_href "{verificationUrl}" ] [ txt " link " ]
        ; txt "to activate your account."
        ]
    ; p
        [ txt
            "If this action wasn`t performed by you, please ignore this email \
             or reply to let us know."
        ]
    ; p
        [ txt
            "If the above link does not work, please copy the following link \
             into your browser manually: {verificationUrl}"
        ]
    ; complimentary_close
    ]
    |> combine_html language (Some "Sign up")
    |> html_to_string
  in
  let text =
    {|
Thank your for sigin up for the Pool Tool.
Follow the link below to activate your account.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.
|}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;

let invitation =
  let label = TemplateLabel.Invitation in
  let language = Pool_common.Language.En in
  let html =
    let open Tyxml.Html in
    [ salutation
    ; p [ txt "We would like to invite you to the following experiment:" ]
    ; p [ txt "{experimentPublicTitle}" ]
    ; p [ txt "{experimentDescription}" ]
    ; p [ txt "The experiment is performed on the following dates:" ]
    ; p [ txt "Sessions......" ]
    ; complimentary_close
    ]
    |> combine_html language (Some "Invitation to participate in a study")
    |> html_to_string
  in
  let text =
    {|
We would like to invite you to the following experiment:

{experimentPublicTitle}
{experimentDescription}

The experiment is performed on the following dates:

Sessions......
    |}
    |> add_salutation_to_text
  in
  { label; language; text; html }
;;
