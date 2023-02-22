open Entity
open Message_utils
open Tyxml.Html

let language = Pool_common.Language.En
let entity_uuid = None
let salutation = h4 [ txt "Dear {name}," ]
let complimentary_close = p [ txt "Yours sincerely,"; br (); txt "Pool Tool" ]

let add_salutation_to_text =
  Format.asprintf "Dear {name},\n\n%s\n\nYours sincerely,\nPool Tool"
;;

let add_salutation html = div ((salutation :: html) @ [ complimentary_close ])

let assignment_confirmation =
  let label = Label.AssignmentConfirmation in
  let email_text =
    [ p
        [ txt "You successfully registered to the following session:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ; p [ txt "The participation in the session is compulsory." ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Assignment confirmation" |> EmailSubject.of_string in
  let sms_text =
    {|You successfully registered to the following session:

{sessionOverview}

The participation in the session is compulsory.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let email_verification =
  let label = Label.EmailVerification in
  let email_text =
    [ p
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
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Email verification" |> EmailSubject.of_string in
  let sms_text =
    {|You recently added a new email address to your account.
Follow the link below to activate it.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let experiment_invitation =
  let label = Label.ExperimentInvitation in
  let email_text =
    [ p [ txt "We would like to invite you to the following experiment:" ]
    ; p [ strong [ txt "{experimentPublicTitle}" ] ]
    ; p [ txt "{experimentDescription}" ]
    ; p
        [ txt "Information about the sessions can be found "
        ; a ~a:[ a_href "{experimentUrl}" ] [ txt "here" ]
        ; txt "."
        ]
    ; p
        [ txt
            "If the above link doesn't work, please copy the following link \
             into your browser manually: {experimentUrl}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Experiment invitation" |> EmailSubject.of_string in
  let sms_text =
    {|We would like to invite you to the following experiment:

{experimentPublicTitle}
{experimentDescription}

Information about the sessions can be found here: {experimentUrl}|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let password_change =
  let label = Label.PasswordChange in
  let email_text =
    [ p [ txt {|You recently changed your password for your account.|} ]
    ; p
        [ txt
            "If you did not change your password, please get in contact with \
             us."
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Password change" |> EmailSubject.of_string in
  let sms_text =
    {|You recently changed your password for your account.

If you did not change your password, please get in contact with us.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let profile_update_trigger =
  let label = Label.ProfileUpdateTrigger in
  let email_text =
    [ p [ txt "Your profile has not been updated in a while." ]
    ; p
        [ txt "Please check your "
        ; a ~a:[ a_href "{profileUrl}" ] [ txt " profile" ]
        ; txt "."
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Please check your profile." |> EmailSubject.of_string in
  let sms_text =
    {|Your profile has not been updated in a while.

Please check your profile: {profileUrl}|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let password_reset =
  let label = Label.PasswordReset in
  let email_text =
    [ p
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
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Password reset" |> EmailSubject.of_string in
  let sms_text =
    {|You recently requested to reset your password for your account.
Follow the link below to reset it.

{resetUrl}

If you did not request a password reset, please ignore this email or
reply to let us know. This password reset is only valid for the next
hour.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let signup_verification =
  let label = Label.SignUpVerification in
  let email_text =
    [ p
        [ txt "Thank your for signing up for the Pool Tool."
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
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Sign up verification" |> EmailSubject.of_string in
  let sms_text =
    {|Thank your for signing up for the Pool Tool.
Follow the link below to activate your account.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let session_cancellation =
  let label = Label.SessionCancellation in
  let email_text =
    [ p [ txt "The following session you have registered to was canceled:" ]
    ; p [ txt "{sessionOverview}" ]
    ; p [ txt "Reason:" ]
    ; p [ txt "{reason}" ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session cancellation" |> EmailSubject.of_string in
  let sms_text =
    {|The following session you have registered to was canceled:

{sessionOverview}

Reason: {reason}|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let session_reminder =
  let label = Label.SessionReminder in
  let email_text =
    [ p
        [ txt "Herewith we remind you about your upcoming experiment session:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session reminder" |> EmailSubject.of_string in
  let sms_text =
    {|Herewith we remind you about your upcoming experiment session:

{sessionOverview}|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let session_reschedule =
  let label = Label.SessionReschedule in
  let email_text =
    [ p
        [ txt "The following session you have registered to was rescheduled:"
        ; br ()
        ; txt "{sessionOverview}"
        ]
    ; p
        [ txt "The new date and time is:"
        ; br ()
        ; txt "{newStart}"
        ; br ()
        ; txt "{newDuration}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session was rescheduled" |> EmailSubject.of_string in
  let sms_text =
    {|The following session you have registered to was rescheduled

{sessionOverview}

New:
{newStart}
{newDuration}
|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;

let contact_registration_attempt =
  let label = Label.ContactRegistrationAttempt in
  let email_text =
    [ p
        [ txt
            "There was an attempt to create an account with the email address \
             '{emailAddress}' on {tenantUrl}."
        ; br ()
        ; txt "An account with this email address already exists."
        ]
    ; p
        [ txt
            "If this was you and you forgot your login credentials, you can \
             reset your password "
        ; a ~a:[ a_href "{resetUrl}" ] [ txt "here" ]
        ; txt "."
        ]
    ; p
        [ txt
            "If this action was not performed by you, you can ignore this \
             message or inform the administrators."
        ]
    ; p
        [ txt
            "If the above link does not work, please copy the following link \
             into your browser manually: {resetUrl}"
        ]
    ]
    |> add_salutation
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Signup attempt" |> EmailSubject.of_string in
  let sms_text =
    {|There was an attempt to create an account with the email address '{emailAddress}' on {tenantUrl}.

If this was you and you forgot your login credentials, you can reset you can reset your password here: {resetUrl}

If this action was not performed by you, you can ignore this message or inform the administrators.
|}
    |> add_salutation_to_text
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; plain_text = sms_text
  ; sms_text
  }
;;
