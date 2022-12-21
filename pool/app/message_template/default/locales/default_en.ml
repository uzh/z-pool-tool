open Entity
open Default_utils

let language = Pool_common.Language.En
let entity_uuid = None

let assignment_confirmation =
  let label = Label.AssignmentConfirmation in
  let email_text =
    let open Tyxml.Html in
    div
      [ p
          [ txt "You successfully registered to the following session:"
          ; br ()
          ; txt "{sessionOverview}"
          ]
      ; p [ txt "The participation in the session is compulsory." ]
      ]
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Assignment confirmation" |> EmailSubject.of_string in
  let sms_text =
    {|You successfully registered to the following session:

{sessionOverview}

The participation in the session is compulsory.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let email_verification =
  let label = Label.EmailVerification in
  let email_text =
    let open Tyxml.Html in
    div
      [ p
          [ txt "You recently added a new email address to your account."
          ; br ()
          ; txt "Follow this"
          ; a ~a:[ a_href "{verificationUrl}" ] [ txt " link " ]
          ; txt "to activate it."
          ]
      ; p
          [ txt
              "If this action wasn`t performed by you, please ignore this \
               email or reply to let us know."
          ]
      ; p
          [ txt
              "If the above link does not work, please copy the following link \
               into your browser manually: {verificationUrl}"
          ]
      ]
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Email verification" |> EmailSubject.of_string in
  let sms_text =
    {|You recently added a new email address to your account.
Follow the link below to activate it.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let experiment_invitation =
  let label = Label.ExperimentInvitation in
  let email_text =
    let open Tyxml.Html in
    div
      [ p [ txt "We would like to invite you to the following experiment:" ]
      ; p [ txt "{experimentPublicTitle}" ]
      ; p [ txt "{experimentDescription}" ]
      ; p [ txt "The experiment is performed on the following dates:" ]
      ; p [ txt "Sessions......" ]
      ]
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Experiment invitation" |> EmailSubject.of_string in
  let sms_text =
    {|We would like to invite you to the following experiment:

{experimentPublicTitle}
{experimentDescription}

The experiment is performed on the following dates:

Sessions......|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let password_change =
  let label = Label.PasswordChange in
  let email_text =
    let open Tyxml.Html in
    div
      [ p [ txt {|You recently changed your password for your account.|} ]
      ; p
          [ txt
              "If you did not change your password, please get in contact with \
               us."
          ]
      ]
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Password change" |> EmailSubject.of_string in
  let sms_text =
    {|You recently changed your password for your account.

If you did not change your password, please get in contact with us.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let password_reset =
  let label = Label.PasswordReset in
  let email_text =
    let open Tyxml.Html in
    div
      [ p
          [ txt
              "You recently requested to reset your password for your account."
          ; br ()
          ; txt "Follow this"
          ; a ~a:[ a_href "{resetUrl}" ] [ txt " link " ]
          ; txt "to reset it."
          ]
      ; p
          [ txt
              "If you did not request a password reset, please ignore this \
               email or reply to let us know. This password reset is only \
               valid for the next hour."
          ]
      ; p
          [ txt
              "If the above link doesn't work, please copy the following link \
               into your browser manually: {resetUrl}"
          ]
      ]
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
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let signup_verification =
  let label = Label.SignUpVerification in
  let email_text =
    let open Tyxml.Html in
    div
      [ p
          [ txt "Thank your for sigin up for the Pool Tool."
          ; br ()
          ; txt "Follow this"
          ; a ~a:[ a_href "{verificationUrl}" ] [ txt " link " ]
          ; txt "to activate your account."
          ]
      ; p
          [ txt
              "If this action wasn`t performed by you, please ignore this \
               email or reply to let us know."
          ]
      ; p
          [ txt
              "If the above link does not work, please copy the following link \
               into your browser manually: {verificationUrl}"
          ]
      ]
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Signup verification" |> EmailSubject.of_string in
  let sms_text =
    {|Thank your for sigin up for the Pool Tool.
Follow the link below to activate your account.

{verificationUrl}

If this action wasn`t performed by you, please ignore this email or reply to let us know.|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let session_cancellation =
  let label = Label.SessionCancellation in
  let email_text =
    let open Tyxml.Html in
    div
      [ p
          [ txt "The following session you have registered to was canceled:"
          ; br ()
          ; txt "{sessionOverview}"
          ]
      ]
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session cancellation" |> EmailSubject.of_string in
  let sms_text =
    {|The following session you have registered to was canceled:

{sessionOverview}|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;

let session_reminder =
  let label = Label.SessionReminder in
  let email_text =
    let open Tyxml.Html in
    div
      [ p
          [ txt "Herewith we remind you about your upcoming experiment session:"
          ; br ()
          ; txt "{sessionOverview}"
          ]
      ]
    |> html_to_string
    |> EmailText.of_string
  in
  let email_subject = "Session reminder" |> EmailSubject.of_string in
  let sms_text =
    {|Herewith we remind you about your upcoming experiment session:

{sessionOverview}|}
    |> SmsText.of_string
  in
  { id = Id.create ()
  ; label
  ; language
  ; entity_uuid
  ; email_text
  ; email_subject
  ; sms_text
  }
;;
