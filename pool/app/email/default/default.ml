include Default_entity
open Pool_common.Language

let password_reset = function
  | De -> Default_de.password_reset
  | En -> Default_en.password_reset
;;

let password_change = function
  | De -> Default_de.password_change
  | En -> Default_en.password_change
;;

let email_verification = function
  | De -> Default_de.email_verification
  | En -> Default_en.email_verification
;;

let signup_verification = function
  | De -> Default_de.signup_verification
  | En -> Default_en.signup_verification
;;

let invitation = function
  | De -> Default_de.invitation
  | En -> Default_en.invitation
;;

let boilerplate language =
  let salutation, complimentary_close, add_salutation_to_text =
    match language with
    | De ->
      let open Default_de in
      salutation, complimentary_close, add_salutation_to_text
    | En ->
      let open Default_en in
      salutation, complimentary_close, add_salutation_to_text
  in
  let label = Entity.TemplateLabel.Boilerplate in
  let html =
    let open Tyxml.Html in
    let open Default_utils in
    [ salutation; p [ txt "{content}" ]; complimentary_close ]
    |> combine_html language None
    |> html_to_string
  in
  let text = {|
{content}
    |} |> add_salutation_to_text in
  { label; language; text; html }
;;

let ( @@@ ) constructors =
  CCList.flat_map (fun lang -> CCList.map (fun fcn -> fcn lang) constructors)
;;

let default_values_root = [ password_reset ] @@@ [ En; De ]

let default_values_tenant =
  [ email_verification
  ; password_change
  ; password_reset
  ; signup_verification
  ; invitation
  ; boilerplate
  ]
  @@@ [ En; De ]
;;
