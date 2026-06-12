open Tyxml.Html
open Component.Input
module HttpUtils = Http_utils

let query_params_with_intended query_parameters intended =
  intended
  |> CCOption.map_or ~default:query_parameters (fun intended ->
    [ Pool_message.Field.Location, intended ] @ query_parameters)
;;

let login_token_confirmation
      Pool_context.{ language; query_parameters; csrf; _ }
      ?authentication_id
      ~email
      ?intended
      url
  =
  let query_parameters = query_params_with_intended query_parameters intended in
  let action = HttpUtils.externalize_path_with_params query_parameters url in
  let hint =
    Pool_common.I18n.LoginTokenSent (Pool_user.EmailAddress.value email)
    |> Pool_common.Utils.hint_to_string language
  in
  let hidden_input =
    let open Authentication in
    match authentication_id with
    | Some id -> input_element language `Hidden Pool_message.Field.Id ~value:(Id.value id)
    | None -> txt ""
  in
  let resend_token =
    let cooldown_seconds = 60 in
    let resend_action =
      HttpUtils.externalize_path_with_params
        query_parameters
        "/resend-login-confirmation-token"
    in
    let button_id = "resend-token-button" in
    let countdown_id = "resend-token-countdown" in
    let js_script =
      Format.asprintf
        {js|
(() => {
  const button = document.getElementById("%s");
  const countdown = document.getElementById("%s");
  if (!button || !countdown) { return; }
  button.disabled = true;
  const deadline = Date.now() + (parseInt(countdown.dataset.seconds, 10) * 1000);
  const update = () => {
    const remaining = Math.max(0, Math.ceil((deadline - Date.now()) / 1000));
    if (remaining > 0) {
      countdown.textContent = " (" + remaining + ")";
    } else {
      countdown.remove();
      button.disabled = false;
      clearInterval(interval);
    }
  };
  const interval = setInterval(update, 1000);
  update();
})();
        |js}
        button_id
        countdown_id
    in
    form
      ~a:[ a_action resend_action; a_method `Post; a_class [ "stack" ] ]
      [ csrf_element csrf ()
      ; hidden_input
      ; div
          ~a:[ a_class [ "flexrow"; "align-center"; "flex-gap" ] ]
          [ submit_element
              ~is_text:true
              ~attributes:[ a_id button_id ]
              language
              (Pool_message.Control.Resend (Some Pool_message.Field.OTP))
              ()
          ; span
              ~a:
                [ a_id countdown_id
                ; a_user_data "seconds" (Int.to_string cooldown_seconds)
                ]
              [ txt (Format.asprintf " (%d)" cooldown_seconds) ]
          ]
      ; script (Unsafe.data js_script)
      ]
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ Pool_common.(Utils.text_to_string language I18n.LoginTitle |> txt) ]
    ; Component.Notification.create language `Success [ txt hint ]
    ; div
        ~a:[ a_class [ "stack"; "gap" ] ]
        [ form
            ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
            [ csrf_element csrf ()
            ; hidden_input
            ; input_element
                ~additional_attributes:
                  [ a_autocomplete (`Tokens [ "one-time-code" ])
                  ; a_maxlength (Authentication.Token.length + 1)
                  ; a_inputmode `Numeric
                  ; a_aria
                      "label"
                      [ Pool_common.(Utils.text_to_string language I18n.OtpHint) ]
                  ; a_placeholder "1234 5678"
                  ]
                language
                `Text
                Pool_message.Field.OTP
            ; div
                ~a:[ a_class [ "flexrow"; "align-center"; "flex-gap" ] ]
                [ submit_element
                    ~classnames:[ "push" ]
                    language
                    Pool_message.Control.Login
                    ()
                ]
            ]
        ; resend_token
        ]
    ]
;;
