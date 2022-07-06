module Field = Pool_common.Message.Field

let signup
    terms
    Pool_context.{ language; query_language; csrf; _ }
    flash_fetcher
  =
  let open Tyxml.Html in
  let submit_url =
    Http_utils.externalize_path_with_lang query_language "/signup"
  in
  let txt_to_string m = [ txt (Pool_common.Utils.text_to_string language m) ] in
  let channel_select =
    let open Contact.RecruitmentChannel in
    let field = Pool_common.Message.Field.RecruitmentChannel in
    let selected =
      field
      |> Pool_common.Message.Field.show
      |> flash_fetcher
      |> CCFun.flip CCOption.bind (fun field ->
             try Some (read field) with
             | _ -> None)
    in
    Component.selector field equal show all selected ()
  in
  let open Component in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 (txt_to_string Pool_common.I18n.SignUpTitle)
    ; form
        ~a:[ a_action submit_url; a_method `Post; a_class [ "stack" ] ]
        [ Component.csrf_element csrf ()
        ; input_element language `Email Field.Email ~flash_fetcher
        ; input_element language `Text Field.Firstname ~flash_fetcher
        ; input_element language `Text Field.Lastname ~flash_fetcher
        ; input_element language `Password Field.Password ~value:""
        ; channel_select
        ; div
            [ p [ txt (Settings.TermsAndConditions.Terms.value terms) ]
            ; Component.checkbox_element
                language
                Pool_common.Message.Field.TermsAccepted
                ~required:true
            ]
        ; Component.submit_element language Pool_common.Message.SignUp ()
        ]
    ]
;;
