open Tyxml.Html

let show _ languages email_suffixes message () =
  let languages_html =
    div
      [ h2 [ txt "Langauges" ]
      ; div
          (CCList.map
             (fun language -> p [ txt (language |> Settings.Language.code) ])
             languages)
      ]
  in
  let email_suffixes_html =
    div
      [ h2 [ txt "Email Suffixes" ]
      ; div
          (CCList.map
             (fun suffix -> p [ txt (suffix |> Settings.EmailSuffix.value) ])
             email_suffixes)
      ]
  in
  let html =
    div [ h1 [ txt "Settings" ]; languages_html; email_suffixes_html ]
  in
  Page_layout.create html message ()
;;
