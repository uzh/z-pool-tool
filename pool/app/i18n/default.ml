open Entity

type default = t list [@@deriving eq, show]

let get_or_failwith = Pool_common.Utils.get_or_failwith

let welcome_text_en =
  {|We conduct economic studies in our labs on a regular basis. Most of these take place in a computer lab where participants sit at a computer and answer questions or take decisions on given situations. In addition to our computer lab studies, we also run studies using technological devices, such as fMRI scanners, etc.

Once you are registered in our database and your account is activated, you will receive invitation emails with information on current studies. After receiving an invitation email, you can decide for yourself whether you would like to participate in a study or not. Your data is treated anonymously and we use your data only for study purposes. In order to participate you must be at least 18 years old.

We look forward to welcoming you soon to one of our upcoming studies.
|}
;;

let welcome_text_de =
  {|Wir führen regelmässig wirtschaftswissenschaftliche Studien durch. Meist finden die Studien in einem Computerlabor statt, wo Sie an einem Computer sitzen und Fragen beantworten oder Entscheidungen zu gegebenen Situationen treffen. Darüberhinaus führen wir auch Studien mit anderen technischen Hilfsmitteln (bsp. FMRI, EEG) durch.

Wenn Sie sich bei uns registrieren und Ihr Account freigeschaltet wurde, erhalten Sie Einladungsemails mit Informationen zu aktuellen Studien. Nun können Sie selbst bestimmen, ob Sie gerne bei der betreffenden Studie mitmachen wollen. Die Studien werden anonym durchgeführt und nur für die wissenschaftliche Forschungsarbeit genutzt. Um an unseren Studien teilzunehmen, müssen Sie mindestens 18 Jahre alt sein.

Wir freuen uns Sie bald bei einer unserer Studien begrüssen zu dürfen.
|}
;;

let default_values =
  [ "credits_text", [ "EN", "<h2>Credits</h2>"; "DE", "<h2>Impressum</h2>" ]
  ; "greetings_text", [ "EN", "Greetings Text "; "DE", "Begrüssungstext" ]
  ; "welcome_text", [ "EN", welcome_text_en; "DE", welcome_text_de ]
  ; ( "password_policy_text"
    , [ ( "EN"
        , "The password must be at least 8 characters long, contain a number, \
           a capital letter and a special character." )
      ; ( "DE"
        , "Das Passwort muss mindestens 8 Zeichen lang sein, eine Zahl, ein \
           Grossbuchstabe und ein Sonderzeichen enthalten." )
      ] )
  ]
  |> CCList.map (fun (key, data) ->
       let key = key |> Key.create |> get_or_failwith in
       CCList.map
         (fun (language, content) ->
           create
             key
             (language |> Pool_common.Language.create |> get_or_failwith)
             (content |> Content.create |> get_or_failwith))
         data)
  |> CCList.flatten
;;
