let get_or_failwith = Pool_common.Utils.get_or_failwith

let sns_fmri_description =
  {|Deutsch:
Betreten Sie das UniversitätsSpital am Haupteingang (Vis-à-vis ETH Hauptgebäude beim goldenen Mann). Gehen Sie den Gang bis ganz hinten und fahren Sie mit einem der drei violetten Lifte (die Sie dann rechter Hand sehen) 2 Stockwerke nach unten in den Stock V. Hier ist bereits im Lift das SNS Labor gut sichtbar angeschrieben.
Entsprechend der Beschilderung zum SNS Labor gehen Sie dann zweimal rechts, bis Sie am Ende eines etwas nach unten verlaufenden Korridors den Eingang des SNS-Lab/MR-Zentrum-Süd erreichen.

Mit dem dort zugänglichen Telefon können Sie mittels der internen Nummer (546 82) Ihre Ankunft bekanntgeben.

English:
Enter the University Hospital through the main entrance (located opposite ETH’s main building on Rämistrasse). Continue down the hall until you see the stairs and purple elevators. Take the elevator down to floor “V”. Get off the elevator and follow the signs to the SNS Lab: turn right twice, the SNS-Lab/MR-Zentrum-Süd is at the end of the hall.

Please call the in-house number (546 82) to announce your arrival.|}
;;

let sns_behavioural_description =
  {|Deutsch:
Betreten Sie das UniversitätsSpital am Haupteingang (Vis-à-vis ETH Hauptgebäude beim goldenen Mann). Gehen Sie den Gang bis ganz hinten und fahren Sie mit einem der drei violetten Lifte (die Sie dann rechter Hand sehen) 2 Stockwerke nach unten in den Stock V. Hier ist bereits im Lift das SNS Labor gut sichtbar angeschrieben.
Entsprechend der Beschilderung zum SNS Labor gehen Sie dann zweimal rechts, bis Sie am Ende eines etwas nach unten verlaufenden Korridors den Eingang des SNS-Lab/MR-Zentrum-Süd erreichen.

Mit dem dort zugänglichen Telefon können Sie mittels der internen Nummer (546 83) Ihre Ankunft bekanntgeben.

English:
Enter the University Hospital through the main entrance (located opposite ETH’s main building on Rämistrasse). Continue down the hall until you see the stairs and purple elevators. Take the elevator down to floor “V”. Get off the elevator and follow the signs to the SNS Lab: turn right twice, the SNS-Lab/MR-Zentrum-Süd is at the end of the hall.

Please call the in-house number (546 83) to announce your arrival.
|}
;;

let locations =
  let open Pool_location in
  [ ( "ETH Decision Science Laboratory"
    , Some "Tram 6, 7, 10 or 15 to Haldenegg"
    , Some (None, Some "Stockwerk A", Some "IFW", "Haldeneggsteig 4", "8092", "Zurich")
    , Some "https://www.uast.uzh.ch/assets/map-descil.jpg"
    , Status.Active )
  ; ( "SNS Lab fMRI"
    , Some sns_fmri_description
    , Some (Some "USZ", Some "SNS Labor", None, "Rämistrasse 100", "8091", "Zürich")
    , Some "https://www.uast.uzh.ch/assets/map-usz.jpg"
    , Status.Active )
  ; ( "SNS Behavioral Lab"
    , Some sns_behavioural_description
    , Some (Some "USZ", Some "SNS Labor", None, "Rämistrasse 100", "8091", "Zürich")
    , Some "https://www.uast.uzh.ch/assets/map-usz.jpg"
    , Status.Active )
  ]
  |> CCList.map (fun (label, description, address, link, status) ->
    let description =
      description
      |> CCOption.map (fun value ->
        Pool_common.Language.all
        |> CCList.map (fun lang -> lang, value)
        |> Description.create Pool_common.Language.all
        |> CCResult.get_exn)
    in
    let address =
      match address with
      | Some (institution, room, building, street, zip, city) ->
        Address.Mail.create institution room building street zip city
        |> Pool_common.Utils.get_or_failwith
        |> Address.physical
      | None -> Address.Virtual
    in
    create label description address link status |> Pool_common.Utils.get_or_failwith)
;;

let create pool =
  let open Pool_location in
  default_values @ locations |> CCList.map created |> Lwt_list.iter_s (handle_event pool)
;;
