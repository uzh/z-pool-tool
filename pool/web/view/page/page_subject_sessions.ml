open Tyxml.Html

let public_overview sessions =
  CCList.map
    (fun (session : Session.t) ->
      div
        ~a:[ a_class [ "flexrow" ] ]
        [ div
            [ txt
                (session.Session.start
                |> Session.Start.value
                |> Pool_common.Utils.Time.formatted_date_time)
            ]
        ])
    sessions
;;
