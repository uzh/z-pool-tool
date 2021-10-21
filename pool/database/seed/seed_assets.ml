type file =
  { id : string
  ; filename : string
  ; filesize : int
  ; mime : string
  ; body : string
  }

let dummy_css () =
  { id = Uuidm.create `V4 |> Uuidm.to_string
  ; filename = "dummy.css"
  ; filesize = 20
  ; mime = "text/css"
  ; body = "html {font-family: sans-serif;}"
  }
;;
