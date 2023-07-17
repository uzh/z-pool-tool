open Tyxml.Html

let create data_attribs =
  div [ div ~a:[ a_id "calendar-notification" ] []; div ~a:data_attribs [] ]
;;
