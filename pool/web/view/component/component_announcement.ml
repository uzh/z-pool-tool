open Tyxml.Html
open Announcement

let make language announcement =
  let text = Text.find language announcement.text in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "gap" ] ]
    [ div ~a:[ a_class [ "notification"; "error" ] ] [ Unsafe.data text ] ]
;;
