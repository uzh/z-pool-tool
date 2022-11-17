open Tyxml.Html

let aspect_ratio ?(contain = false) ratio img =
  let ratio =
    match ratio with
    | `R16x9 -> "ratio-16x9"
    | `R4x3 -> "ratio-4x3"
    | `R2x1 -> "ratio-2x1"
    | `R3x1 -> "ratio-3x1"
  in
  let classnames = [ "aspect-ratio"; ratio ] in
  let classnames = if contain then "contain" :: classnames else classnames in
  div ~a:[ a_class classnames ] [ img ]
;;
