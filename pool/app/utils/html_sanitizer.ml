(* Allowlist based sanitizer for user provided rich text (e.g. CKEditor output).

   Unknown elements are unwrapped (their children are kept), elements whose
   content is dangerous or meaningless as text are dropped including their
   children. Only allowlisted attributes survive, and URL attributes must use
   a safe scheme. *)

let html_ns = Markup.Ns.html

let allowed_elements =
  [ "a"
  ; "b"
  ; "blockquote"
  ; "br"
  ; "caption"
  ; "code"
  ; "col"
  ; "colgroup"
  ; "div"
  ; "em"
  ; "figcaption"
  ; "figure"
  ; "h1"
  ; "h2"
  ; "h3"
  ; "h4"
  ; "h5"
  ; "h6"
  ; "hr"
  ; "i"
  ; "img"
  ; "li"
  ; "mark"
  ; "ol"
  ; "p"
  ; "pre"
  ; "s"
  ; "small"
  ; "span"
  ; "strong"
  ; "sub"
  ; "sup"
  ; "table"
  ; "tbody"
  ; "td"
  ; "tfoot"
  ; "th"
  ; "thead"
  ; "tr"
  ; "u"
  ; "ul"
  ]
;;

(* Elements removed together with their content *)
let dropped_elements =
  [ "applet"
  ; "audio"
  ; "base"
  ; "button"
  ; "canvas"
  ; "embed"
  ; "form"
  ; "frame"
  ; "frameset"
  ; "head"
  ; "iframe"
  ; "input"
  ; "link"
  ; "math"
  ; "meta"
  ; "noembed"
  ; "noframes"
  ; "noscript"
  ; "object"
  ; "option"
  ; "script"
  ; "select"
  ; "style"
  ; "svg"
  ; "template"
  ; "textarea"
  ; "title"
  ; "video"
  ; "xmp"
  ]
;;

let global_attributes = [ "class"; "dir"; "lang"; "title" ]

let element_attributes = function
  | "a" -> [ "href"; "name"; "rel"; "target" ]
  | "img" -> [ "alt"; "height"; "src"; "width" ]
  | "td" | "th" -> [ "colspan"; "rowspan"; "scope" ]
  | "col" | "colgroup" -> [ "span" ]
  | "ol" -> [ "reversed"; "start"; "type" ]
  | "li" -> [ "value" ]
  | _ -> []
;;

let mem = CCList.mem ~eq:CCString.equal

(* Browsers ignore ASCII whitespace and control characters when parsing the
   scheme, e.g. "java\tscript:" is treated as "javascript:" *)
let normalize_url url =
  url |> CCString.filter (fun c -> Char.code c > 0x20 && c <> '\x7f')
;;

let scheme url =
  let terminates_scheme = function
    | '/' | '?' | '#' -> true
    | _ -> false
  in
  let rec find i =
    if i >= CCString.length url
    then None
    else (
      match url.[i] with
      | ':' -> Some (CCString.sub url 0 i |> CCString.lowercase_ascii)
      | c when terminates_scheme c -> None
      | _ -> find (i + 1))
  in
  find 0
;;

let is_safe_url ~element url =
  let url = normalize_url url in
  match scheme url, element with
  | None, _ -> true
  | Some ("http" | "https"), _ -> true
  | Some ("mailto" | "tel"), "a" -> true
  | Some "data", "img" ->
    let url = CCString.lowercase_ascii url in
    [ "data:image/png;"; "data:image/jpeg;"; "data:image/gif;"; "data:image/webp;" ]
    |> CCList.exists (fun prefix -> CCString.prefix ~pre:prefix url)
  | Some _, _ -> false
;;

let sanitize_attributes element attributes =
  let allowed = global_attributes @ element_attributes element in
  let attributes =
    attributes
    |> CCList.filter_map (fun ((ns, name), value) ->
      let name = CCString.lowercase_ascii name in
      match CCString.is_empty ns && mem name allowed with
      | false -> None
      | true ->
        (match name with
         | ("href" | "src") when not (is_safe_url ~element value) -> None
         | "target" when not (mem value [ "_blank"; "_self" ]) -> None
         | _ -> Some (("", name), value)))
  in
  (* Prevent reverse tabnabbing *)
  match element with
  | "a" when CCList.exists (fun ((_, name), _) -> name = "target") attributes ->
    CCList.filter (fun ((_, name), _) -> name <> "rel") attributes
    @ [ ("", "rel"), "noopener noreferrer" ]
  | _ -> attributes
;;

type action =
  | Keep
  | Unwrap
  | Drop

let sanitize html =
  let open Markup in
  let rec go acc stack = function
    | [] -> CCList.rev acc
    | signal :: tl ->
      (match stack, signal with
       | Drop :: _, `Start_element _ -> go acc (Drop :: stack) tl
       | Drop :: rest, `End_element -> go acc rest tl
       | Drop :: _, _ -> go acc stack tl
       | _, `Start_element ((ns, name), attributes) ->
         let name = CCString.lowercase_ascii name in
         if (not (CCString.equal ns html_ns)) || mem name dropped_elements
         then go acc (Drop :: stack) tl
         else if mem name allowed_elements
         then (
           let signal =
             `Start_element ((ns, name), sanitize_attributes name attributes)
           in
           go (signal :: acc) (Keep :: stack) tl)
         else go acc (Unwrap :: stack) tl
       | Keep :: rest, `End_element -> go (signal :: acc) rest tl
       | Unwrap :: rest, `End_element -> go acc rest tl
       | [], `End_element -> go acc stack tl
       | _, `Text _ -> go (signal :: acc) stack tl
       | _, (`Comment _ | `Doctype _ | `PI _ | `Xml _) -> go acc stack tl)
  in
  string html
  |> parse_html ~context:(`Fragment "div")
  |> signals
  |> to_list
  |> go [] []
  |> of_list
  |> write_html
  |> to_string
;;
