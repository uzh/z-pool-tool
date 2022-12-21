let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ~indent:true ()) html
;;
