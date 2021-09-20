open Tyxml

module Message = struct
  let concat_messages txts styles =
    [%html
      {|<div style=|}
        styles
        {|>
         |}
        [ Html.txt (CCString.unlines txts) ]
        {|</div>
      |}]
  ;;

  let match_message message classname =
    match message with
    | [] -> [%html {|<div></div>|}]
    | txts -> concat_messages txts classname
  ;;

  let create ~message () =
    match message with
    | None -> [%html {|<div></div>|}]
    | Some message ->
      let success =
        match_message (Http_utils.Message.get_success message) "color: green;"
      in
      let info =
        match_message (Http_utils.Message.get_info message) "color: blue;"
      in
      let warning =
        match_message (Http_utils.Message.get_warning message) "color: orange;"
      in
      let error =
        match_message (Http_utils.Message.get_error message) "color: red;"
      in
      [%html {|<div>|} [ success; info; warning; error ] {|</div>|}]
  ;;
end

let create ~children ~message () =
  let message = Message.create ~message () in
  [%html
    {|
  <!doctype html>
  <html lang="en">
     <head>
        <meta charset="UTF-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>Pool</title>
     </head>
     <body>
      <main>
      <section>
      |}
      [ message ]
      {|
      </section>
      <section>
      |}
      children
      {|
      </section>
      </main>
     </body>
     </html>
     |}]
;;
