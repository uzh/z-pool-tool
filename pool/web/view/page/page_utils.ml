open Tyxml

let note title info ~message =
  let html =
    let of_string value = [ Html.txt value ] in
    [%html
      {|
          <div>
            <h1>|}
        (title |> of_string)
        {|</h1>
            <p>|}
        (info |> of_string)
        {|</p>
          </div>
      |}]
  in
  Page_layout.create ~children:[ html ] ~message ()
;;

let error_page_not_found () =
  note
    "Page not found"
    "An error occurred. The requested page could not be found."
    ~message:None
;;
