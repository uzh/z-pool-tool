open Tyxml

let note title info ~message =
  let html =
    let of_string value = [ Html.txt value ] in
    [%html
      {|
        <div class="columns is-centered">
          <div class="column is-half-desktop is-full-mobile content">
            <h1 class="title">|}
        (title |> of_string)
        {|</h1>
            <p>|}
        (info |> of_string)
        {|</p>
          </div>
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

let error_general () =
  note
    "Something went wrong"
    "An error occurred. Please clear the browser's cache and cookies and \
     refresh the page."
    ~message:None
;;
