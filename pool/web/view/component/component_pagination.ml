open Tyxml.Html
open Query

(* TODO: Finish *)
let[@warning "-27"] create { Pagination.page; page_count; _ } =
  let open Pagination in
  div
    ~a:[ a_class [ "flexrow" ] ]
    (* (CCList.range 0 (PageCount.value page_count) *)
    (CCList.range 0 2
    |> CCList.map (fun i ->
         if CCInt.equal i (Page.value page)
         then span [ txt (CCInt.to_string i) ]
         else a ~a:[ a_href "" ] [ txt (CCInt.to_string i) ]))
;;
