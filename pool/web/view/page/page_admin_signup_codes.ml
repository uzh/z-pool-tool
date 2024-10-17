open Tyxml.Html

let list Pool_context.{ language; _ } (admins, query) =
  let open Signup_code in
  let url = Uri.of_string Http_utils.Url.Admin.Settings.signup_codes_path in
  let data_table =
    Component.DataTable.create_meta ~search:searchable_by url query language
  in
  let cols =
    [ `column column_code
    ; `column column_signup_count
    ; `column column_verification_count
    ]
  in
  let row { code; signup_count; verification_count; _ } =
    [ Code.value code
    ; Count.value signup_count |> CCInt.to_string
    ; Count.value verification_count |> CCInt.to_string
    ]
    |> CCList.map (fun x -> td [ txt x ])
    |> tr
  in
  Component.DataTable.make
    ~target_id:"signup-codes-list"
    ~cols
    ~row
    data_table
    admins
;;

let index (Pool_context.{ language; _ } as context) codes =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.SignupCodes) ]
    ; list context codes
    ]
;;
