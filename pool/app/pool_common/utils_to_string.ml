open Entity

let to_string = function
  | Language.De -> Locales_de.to_string
  | Language.En -> Locales_en.to_string
;;

let info_to_string = function
  | Language.De -> Locales_de.info_to_string
  | Language.En -> Locales_en.info_to_string
;;

let success_to_string = function
  | Language.De -> Locales_de.success_to_string
  | Language.En -> Locales_en.success_to_string
;;

let warning_to_string = function
  | Language.De -> Locales_de.warning_to_string
  | Language.En -> Locales_en.warning_to_string
;;

let error_to_string = function
  | Language.De -> Locales_de.error_to_string
  | Language.En -> Locales_en.error_to_string
;;

(* TODO[timhub]: Can we capitalize this globally? *)
let field_to_string = function
  | Language.De -> Locales_de.field_to_string
  | Language.En -> Locales_en.field_to_string
;;

let confirmable_to_string = function
  | Language.De -> I18n_de.confirmable_to_string
  | Language.En -> I18n_en.confirmable_to_string
;;

let field_to_string_capitalized l m =
  field_to_string l m |> CCString.capitalize_ascii
;;

let control_to_string = function
  | Language.De -> Locales_de.control_to_string
  | Language.En -> Locales_en.control_to_string
;;

let text_to_string = function
  | Language.De -> I18n_de.to_string
  | Language.En -> I18n_en.to_string
;;

let nav_link_to_string = function
  | Language.De -> I18n_de.nav_link_to_string
  | Language.En -> I18n_en.nav_link_to_string
;;

let hint_to_string = function
  | Language.De -> I18n_de.hint_to_string
  | Language.En -> I18n_en.hint_to_string
;;

let get_or_failwith m =
  m
  |> CCResult.map_err (error_to_string Language.En)
  |> CCResult.get_or_failwith
;;

let failwith = CCFun.(error_to_string Language.En %> failwith)

let bool_to_string lang m =
  match lang with
  | Language.De -> if m then "Ja" else "Nein"
  | Language.En -> if m then "Yes" else "No"
;;
