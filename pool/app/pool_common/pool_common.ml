include Entity
module Message = Entity_message
module I18n = Entity_i18n
module Repo = Repo

module Utils = struct
  include Pool_common_utils

  let to_string lang =
    match lang with
    | Language.De -> Locales_de.to_string
    | Language.En -> Locales_en.to_string
  ;;

  let info_to_string lang =
    match lang with
    | Language.De -> Locales_de.info_to_string
    | Language.En -> Locales_en.info_to_string
  ;;

  let success_to_string lang =
    match lang with
    | Language.De -> Locales_de.success_to_string
    | Language.En -> Locales_en.success_to_string
  ;;

  let warning_to_string lang =
    match lang with
    | Language.De -> Locales_de.warning_to_string
    | Language.En -> Locales_en.warning_to_string
  ;;

  let error_to_string lang =
    match lang with
    | Language.De -> Locales_de.error_to_string
    | Language.En -> Locales_en.error_to_string
  ;;

  let field_to_string lang =
    match lang with
    | Language.De -> Locales_de.field_to_string
    | Language.En -> Locales_en.field_to_string
  ;;

  let control_to_string lang =
    match lang with
    | Language.De -> Locales_de.control_to_string
    | Language.En -> Locales_en.control_to_string
  ;;

  let text_to_string lang =
    match lang with
    | Language.De -> I18n_de.to_string
    | Language.En -> I18n_en.to_string
  ;;
end
