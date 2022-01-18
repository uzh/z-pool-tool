include Entity
module Message = Entity_message
module I18n = Entity_i18n
module Repo = Repo

module Utils = struct
  include Pool_common_utils

  let to_string lang =
    match lang with
    | Language.De ->
      let open Locales_de in
      to_string
    | Language.En ->
      let open Locales_en in
      to_string
  ;;

  let info_to_string lang =
    match lang with
    | Language.De ->
      let open Locales_de in
      info_to_string
    | Language.En ->
      let open Locales_en in
      info_to_string
  ;;

  let success_to_string lang =
    match lang with
    | Language.De ->
      let open Locales_de in
      success_to_string
    | Language.En ->
      let open Locales_en in
      success_to_string
  ;;

  let warning_to_string lang =
    match lang with
    | Language.De ->
      let open Locales_de in
      warning_to_string
    | Language.En ->
      let open Locales_en in
      warning_to_string
  ;;

  let error_to_string lang =
    match lang with
    | Language.De ->
      let open Locales_de in
      error_to_string
    | Language.En ->
      let open Locales_en in
      error_to_string
  ;;

  let field_to_string lang =
    match lang with
    | Language.De ->
      let open Locales_de in
      field_to_string
    | Language.En ->
      let open Locales_en in
      field_to_string
  ;;

  let submit_to_string lang =
    match lang with
    | Language.De ->
      let open Locales_de in
      submit_to_string
    | Language.En ->
      let open Locales_en in
      submit_to_string
  ;;

  let text_to_string lang m =
    let open Entity_i18n in
    match lang with
    | Language.De -> De.to_string m
    | Language.En -> En.to_string m
  ;;
end
