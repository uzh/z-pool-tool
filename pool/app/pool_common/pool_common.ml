include Entity
module Message = Entity_message
module Repo = Repo

module Utils = struct
  include Pool_common_utils

  let pool_to_ctx pool = [ "pool", Entity.Database.Label.value pool ]

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
end

module Database = struct
  include Database

  let root = Label.of_string "root"
end
