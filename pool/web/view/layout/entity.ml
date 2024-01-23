module I18n = Pool_common.I18n
module Icon = Component.Icon
module Language = Pool_common.Language

type raw =
  | Single of string * I18n.nav_link * Guard.ValidationSet.t
  | Parent of string option * I18n.nav_link * Guard.ValidationSet.t * raw list

let validation_set = function
  | Single (_, _, validation_set) | Parent (_, _, validation_set, _) ->
    validation_set
;;

module NavElement = struct
  type t =
    { url : string option
    ; label : I18n.nav_link
    ; icon : Icon.t option
    ; validation_set : Guard.ValidationSet.t
    ; children : t list
    }

  let rec create ?icon =
    let build
      ?icon
      ?(children = [])
      ?(validation_set = Guard.ValidationSet.empty)
      ?url
      label
      =
      { icon; children; validation_set; url; label }
    in
    function
    | Single (url, label, validation_set) ->
      build ?icon ~validation_set ~url label
    | Parent (url, label, validation_set, children) ->
      let children = children |> CCList.map create in
      build ~children ~validation_set ?url label
  ;;

  let login ?(prefix = "") () =
    create
      (Single
         ( Format.(asprintf "%s/login" prefix)
         , I18n.Login
         , Guard.ValidationSet.empty ))
  ;;

  let logout ?(prefix = "") () =
    create
      (Single
         ( Format.(asprintf "%s/logout" prefix)
         , I18n.Logout
         , Guard.ValidationSet.empty ))
  ;;
end
