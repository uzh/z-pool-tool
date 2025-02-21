module I18n = Pool_common.I18n
module Icon = Component.Icon
module Language = Pool_common.Language

type layout =
  | Horizonal
  | Vertical

type validation =
  | AlwaysOn
  | OnChildren
  | Set of Guard.ValidationSet.t
[@@deriving show, eq]

type raw =
  | Single of string * I18n.nav_link * validation
  | Parent of string option * I18n.nav_link * validation * raw list

let single url nav_link validation = Single (url, nav_link, validation)

let parent ?url ?(validation = OnChildren) nav_link raw =
  Parent (url, nav_link, validation, raw)
;;

let validation = function
  | Single (_, _, validation) | Parent (_, _, validation, _) -> validation
;;

module NavElement = struct
  type t =
    { url : string option
    ; label : I18n.nav_link
    ; icon : Icon.t option
    ; validation : validation
    ; children : t list
    }
  [@@deriving show, eq]

  let rec create ?icon =
    let build
          ?icon
          ?(children = [])
          ?(validation = Set Guard.ValidationSet.empty)
          ?url
          label
      =
      { icon; children; validation; url; label }
    in
    function
    | Single (url, label, validation) -> build ?icon ~validation ~url label
    | Parent (url, label, validation, children) ->
      let children = children |> CCList.map create in
      build ~children ~validation ?url label
  ;;

  let login ?(prefix = "") () =
    Set Guard.ValidationSet.empty
    |> single Format.(asprintf "%s/login" prefix) I18n.Login
    |> create
  ;;

  let logout ?(prefix = "") () =
    Set Guard.ValidationSet.empty
    |> single Format.(asprintf "%s/logout" prefix) I18n.Logout
    |> create
  ;;
end
