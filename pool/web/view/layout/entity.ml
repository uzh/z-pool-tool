module I18n = Pool_common.I18n
module Icon = Component.Icon
module Language = Pool_common.Language

module NavElement = struct
  type t =
    { url : string
    ; label : I18n.nav_link
    ; icon : Icon.t option
    ; validation_set : Guard.ValidationSet.t
    ; children : t list
    }

  let create
    ?icon
    ?(children = [])
    ?(validation_set = Guard.ValidationSet.empty)
    url
    label
    =
    { url; label; icon; validation_set; children }
  ;;

  let create_all =
    CCList.map (fun (url, label, icon, children) ->
      create ~children ?icon url label)
  ;;

  let create_all_req = CCList.map (fun (url, label) -> create url label)

  let create_all_req_with_set =
    CCList.map (fun (url, label, validation_set) ->
      create ~validation_set url label)
  ;;

  let login ?(prefix = "") () =
    create Format.(asprintf "%s/login" prefix) I18n.Login
  ;;

  let logout ?(prefix = "") () =
    create Format.(asprintf "%s/logout" prefix) I18n.Logout
  ;;
end
