open CCFun
module Message = Pool_common.Message
module Utils = Pool_common.Utils

let get_error message lang =
  message.Pool_common.Message.Collection.error
  |> CCList.map (Utils.error_to_string lang)
;;

let get_warning message lang =
  message.Pool_common.Message.Collection.warning
  |> CCList.map (Utils.warning_to_string lang)
;;

let get_success message lang =
  message.Pool_common.Message.Collection.success
  |> CCList.map (Utils.success_to_string lang)
;;

let get_info message lang =
  message.Pool_common.Message.Collection.info
  |> CCList.map (Utils.info_to_string lang)
;;

let set ?(error = []) ?(warning = []) ?(success = []) ?(info = []) res =
  let open Pool_common.Message.Collection in
  let message =
    empty
    |> set_error error
    |> set_warning warning
    |> set_success success
    |> set_info info
    |> to_string
  in
  (* We use alerts for all messages *)
  Sihl.Web.Flash.set_alert message res
;;

let error_to_html ?(language = Pool_common.Language.En) =
  let open Tyxml.Html in
  Pool_common.(Utils.error_to_string language) %> txt %> CCList.return %> div
;;
