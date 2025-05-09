open Tyxml.Html

let prt = Utils.ppx_printer

type t =
  | Add [@name "add"] [@printer prt "add"]
  | Calendar [@name "calendar"] [@printer prt "calendar"]
  | CalendarOutline [@name "calendar-outline"] [@printer prt "calendar-outline"]
  | CaretDown [@name "caret-down"] [@printer prt "caret-down"]
  | CaretUp [@name "caret-up"] [@printer prt "caret-up"]
  | Checkmark [@name "checkmark"] [@printer prt "checkmark"]
  | CheckmarkCircle [@name "checkmark-circle"] [@printer prt "checkmark-circle"]
  | CheckmarkCircleOutline [@name "checkmark-circle-outline"]
  [@printer prt "checkmark-circle-outline"]
  | ChevronBack [@name "chevron-back-outline"] [@printer prt "chevron-back-outline"]
  | ChevronBackCircle [@name "chevron-back-circle-outline"]
  [@printer prt "chevron-back-circle-outline"]
  | ChevronForward [@name "chevron-forward-outline"]
  [@printer prt "chevron-forward-outline"]
  | ChevronForwardCircle [@name "chevron-forward-circle-outline"]
  [@printer prt "chevron-forward-circle-outline"]
  | Close [@name "close"] [@printer prt "close"]
  | CloseCircle [@name "close-circle"] [@printer prt "close-circle"]
  | Create [@name "create"] [@printer prt "create"]
  | CreateOutline [@name "create-outline"] [@printer prt "create-outline"]
  | Download [@name "download"] [@printer prt "download"]
  | DownloadOutline [@name "download-outline"] [@printer prt "download-outline"]
  | Earth [@name "earth"] [@printer prt "earth"]
  | EarthOutline [@name "earth-outline"] [@printer prt "earth-outline"]
  | EllipsisVertical [@name "ellipsis-vertical"] [@printer prt "ellipsis-vertical"]
  | EllipsisVerticalOutline [@name "ellipsis-vertical-outline"]
  [@printer prt "ellipsis-vertical-outline"]
  | Eye [@name "eye"] [@printer prt "eye"]
  | EyeOutline [@name "eye-outline"] [@printer prt "eye-outline"]
  | Help [@name "help"] [@printer prt "help"]
  | HelpOutline [@name "help-outline"] [@printer prt "help-outline"]
  | InformationCircle [@name "information-circle-outline"]
  [@printer prt "information-circle-outline"]
  | InformationOutline [@name "information-outline"] [@printer prt "information-outline"]
  | Leaf [@name "leaf"] [@printer prt "leaf"]
  | LeafOutline [@name "leaf-outline"] [@printer prt "leaf-outline"]
  | Location [@name "location"] [@printer prt "location"]
  | LocationOutline [@name "location-outline"] [@printer prt "location-outline"]
  | Mail [@name "mail"] [@printer prt "mail"]
  | MailAlert [@name "mail-alert"] [@printer prt "mail-alert"]
  | MailError [@name "mail-error"] [@printer prt "mail-error"]
  | MailOutline [@name "mail-outline"] [@printer prt "mail-outline"]
  | MenuOutline [@name "menu-outline"] [@printer prt "menu-outline"]
  | NextCircleOutline [@name "next-circle-outline"] [@printer prt "next-circle-outline"]
  | NextOutline [@name "next-outline"] [@printer prt "next-outline"]
  | Notifications [@name "notifications"] [@printer prt "notifications"]
  | NotificationsOff [@name "notifications-off"] [@printer prt "notifications-off"]
  | NotificationsOffOutline [@name "notifications-off-outline"]
  [@printer prt "notifications-off-outline"]
  | NotificationsOutline [@name "notifications-outline"]
  [@printer prt "notifications-outline"]
  | OpenOutline [@name "open-outline"] [@printer prt "open-outline"]
  | Person [@name "person"] [@printer prt "person"]
  | PersonOutline [@name "person-outline"] [@printer prt "person-outline"]
  | PrevCircleOutline [@name "prev-circle-outline"] [@printer prt "prev-circle-outline"]
  | PrevOutline [@name "prev-outline"] [@printer prt "prev-outline"]
  | Print [@name "print"] [@printer prt "print"]
  | PrintOutline [@name "print-outline"] [@printer prt "print-outline"]
  | RefreshOutline [@name "refresh-outline"] [@printer prt "refresh-outline"]
  | ReorderThree [@name "reorder-three"] [@printer prt "reorder-three"]
  | ReorderTwo [@name "reorder-two"] [@printer prt "reorder-two"]
  | Save [@name "save"] [@printer prt "save"]
  | SaveOutline [@name "save-outline"] [@printer prt "save-outline"]
  | School [@name "school"] [@printer prt "school"]
  | SchoolOutline [@name "school-outline"] [@printer prt "school-outline"]
  | Settings [@name "settings"] [@printer prt "settings"]
  | SettingsOutline [@name "settings-outline"] [@printer prt "settings-outline"]
  | SwapHorizonal [@name "swap-horizontal"] [@printer prt "swap-horizontal"]
  | Trash [@name "trash"] [@printer prt "trash"]
  | TrashOutline [@name "trash-outline"] [@printer prt "trash-outline"]
  | UploadOutline [@name "upload-outline"] [@printer prt "upload-outline"]
[@@deriving eq, show]

let to_html ?title ?(attributes = []) ?(classnames = []) icon =
  let open CCFun in
  let attributes =
    let title = CCOption.map_or ~default:[] (a_title %> CCList.return) title in
    [ a_class ([ Format.asprintf "icon-%s" (show icon) ] @ classnames) ]
    @ title
    @ attributes
  in
  i ~a:attributes []
;;

let bool_to_icon ?(colored = false) ?(outlined = false) ?(classnames = []) = function
  | false ->
    let icon = if outlined then CloseCircle else Close in
    to_html ~classnames:(classnames @ if colored then [ "color-red" ] else []) icon
  | true ->
    let icon = if outlined then CheckmarkCircle else Checkmark in
    to_html ~classnames:(classnames @ if colored then [ "color-green" ] else []) icon
;;
