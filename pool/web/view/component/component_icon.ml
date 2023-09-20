open Tyxml.Html

let prt = Utils.ppx_printer

type t =
  | Add [@name "add"] [@printer prt "add"]
  | Calendar [@name "calendar"] [@printer prt "calendar"]
  | CalendarOutline [@name "calendar-outline"] [@printer prt "calendar-outline"]
  | Checkmark [@name "checkmark"] [@printer prt "checkmark"]
  | CheckmarkCircle [@name "checkmark-circle"] [@printer prt "checkmark-circle"]
  | CheckmarkCircleOutline [@name "checkmark-circle-outline"]
  [@printer prt "checkmark-circle-outline"]
  | ChevronBackCircle [@name "chevron-back-circle-outline"]
  [@printer prt "chevron-back-circle-outline"]
  | ChevronBack [@name "chevron-back-outline"]
  [@printer prt "chevron-back-outline"]
  | ChevronForwardCircle [@name "chevron-forward-circle-outline"]
  [@printer prt "chevron-forward-circle-outline"]
  | ChevronForward [@name "chevron-forward-outline"]
  [@printer prt "chevron-forward-outline"]
  | Close [@name "close"] [@printer prt "close"]
  | CloseCircle [@name "close-circle"] [@printer prt "close-circle"]
  | Create [@name "create"] [@printer prt "create"]
  | CreateOutline [@name "create-outline"] [@printer prt "create-outline"]
  | Earth [@name "earth"] [@printer prt "earth"]
  | EarthOutline [@name "earth-outline"] [@printer prt "earth-outline"]
  | EllipsisVertical [@name "ellipsis-vertical"]
  [@printer prt "ellipsis-vertical"]
  | EllipsisVerticalOutline [@name "ellipsis-vertical-outline"]
  [@printer prt "ellipsis-vertical-outline"]
  | Eye [@name "eye"] [@printer prt "eye"]
  | EyeOutline [@name "eye-outline"] [@printer prt "eye-outline"]
  | Help [@name "help"] [@printer prt "help"]
  | HelpOutline [@name "help-outline"] [@printer prt "help-outline"]
  | Leaf [@name "leaf"] [@printer prt "leaf"]
  | LeafOutline [@name "leaf-outline"] [@printer prt "leaf-outline"]
  | Location [@name "location"] [@printer prt "location"]
  | LocationOutline [@name "location-outline"] [@printer prt "location-outline"]
  | Mail [@name "mail"] [@printer prt "mail"]
  | MailAlert [@name "mail-alert"] [@printer prt "mail-alert"]
  | MailOutline [@name "mail-outline"] [@printer prt "mail-outline"]
  | MenuOutline [@name "menu-outline"] [@printer prt "menu-outline"]
  | NextCircleOutline [@name "next-circle-outline"]
  [@printer prt "next-circle-outline"]
  | NextOutline [@name "next-outline"] [@printer prt "next-outline"]
  | Notifications [@name "notifications"] [@printer prt "notifications"]
  | NotificationsOutline [@name "notifications-outline"]
  [@printer prt "notifications-outline"]
  | NotificationsOff [@name "notifications-off"]
  [@printer prt "notifications-off"]
  | NotificationsOffOutline [@name "notifications-off-outline"]
  [@printer prt "notifications-off-outline"]
  | Person [@name "person"] [@printer prt "person"]
  | PersonOutline [@name "person-outline"] [@printer prt "person-outline"]
  | PrevCircleOutline [@name "prev-circle-outline"]
  [@printer prt "prev-circle-outline"]
  | PrevOutline [@name "prev-outline"] [@printer prt "prev-outline"]
  | RefreshOutline [@name "refresh-outline"] [@printer prt "refresh-outline"]
  | ReorderThree [@name "reorder-three"] [@printer prt "reorder-three"]
  | ReorderTwo [@name "reorder-two"] [@printer prt "reorder-two"]
  | Save [@name "save"] [@printer prt "save"]
  | SaveOutline [@name "save-outline"] [@printer prt "save-outline"]
  | School [@name "school"] [@printer prt "school"]
  | SchoolOutline [@name "school-outline"] [@printer prt "school-outline"]
  | Trash [@name "trash"] [@printer prt "trash"]
  | TrashOutline [@name "trash-outline"] [@printer prt "trash-outline"]
  | UploadOutline [@name "upload-outline"] [@printer prt "upload-outline"]
[@@deriving eq, show]

let to_html ?(classnames = []) icon =
  i ~a:[ a_class ([ Format.asprintf "icon-%s" (show icon) ] @ classnames) ] []
;;
