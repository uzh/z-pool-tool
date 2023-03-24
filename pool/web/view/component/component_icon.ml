open Tyxml.Html

let prt = Utils.ppx_printer

type t =
  | Add [@name "add"] [@printer prt "add"]
  | Calendar [@name "calendar"] [@printer prt "calendar"]
  | CalendarOutline [@name "calendar-outline"] [@printer prt "calendar-outline"]
  | Close [@name "close"] [@printer prt "close"]
  | CloseCircle [@name "close-circle"] [@printer prt "close-circle"]
  | Create [@name "create"] [@printer prt "create"]
  | CreateOutline [@name "create-outline"] [@printer prt "create-outline"]
  | Earth [@name "earth"] [@printer prt "earth"]
  | EarthOutline [@name "earth-outline"] [@printer prt "earth-outline"]
  | Eye [@name "eye"] [@printer prt "eye"]
  | EyeOutline [@name "eye-outline"] [@printer prt "eye-outline"]
  | Help [@name "help"] [@printer prt "help"]
  | HelpOutline [@name "help-outline"] [@printer prt "help-outline"]
  | Leaf [@name "leaf"] [@printer prt "leaf"]
  | LeafOutline [@name "leaf-outline"] [@printer prt "leaf-outline"]
  | Location [@name "location"] [@printer prt "location"]
  | LocationOutline [@name "location-outline"] [@printer prt "location-outline"]
  | Mail [@name "mail"] [@printer prt "mail"]
  | MailOutline [@name "mail-outline"] [@printer prt "mail-outline"]
  | MenuOutline [@name "menu-outline"] [@printer prt "menu-outline"]
  | NextCircleOutline [@name "next-circle-outline"]
      [@printer prt "next-circle-outline"]
  | NextOutline [@name "next-outline"] [@printer prt "next-outline"]
  | Person [@name "person"] [@printer prt "person"]
  | PersonOutline [@name "person-outline"] [@printer prt "person-outline"]
  | PrevCircleOutline [@name "prev-circle-outline"]
      [@printer prt "prev-circle-outline"]
  | PrevOutline [@name "prev-outline"] [@printer prt "prev-outline"]
  | RefreshOutline [@name "refresh-outline"] [@printer prt "refresh-outline"]
  | Save [@name "save"] [@printer prt "save"]
  | SaveOutline [@name "save-outline"] [@printer prt "save-outline"]
  | School [@name "school"] [@printer prt "school"]
  | SchoolOutline [@name "school-outline"] [@printer prt "school-outline"]
  | Sort [@name "sort"] [@printer prt "sort"]
  | SortOutline [@name "sort-outline"] [@printer prt "sort-outline"]
  | Trash [@name "trash"] [@printer prt "trash"]
  | TrashOutline [@name "trash-outline"] [@printer prt "trash-outline"]
  | UploadOutline [@name "upload-outline"] [@printer prt "upload-outline"]
[@@deriving eq, show]

let to_html ?(classnames = []) icon =
  i ~a:[ a_class ([ Format.asprintf "icon-%s" (show icon) ] @ classnames) ] []
;;

let icon ?(classnames = []) icon_type =
  (match icon_type with
   | `Add -> "add"
   | `CalendarOutline -> "calendar-outline"
   | `Calendar -> "calendar"
   | `CreateOutline -> "create-outline"
   | `Close -> "close"
   | `CloseCircle -> "close-circle"
   | `Create -> "create"
   | `Earth -> "earth"
   | `EarthOutline -> "earth-outline"
   | `Eye -> "eye"
   | `EyeOutline -> "eye-outline"
   | `Help -> "help"
   | `HelpOutline -> "help-outline"
   | `Leaf -> "leaf"
   | `LeafOutline -> "leaf-outline"
   | `Location -> "location"
   | `LocationOutline -> "location-outline"
   | `Mail -> "mail"
   | `MailOutline -> "mail-outline"
   | `MenuOutline -> "menu-outline"
   | `NextCircleOutline -> "next-circle-outline"
   | `NextOutline -> "next-outline"
   | `Person -> "person"
   | `PersonOutline -> "person-outline"
   | `PrevCircleOutline -> "prev-circle-outline"
   | `PrevOutline -> "prev-outline"
   | `RefreshOutline -> "refresh-outline"
   | `Save -> "save"
   | `SaveOutline -> "save-outline"
   | `School -> "school"
   | `SchoolOutline -> "school-outline"
   | `Sort -> "sort"
   | `SortOutline -> "sort-outline"
   | `TrashOutline -> "trash-outline"
   | `Trash -> "trash"
   | `UploadOutline -> "upload-outline")
  |> fun icon_class ->
  i ~a:[ a_class ([ Format.asprintf "icon-%s" icon_class ] @ classnames) ] []
;;
