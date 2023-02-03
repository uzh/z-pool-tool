open Tyxml.Html

let icon ?(classnames = []) icon_type =
  (match icon_type with
   | `Add -> "add"
   | `CalendarOutline -> "calendar-outline"
   | `Calendar -> "calendar"
   | `CreateOutline -> "create-outline"
   | `Create -> "create"
   | `Close -> "close"
   | `CloseCircle -> "close-circle"
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
   | `Person -> "person"
   | `PersonOutline -> "person-outline"
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
