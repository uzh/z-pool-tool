open Tyxml.Html

let icon icon_type =
  (match icon_type with
   | `Add -> "add"
   | `CalendarOutline -> "calendar-outline"
   | `Calendar -> "calendar"
   | `CreateOutline -> "create-outline"
   | `Create -> "create"
   | `Close -> "close"
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
   | `Save -> "save"
   | `SaveOutline -> "save-outline"
   | `SchoolOutline -> "school-outline"
   | `School -> "school"
   | `SortOutline -> "sort-outline"
   | `Sort -> "sort"
   | `TrashOutline -> "trash-outline"
   | `Trash -> "trash"
   | `UploadOutline -> "upload-outline")
  |> fun icon_class ->
  i ~a:[ a_class [ Format.asprintf "icon-%s" icon_class ] ] []
;;
