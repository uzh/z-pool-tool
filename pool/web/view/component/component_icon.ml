open Tyxml.Html

let icon icon_type =
  (match icon_type with
  | `Add -> "add"
  | `CalendarOutline -> "calendar-outline"
  | `Calendar -> "calendar"
  | `CreateOutline -> "create-outline"
  | `Create -> "create"
  | `EarthOutline -> "earth-outline"
  | `Earth -> "earth"
  | `HelpOutline -> "help-outline"
  | `Help -> "help"
  | `LeafOutline -> "leaf-outline"
  | `Leaf -> "leaf"
  | `LocationOutline -> "location-outline"
  | `Location -> "location"
  | `MailOutline -> "mail-outline"
  | `Mail -> "mail"
  | `PersonOutline -> "person-outline"
  | `Person -> "person"
  | `SaveOutline -> "save-outline"
  | `Save -> "save"
  | `SchoolOutline -> "school-outline"
  | `School -> "school"
  | `TrashOutline -> "trash-outline"
  | `Trash -> "trash"
  | `UploadOutline -> "upload-outline")
  |> fun icon_class ->
  i ~a:[ a_class [ Format.asprintf "icon-%s" icon_class ] ] []
;;
