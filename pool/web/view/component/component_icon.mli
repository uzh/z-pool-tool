val prt : string -> Format.formatter -> 'a -> unit

type t =
  | Add
  | Calendar
  | CalendarOutline
  | CaretDown
  | CaretUp
  | Checkmark
  | CheckmarkCircle
  | CheckmarkCircleOutline
  | ChevronBack
  | ChevronBackCircle
  | ChevronForward
  | ChevronForwardCircle
  | Close
  | CloseCircle
  | Create
  | CreateOutline
  | Download
  | DownloadOutline
  | Earth
  | EarthOutline
  | EllipsisVertical
  | EllipsisVerticalOutline
  | Eye
  | EyeOutline
  | Help
  | HelpOutline
  | Leaf
  | LeafOutline
  | Location
  | LocationOutline
  | Mail
  | MailAlert
  | MailError
  | MailOutline
  | MenuOutline
  | NextCircleOutline
  | NextOutline
  | Notifications
  | NotificationsOff
  | NotificationsOffOutline
  | NotificationsOutline
  | OpenOutline
  | Person
  | PersonOutline
  | PrevCircleOutline
  | PrevOutline
  | Print
  | PrintOutline
  | RefreshOutline
  | ReorderThree
  | ReorderTwo
  | Save
  | SaveOutline
  | School
  | SchoolOutline
  | Settings
  | SettingsOutline
  | SwapHorizonal
  | Trash
  | TrashOutline
  | UploadOutline

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val to_html
  :  ?title:string
  -> ?classnames:string list
  -> t
  -> [> Html_types.i ] Tyxml_html.elt
