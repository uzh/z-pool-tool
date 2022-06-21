module Id = Pool_common.Id
module Common = Pool_common

module Title = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create title =
    if CCString.is_empty title
    then Error Common.Message.(Invalid Field.Title)
    else Ok title
  ;;

  let schema () = Common.(Utils.schema_decoder create value Message.Field.Title)
end

module PublicTitle = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create title =
    if CCString.is_empty title
    then Error Common.Message.(Invalid Field.PublicTitle)
    else Ok title
  ;;

  let schema () =
    Common.(Utils.schema_decoder create value Message.Field.PublicTitle)
  ;;
end

module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create description =
    if CCString.is_empty description
    then Error Pool_common.Message.(Invalid Field.Description)
    else Ok description
  ;;

  let schema () =
    Common.(Utils.schema_decoder create value Message.Field.Description)
  ;;
end

module DirectRegistrationDisabled = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m

  let schema () =
    Pool_common.Utils.schema_decoder
      (fun m ->
        m
        |> bool_of_string_opt
        |> CCOption.get_or ~default:false
        |> CCResult.pure)
      string_of_bool
      Common.Message.Field.DirectRegistrationDisabled
  ;;
end

module RegistrationDisabled = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m

  let schema () =
    Pool_common.Utils.schema_decoder
      (fun m ->
        m
        |> bool_of_string_opt
        |> CCOption.get_or ~default:false
        |> CCResult.pure)
      string_of_bool
      Common.Message.Field.RegistrationDisabled
  ;;
end

type t =
  { id : Id.t
  ; title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t
  ; filter : string
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; session_reminder_text : Pool_common.Reminder.Text.t option
  ; session_reminder_language : Pool_common.Language.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

let create
    ?id
    title
    public_title
    description
    direct_registration_disabled
    registration_disabled
    session_reminder_lead_time
    session_reminder_text
    session_reminder_language
  =
  let open CCResult in
  let* session_reminder_language =
    match session_reminder_text, session_reminder_language with
    | Some _, None -> Error Pool_common.Message.LanguageRequiredIfTextProvided
    | Some _, Some _ -> Ok session_reminder_language
    | _ -> Ok None
  in
  Ok
    { id = id |> CCOption.value ~default:(Id.create ())
    ; title
    ; public_title
    ; description
    ; filter = "1=1"
    ; direct_registration_disabled
    ; registration_disabled
    ; session_reminder_lead_time
    ; session_reminder_text
    ; session_reminder_language
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
;;

let title_value (m : t) = Title.value m.title
let public_title_value (m : t) = PublicTitle.value m.public_title
let description_value (m : t) = Description.value m.description

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; public_title : PublicTitle.t
    ; description : Description.t
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    }
  [@@deriving eq, show]
end

let session_reminder_text_value m =
  m.session_reminder_text |> CCOption.map Pool_common.Reminder.Text.value
;;

let session_reminder_lead_time_value m =
  m.session_reminder_lead_time
  |> CCOption.map Pool_common.Reminder.LeadTime.value
;;

let direct_registration_disabled_value (m : t) =
  DirectRegistrationDisabled.value m.direct_registration_disabled
;;

let registration_disabled_value (m : t) =
  RegistrationDisabled.value m.registration_disabled
;;
