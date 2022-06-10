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

module WaitingListDisabled = struct
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
      Common.Message.Field.WaitingListDisabled
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
  ; description : Description.t
  ; filter : string
  ; mailings : Mailing.t list
  ; waiting_list_disabled : WaitingListDisabled.t
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

let create
    ?id
    title
    description
    waiting_list_disabled
    direct_registration_disabled
    registration_disabled
  =
  { id = id |> CCOption.value ~default:(Id.create ())
  ; title
  ; description
  ; filter = "1=1"
  ; mailings = []
  ; waiting_list_disabled
  ; direct_registration_disabled
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  ; registration_disabled
  }
;;

let title_value (m : t) = Title.value m.title
let description_value (m : t) = Description.value m.description

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; description : Description.t
    ; waiting_list_disabled : WaitingListDisabled.t
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    }
  [@@deriving eq, show]
end

let waiting_list_disabled_value (m : t) =
  WaitingListDisabled.value m.waiting_list_disabled
;;

let direct_registration_disabled_value (m : t) =
  DirectRegistrationDisabled.value m.direct_registration_disabled
;;

let registration_disabled_value (m : t) =
  RegistrationDisabled.value m.registration_disabled
;;
