module Common = Pool_user

module RecruitmentChannel = struct
  type t =
    | Friend
    | Online
    | Lecture
    | Mailing
  [@@deriving eq, show]

  let of_string = function
    | "friend" -> Ok Friend
    | "online" -> Ok Online
    | "lecture" -> Ok Lecture
    | "mailing" -> Ok Mailing
    | _ -> Error Pool_common.Message.(Invalid RecruitmentChannel)
  ;;

  let to_string = function
    | Friend -> "friend"
    | Online -> "online"
    | Lecture -> "lecture"
    | Mailing -> "mailing"
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder of_string Message.RecruitmentChannel)
      (fun l -> [ to_string l ])
      "recruitment_channel"
  ;;

  let all () = CCList.map to_string [ Friend; Online; Lecture; Mailing ]
end

type t =
  { user : Sihl_user.t
        [@equal fun m k -> CCString.equal m.Sihl_user.id k.Sihl_user.id]
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Common.TermsAccepted.t
  ; paused : Common.Paused.t
  ; disabled : Common.Disabled.t
  ; verified : Common.Verified.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

module Write = struct
  type t =
    { user_id : Pool_common.Id.t
    ; recruitment_channel : RecruitmentChannel.t
    ; terms_accepted_at : Common.TermsAccepted.t
    ; paused : Common.Paused.t
    ; disabled : Common.Disabled.t
    ; verified : Common.Verified.t
    ; firstname_version : Pool_common.Version.t
    ; lastname_version : Pool_common.Version.t
    ; paused_version : Pool_common.Version.t
    }
  [@@deriving eq, show]

  let create m =
    { user_id = Pool_common.Id.of_string m.user.Sihl.Contract.User.id
    ; recruitment_channel = m.recruitment_channel
    ; terms_accepted_at = m.terms_accepted_at
    ; paused = m.paused
    ; disabled = m.disabled
    ; verified = m.verified
    ; firstname_version = m.firstname_version
    ; lastname_version = m.lastname_version
    ; paused_version = m.paused_version
    }
  ;;
end

let id m = m.user.Sihl_user.id |> Pool_common.Id.of_string

let firstname m =
  m.user.Sihl_user.given_name
  |> CCOption.get_exn_or
       (Format.asprintf "User '%s' has no firstname" m.user.Sihl_user.id)
  |> Common.Firstname.of_string
;;

let lastname m =
  m.user.Sihl_user.name
  |> CCOption.get_exn_or
       (Format.asprintf "User '%s' has no lastname" m.user.Sihl_user.id)
  |> Common.Lastname.of_string
;;

let fullname m =
  Format.asprintf
    "%s %s"
    (m |> firstname |> Common.Firstname.value)
    (m |> lastname |> Common.Lastname.value)
;;

let email_address m = m.user.Sihl_user.email |> Common.EmailAddress.of_string

let version_selector p = function
  | "firstname" -> Some p.firstname_version
  | "lastname" -> Some p.lastname_version
  | "paused" -> Some p.paused_version
  | _ -> None
;;

module Duplicate = struct
  type t =
    { first : t
    ; second : t
    ; ignored_at : Ptime.t option
    }
  [@@deriving eq, show]
end
