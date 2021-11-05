module Common = Common_user
module Email = Common.Email

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
    | _ -> Error Pool_common.Error.(Invalid RecruitmentChannel)
  ;;

  let to_string = function
    | Friend -> "friend"
    | Online -> "online"
    | Lecture -> "lecture"
    | Mailing -> "mailing"
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder of_string Error.RecruitmentChannel)
      (fun l -> [ to_string l ])
      "recruitment_channel"
  ;;

  let all () = CCList.map to_string [ Friend; Online; Lecture; Mailing ]
end

type t =
  { user : Sihl_user.t
        [@equal fun m k -> String.equal m.Sihl_user.id k.Sihl_user.id]
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Common.TermsAccepted.t
  ; paused : Common.Paused.t
  ; disabled : Common.Disabled.t
  ; verified : Common.Verified.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

module Duplicate = struct
  type t =
    { first : t
    ; second : t
    ; ignored_at : Ptime.t option
    }
  [@@deriving eq, show]
end
