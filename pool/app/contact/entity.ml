module User = Pool_user

module Sihl_user = struct
  include Sihl_user

  let equal m k = CCString.equal m.id k.id
end

module RecruitmentChannel = struct
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | Friend [@name "friend"] [@printer go "friend"]
    | Online [@name "online"] [@printer go "online"]
    | Lecture [@name "lecture"] [@printer go "lecture"]
    | Mailing [@name "mailing"] [@printer go "mailing"]
  (* @name: used by yojson as key *)
  (* @printer: used by show as key/string (removing @printer would change to
     Field as written -> Capital case) *)
  [@@deriving eq, show { with_path = false }, enum, yojson]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "I18n Keys: Could not create list of all keys!"
  ;;

  let schema () =
    Pool_common.(
      Utils.schema_decoder
        (fun m -> m |> read |> CCResult.pure)
        show
        Message.Field.RecruitmentChannel)
  ;;
end

module NumberOfInvitations = struct
  type t = int [@@deriving eq, show]

  let init = 0
  let value m = m
  let of_int m = m
  let increment m = m + 1
end

module NumberOfAssignments = struct
  type t = int [@@deriving eq, show]

  let init = 0
  let value m = m
  let of_int m = m
  let increment m = m + 1
end

type t =
  { user : Sihl_user.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : User.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  ; experiment_type_preference : Pool_common.ExperimentType.t option
  ; paused : User.Paused.t
  ; disabled : User.Disabled.t
  ; verified : User.Verified.t option
  ; email_verified : User.EmailVerified.t option
  ; num_invitations : NumberOfInvitations.t
  ; num_assignments : NumberOfAssignments.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; language_version : Pool_common.Version.t
  ; experiment_type_preference_version : Pool_common.Version.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

module Write = struct
  type t =
    { user_id : Pool_common.Id.t
    ; recruitment_channel : RecruitmentChannel.t
    ; terms_accepted_at : User.TermsAccepted.t option
    ; language : Pool_common.Language.t option
    ; experiment_type_preference : Pool_common.ExperimentType.t option
    ; paused : User.Paused.t
    ; disabled : User.Disabled.t
    ; verified : User.Verified.t option
    ; email_verified : User.EmailVerified.t option
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    ; firstname_version : Pool_common.Version.t
    ; lastname_version : Pool_common.Version.t
    ; paused_version : Pool_common.Version.t
    ; language_version : Pool_common.Version.t
    ; experiment_type_preference_version : Pool_common.Version.t
    }
  [@@deriving eq, show]

  let create m =
    { user_id = Pool_common.Id.of_string m.user.Sihl.Contract.User.id
    ; recruitment_channel = m.recruitment_channel
    ; terms_accepted_at = m.terms_accepted_at
    ; language = m.language
    ; experiment_type_preference = m.experiment_type_preference
    ; paused = m.paused
    ; disabled = m.disabled
    ; verified = m.verified
    ; email_verified = m.email_verified
    ; num_invitations = m.num_invitations
    ; num_assignments = m.num_assignments
    ; firstname_version = m.firstname_version
    ; lastname_version = m.lastname_version
    ; paused_version = m.paused_version
    ; language_version = m.paused_version
    ; experiment_type_preference_version = m.experiment_type_preference_version
    }
  ;;
end

let id m = m.user.Sihl_user.id |> Pool_common.Id.of_string
let fullname m = m.user |> User.user_fullname
let firstname m = m.user |> User.user_firstname
let lastname m = m.user |> User.user_lastname
let email_address m = m.user.Sihl_user.email |> User.EmailAddress.of_string

let version_selector p = function
  | "firstname" -> Some p.firstname_version
  | "lastname" -> Some p.lastname_version
  | "paused" -> Some p.paused_version
  | "language" -> Some p.language_version
  | _ -> None
;;

module Preview = struct
  type t =
    { user : Sihl_user.t
    ; language : Pool_common.Language.t option
    ; paused : User.Paused.t
    ; verified : User.Verified.t option
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    }
  [@@deriving eq, show]

  let fullname (m : t) = m.user |> User.user_fullname

  let email_address (m : t) =
    m.user.Sihl_user.email |> User.EmailAddress.of_string
  ;;
end

module PartialUpdate = struct
  module PoolField = Pool_common.Message.Field
  module Conformist = Pool_common.Utils.PoolConformist

  type t =
    | Firstname of Pool_common.Version.t * User.Firstname.t
    | Lastname of Pool_common.Version.t * User.Lastname.t
    | Paused of Pool_common.Version.t * User.Paused.t
    | Language of Pool_common.Version.t * Pool_common.Language.t option
    | Custom of Custom_field.Public.t
  [@@deriving eq, show, variants]

  let validate contact (field, version, value) =
    let check_version old_v t =
      let open Pool_common.Version in
      if old_v |> value > (version |> value)
      then Error Pool_common.Message.(MeantimeUpdate field)
      else Ok t
    in
    let validate schema =
      let schema =
        Pool_common.Utils.PoolConformist.(make Field.[ schema () ] CCFun.id)
      in
      Conformist.decode_and_validate
        schema
        [ field |> Pool_common.Message.Field.show, [ value ] ]
      |> CCResult.map_err Pool_common.Message.to_conformist_error
    in
    let open CCResult in
    match[@warning "-4"] field with
    | PoolField.Firstname ->
      User.Firstname.schema
      |> validate
      >|= (fun m -> Firstname (version, m))
      >>= check_version contact.firstname_version
    | PoolField.Lastname ->
      User.Lastname.schema
      |> validate
      >|= (fun m -> Lastname (version, m))
      >>= check_version contact.lastname_version
    | PoolField.Paused ->
      User.Paused.schema
      |> validate
      >|= (fun m -> Paused (version, m))
      >>= check_version contact.paused_version
    | PoolField.Language ->
      (fun () -> Conformist.optional @@ Pool_common.Language.schema ())
      |> validate
      >|= (fun m -> Language (version, m))
      >>= check_version contact.language_version
    | _ -> failwith "Todo"
  ;;

  let increment_version =
    let increment = Pool_common.Version.increment in
    function
    | Firstname (version, value) -> firstname (version |> increment) value
    | Lastname (version, value) -> lastname (version |> increment) value
    | Paused (version, value) -> paused (version |> increment) value
    | Language (version, value) -> language (version |> increment) value
    | Custom _ -> failwith "TODO"
  ;;
end

let validate_partial_update = PartialUpdate.validate
