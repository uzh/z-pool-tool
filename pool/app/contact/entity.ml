module Common = Pool_user

module Sihl_user = struct
  include Sihl_user

  let equal m k = CCString.equal m.id k.id

  let firstname m =
    m.given_name
    |> CCOption.get_exn_or (Format.asprintf "User '%s' has no firstname" m.id)
    |> Common.Firstname.of_string
  ;;

  let lastname m =
    m.name
    |> CCOption.get_exn_or (Format.asprintf "User '%s' has no lastname" m.id)
    |> Common.Lastname.of_string
  ;;

  let full_name m =
    Format.asprintf
      "%s %s"
      (m |> firstname |> Common.Firstname.value)
      (m |> lastname |> Common.Lastname.value)
  ;;
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
  ; terms_accepted_at : Common.TermsAccepted.t
  ; language : Pool_common.Language.t option
  ; paused : Common.Paused.t
  ; disabled : Common.Disabled.t
  ; verified : Common.Verified.t
  ; email_verified : Common.EmailVerified.t
  ; num_invitations : NumberOfInvitations.t
  ; num_assignments : NumberOfAssignments.t
  ; firstname_version : Pool_common.Version.t
  ; lastname_version : Pool_common.Version.t
  ; paused_version : Pool_common.Version.t
  ; language_version : Pool_common.Version.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

module Write = struct
  type t =
    { user_id : Pool_common.Id.t
    ; recruitment_channel : RecruitmentChannel.t
    ; terms_accepted_at : Common.TermsAccepted.t
    ; language : Pool_common.Language.t option
    ; paused : Common.Paused.t
    ; disabled : Common.Disabled.t
    ; verified : Common.Verified.t
    ; email_verified : Common.EmailVerified.t
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    ; firstname_version : Pool_common.Version.t
    ; lastname_version : Pool_common.Version.t
    ; paused_version : Pool_common.Version.t
    ; language_version : Pool_common.Version.t
    }
  [@@deriving eq, show]

  let create m =
    { user_id = Pool_common.Id.of_string m.user.Sihl.Contract.User.id
    ; recruitment_channel = m.recruitment_channel
    ; terms_accepted_at = m.terms_accepted_at
    ; language = m.language
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
    }
  ;;
end

let id m = m.user.Sihl_user.id |> Pool_common.Id.of_string
let fullname m = m.user |> Sihl_user.full_name
let firstname m = m.user |> Sihl_user.firstname
let lastname m = m.user |> Sihl_user.lastname
let email_address m = m.user.Sihl_user.email |> Common.EmailAddress.of_string

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
    ; paused : Common.Paused.t
    ; verified : Common.Verified.t
    ; num_invitations : NumberOfInvitations.t
    ; num_assignments : NumberOfAssignments.t
    }
  [@@deriving eq, show]

  let fullname (m : t) = m.user |> Sihl_user.full_name

  let email_address (m : t) =
    m.user.Sihl_user.email |> Common.EmailAddress.of_string
  ;;
end

module Field = struct
  module Conformist = Pool_common.Utils.PoolConformist


  type htmx_field =
    | Firstname of Common.Firstname.t
    | Lastname of Common.Lastname.t
    | Paused of Common.Paused.t
    | Language of Pool_common.Language.t option
    | Custom of string * string
  [@@deriving eq, show]

  type t = htmx_field * Pool_common.Version.t [@@deriving eq, show]

  let decode data =
    let decode_and_validate schema =
      let schema =
        Pool_common.Utils.PoolConformist.(make Field.[ schema () ] CCFun.id)
      in
      Conformist.decode_and_validate schema data
      |> CCResult.map_err Pool_common.Message.to_conformist_error
    in
    let open CCResult in
    let find name err =
      let open CCOption in
      CCList.assoc_opt ~eq:CCString.equal name data
      >>= CCList.head_opt
      |> to_result err
    in
    let open Pool_common.Message in
    let* field_str = find "field" InvalidHtmxRequest in
    let* value = find field_str InvalidHtmxRequest in
    let* version =
      find "version" (HtmxVersionNotFound field_str)
      >>= fun i ->
      i
      |> CCInt.of_string
      |> CCOption.map Pool_common.Version.of_int
      |> CCOption.to_result Pool_common.Message.(Invalid Field.Version)
    in
    let custom () = Custom (field_str, value) in
    let field =
      try Some (Pool_common.Message.Field.read field_str) with
      | _ -> None
    in
    (match field with
    | None -> Ok (custom ())
    | Some field ->
      let open Pool_common.Message in
      (match[@warning "-4"] field with
      | Field.Firstname ->
        Common.Firstname.schema |> decode_and_validate >|= fun m -> Firstname m
      | Field.Lastname ->
        Common.Lastname.schema |> decode_and_validate >|= fun m -> Lastname m
      | Field.Paused ->
        Common.Paused.schema |> decode_and_validate >|= fun m -> Paused m
      | Field.Language ->
        (fun () -> Conformist.optional @@ Pool_common.Language.schema ())
        |> decode_and_validate
        >|= fun m -> Language m
      | _ -> custom () |> return))
    >|= fun field -> field, version
  ;;
end
