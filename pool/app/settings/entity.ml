module Day = struct
  type t = int
end

module Week = struct
  type t = int
end

module Language = struct
  type t =
    | En [@name "EN"]
    | De [@name "DE"]
  [@@deriving eq, show, yojson]

  let code = function
    | En -> "EN"
    | De -> "DE"
  ;;

  let of_string = function
    | "EN" -> Ok En
    | "DE" -> Ok De
    | _ -> Error "Invalid Language privided"
  ;;

  let t =
    Caqti_type.(
      custom ~encode:(fun m -> m |> code |> Result.ok) ~decode:of_string string)
  ;;

  let label country_code = country_code |> code |> Utils.Countries.find

  let schema () =
    Conformist.custom
      (Utils.schema_decoder of_string "default language")
      (fun l -> [ code l ])
      "default_language"
  ;;

  let all () = [ En; De ]
  let all_codes () = [ En; De ] |> CCList.map code
end

module ContactEmail = struct
  (* TODO [timhub] : Use Common User Email -> Dependency cycle *)
  type t = string [@@deriving eq, show, yojson]

  let value m = m

  let create email =
    if CCString.length email <= 0
    then Error "invalid email address!"
    else Ok email
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "contact email")
      (fun l -> [ value l ])
      "contact_email"
  ;;
end

module EmailSuffix = struct
  type t = string [@@deriving eq, show, yojson]

  let value m = m

  let create suffix =
    if CCString.length suffix <= 0
    then Error "Invalid email suffix!"
    else Ok suffix
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "email_suffix")
      CCList.pure
      "email_suffix"
  ;;
end

module InactiveUser = struct
  module DisableAfter = struct
    type t = Ptime.Span.t [@@deriving show]
  end

  module Warning = struct
    type t = Ptime.Span.t [@@deriving show]
  end
end

module TermsAndConditions = struct
  type t = string

  let create m = m
  let value m = m
end

module Value = struct
  type tenant_languages = Language.t list [@@deriving eq, show, yojson]
  type tenant_email_suffixes = EmailSuffix.t list [@@deriving eq, show, yojson]
  type tenant_contact_email = ContactEmail.t [@@deriving eq, show, yojson]

  type t =
    | TenantLanguages of tenant_languages
    | TenantEmailSuffixes of tenant_email_suffixes
    | TenantContactEmail of tenant_contact_email
  [@@deriving eq, show, yojson]
end

type setting_key =
  | Languages [@name "languages"]
  | EmailSuffixes [@name "email_suffixes"]
  | ContactEmail [@name "contact_email"]
[@@deriving eq, show, yojson]

type t =
  { value : Value.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

let updated_at s = s.updated_at

let[@warning "-4"] languages setting =
  let open Value in
  match setting.value with
  | TenantLanguages value -> value
  | _ -> failwith "Invalid setting provided!"
;;

let[@warning "-4"] email_suffixes setting =
  let open Value in
  match setting.value with
  | TenantEmailSuffixes value -> value
  | _ -> failwith "Invalid setting provided!"
;;

let[@warning "-4"] contact_email setting =
  let open Value in
  match setting.value with
  | TenantContactEmail value -> value
  | _ -> failwith "Invalid setting provided!"
;;

module Write = struct
  type t = { value : Value.t }
end
