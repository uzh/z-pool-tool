module Email = Entity_email

module Password = struct
  type t = string [@@deriving eq]

  let default_password_policy p =
    if String.length p < 8 then Error "password_policy_text" else Ok ()
  ;;

  let validate ?(password_policy = default_password_policy) password =
    let open CCResult in
    let* () = password_policy password in
    Ok ()
  ;;

  let create password = Ok password
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "password")
      CCList.pure
      "password"
  ;;
end

module PasswordConfirmed = struct
  type t = string [@@deriving eq]

  let create m = m
  let to_sihl m = m
  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;
end

module Firstname = struct
  type t = string [@@deriving eq, show]

  let create m =
    if String.length m <= 0 then Error "Invalid firstname" else Ok m
  ;;

  let value m = m

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "firstname")
      CCList.pure
      "firstname"
  ;;
end

module Lastname = struct
  type t = string [@@deriving eq, show]

  let create m = if String.length m <= 0 then Error "Invalid lastname" else Ok m
  let value m = m

  let schema () =
    Conformist.custom
      (Utils.schema_decoder create "lastname")
      CCList.pure
      "lastname"
  ;;
end

module Paused = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
end

module TermsAccepted = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let create_now = Ptime_clock.now ()
  let value m = m
end

module Verified = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = m
  let create_now = Ptime_clock.now ()
  let value m = m
end
