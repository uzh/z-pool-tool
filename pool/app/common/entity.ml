module Email = Entity_email

module Id = struct
  type t = string

  let create () = Uuidm.create `V4 |> Uuidm.to_string
end

module Password = struct
  type t = string [@@deriving eq]

  let default_password_policy p =
    if String.length p < 8 then Error "password_policy_text" else Ok ()
  ;;

  let create ?(password_policy = default_password_policy) password () =
    let ( let* ) = Result.bind in
    let* () = password_policy password in
    Ok password
  ;;

  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;
end

module PasswordConfirmed = struct
  type t = string [@@deriving eq]

  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;
end

module Firstname = struct
  type t = string [@@deriving eq, show]

  let create m = Ok m
end

module Lastname = struct
  type t = string [@@deriving eq, show]

  let create m = Ok m
end

module Paused = struct
  type t = bool [@@deriving eq, show]

  let create m = Ok m
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let create m = Ok m
end

module TermsAccepted = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = Ok m
  let create_now = Ok (Ptime_clock.now ())
end

module Verified = struct
  type t = Ptime.t [@@deriving eq, show]

  let create m = Ok m
end
