module Email = Entity_email

module Password = struct
  type t = string [@@deriving eq]

  let default_password_policy p =
    if CCString.length p < 8 then Error "password_policy_text" else Ok ()
  ;;

  let validate ?(password_policy = default_password_policy) password =
    let open CCResult in
    let* () =
      password_policy password |> map_err Pool_common.Message.passwordpolicy
    in
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
      Pool_common.(Utils.schema_decoder create Message.Password)
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
    m |> show |> Format.fprintf formatter "%s"
  ;;
end

module Firstname = struct
  type t = string [@@deriving eq, show]

  let create m =
    if CCString.is_empty m
    then Error Pool_common.Message.(Invalid Firstname)
    else Ok m
  ;;

  let of_string m = m
  let value m = m

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.Firstname)
      CCList.pure
      "firstname"
  ;;
end

module Lastname = struct
  type t = string [@@deriving eq, show]

  let create m =
    if CCString.is_empty m
    then Error Pool_common.Message.(Invalid Lastname)
    else Ok m
  ;;

  let of_string m = m
  let value m = m

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.Lastname)
      CCList.pure
      "lastname"
  ;;
end

module Paused = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m

  let schema () =
    Conformist.custom
      (Pool_common.Utils.schema_decoder
         (fun m -> m |> Utils.Bool.of_string |> CCResult.pure)
         Pool_common.Message.Paused)
      (fun l -> l |> Utils.Bool.stringify |> CCList.pure)
      "paused"
  ;;
end

module Disabled = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
end

module TermsAccepted = struct
  type t = Ptime.t option [@@deriving eq, show]

  let create m = m
  let create_now () = Some (Ptime_clock.now ())
  let value m = m
end

module Verified = struct
  type t = Ptime.t option [@@deriving eq, show]

  let create m = m
  let create_now () = Some (Ptime_clock.now ())
  let value m = m
end
