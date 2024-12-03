open CCFun.Infix

module Policy = struct
  type rule =
    | MinLength of int
    | MustContainCapitalLetter
    | MustContainNumber
    | MustContainSpecialChar of char list

  type t = rule list

  let validate_min_length num p =
    if CCString.length p < num
    then Error (Pool_message.Error.PasswordPolicyMinLength num)
    else Ok p
  ;;

  let validate_characters validator error p =
    p
    |> CCString.to_list
    |> CCList.fold_left (fun is_ok c -> is_ok || validator c) false
    |> function
    | true -> Ok p
    | false -> Error error
  ;;

  let validate_capital_letter =
    let validate c = CCChar.to_int c >= 65 && CCChar.to_int c <= 90 in
    validate_characters validate Pool_message.Error.PasswordPolicyCapitalLetter
  ;;

  let validate_number =
    let validate c = CCChar.to_int c >= 48 && CCChar.to_int c <= 57 in
    validate_characters validate Pool_message.Error.PasswordPolicyNumber
  ;;

  let validate_special_char chars p =
    chars
    |> CCList.fold_left (fun is_ok c -> is_ok || CCString.contains p c) false
    |> function
    | true -> Ok p
    | false -> Error (Pool_message.Error.PasswordPolicySpecialChar chars)
  ;;

  let default_special_char_set =
    [ '!'; '?'; '*'; '+'; '-'; '_'; '&'; '%'; '('; ')'; '}'; '{'; '$'; ','; '.' ]
  ;;

  let default =
    [ MinLength 8
    ; MustContainCapitalLetter
    ; MustContainNumber
    ; MustContainSpecialChar default_special_char_set
    ]
  ;;

  let validate =
    CCResult.return
    %> CCList.fold_left (fun password rule ->
      CCResult.flat_map
        (match rule with
         | MinLength n -> validate_min_length n
         | MustContainCapitalLetter -> validate_capital_letter
         | MustContainNumber -> validate_number
         | MustContainSpecialChar chars -> validate_special_char chars)
        password)
  ;;
end

module Confirmation = struct
  include Pool_model.Base.String

  let create m = m
  let show _ = CCString.repeat "*" 10
  let pp formatter m = Format.fprintf formatter "%s" (show m)
  let schema ?(field = Pool_message.Field.PasswordConfirmation) () = schema field ()
end

module Plain = struct
  include Pool_model.Base.String

  let create = CCFun.id
  let show _ = CCString.repeat "*" 10
  let pp formatter m = Format.fprintf formatter "%s" (show m)
  let validate = Policy.(CCFun.flip validate default)

  let schema ?(field = Pool_message.Field.Password) ?(validation = validate) () =
    schema ~validation field ()
  ;;
end

let to_confirmed (m : Plain.t) : Confirmation.t = m

let validate_confirmation (password : Plain.t) (confirmation : Confirmation.t) =
  if CCString.equal (Plain.value password) (Confirmation.value confirmation)
  then Ok ()
  else Error Pool_message.Error.PasswordConfirmationDoesNotMatch
;;

module Hashing = struct
  let hash ?count plain =
    match count, not (Sihl.Configuration.is_production ()) with
    | _, true -> Ok (Bcrypt.hash ~count:4 plain |> Bcrypt.string_of_hash)
    | Some count, false ->
      if count < 4 || count > 31
      then Error Pool_message.Error.InvalidPasswordHashingCount
      else Ok (Bcrypt.hash ~count plain |> Bcrypt.string_of_hash)
    | None, false -> Ok (Bcrypt.hash ~count:10 plain |> Bcrypt.string_of_hash)
  ;;

  let matches ~hash ~plain = Bcrypt.verify plain (Bcrypt.hash_of_string hash)
end

include Pool_model.Base.String

let of_hash m = m
let show _ = CCString.repeat "*" 10
let pp formatter m = Format.fprintf formatter "%s" (show m)
let schema ?(field = Pool_message.Field.Password) () = schema field ()

let validate (hashed_password : t) (password : Plain.t) =
  Hashing.matches ~hash:(value hashed_password) ~plain:(Plain.value password)
;;

let validate_res ?(field = Pool_message.Field.CurrentPassword) user password =
  if validate user password then Ok () else Error Pool_message.Error.(Invalid field)
;;

let create password password_confirmation =
  let open CCResult.Infix in
  let* () = validate_confirmation password password_confirmation in
  Policy.validate password Policy.default >>= Hashing.hash
;;

let update
      (hashed_password : t)
      ~(old_password : Plain.t)
      ~(new_password : Plain.t)
      ~(new_password_confirmation : Confirmation.t)
  =
  let open CCResult.Infix in
  let* () = validate_res hashed_password old_password in
  let* () = validate_confirmation new_password new_password_confirmation in
  Hashing.hash new_password
;;
