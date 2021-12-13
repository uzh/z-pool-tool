module PoolError = Pool_common.Message

module Token = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m
end

module Address = struct
  type t = string [@@deriving eq, show]

  let remove_whitespaces =
    let open Re in
    replace_string (space |> compile) ~by:""
  ;;

  let validate_characters email =
    let open Re in
    (* Checks for more than 1 character before and more than 2 characters after
       the @ sign *)
    let regex =
      seq [ repn any 1 None; char '@'; repn any 2 None ]
      |> whole_string
      |> compile
    in
    if Re.execp regex email
    then Ok email
    else Error PoolError.(Invalid EmailAddress)
  ;;

  let strip_email_suffix email =
    (* TODO check whether this is stable *)
    let tail = CCString.split_on_char '@' email |> CCList.tail_opt in
    CCOption.bind tail CCList.head_opt
  ;;

  let validate_suffix
      (allowed_email_suffixes : Settings.EmailSuffix.t list option)
      email
    =
    match allowed_email_suffixes with
    | None -> Ok ()
    | Some allowed_email_suffixes ->
      (match strip_email_suffix email with
      (* TODO check whether this is really the case *)
      | None -> Error PoolError.EmailMalformed
      | Some suffix ->
        let open CCResult in
        let* suffix = suffix |> Settings.EmailSuffix.create in
        if CCList.mem
             ~eq:Settings.EmailSuffix.equal
             suffix
             allowed_email_suffixes
        then Ok ()
        else Error PoolError.(Invalid EmailSuffix))
  ;;

  let validate = validate_suffix
  let value m = m
  let create email = email |> remove_whitespaces |> validate_characters
  let of_string m = m

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create PoolError.EmailAddress)
      CCList.pure
      "email"
  ;;
end

module VerifiedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let value m = m
  let create m = m
  let create_now = Ptime_clock.now
end
