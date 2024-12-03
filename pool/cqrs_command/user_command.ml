open CCFun.Infix

let src = Logs.Src.create "user.cqrs"

type create_user =
  { email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.Plain.t [@opaque]
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  }

let create_user_command email password firstname lastname =
  { email; password; firstname; lastname }
;;

let create_user_schema =
  Pool_conformist.(
    make
      Field.
        [ Pool_user.EmailAddress.schema ()
        ; Pool_user.Password.Plain.schema ()
        ; Pool_user.Firstname.schema ()
        ; Pool_user.Lastname.schema ()
        ]
      create_user_command)
;;

type user =
  | Contact of Contact.t
  | Admin of Admin.t

module UpdateEmail : sig
  include Common.CommandSig

  type t = Email.unverified Email.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> user
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Role.Target.t -> Pool_user.t -> Guard.ValidationSet.t
end = struct
  type t = Email.unverified Email.t

  let handle ?(tags = Logs.Tag.empty) ?allowed_email_suffixes contact email =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command UpdateEmail" ~tags);
    let* () =
      Pool_user.EmailAddress.validate allowed_email_suffixes (Email.address email)
    in
    match contact with
    | Contact contact ->
      Ok
        [ Contact.EmailUpdated (contact, Email.address email) |> Pool_event.contact
        ; Email.EmailVerified email |> Pool_event.email_verification
        ]
    | Admin _ -> Ok [ Email.EmailVerified email |> Pool_event.email_verification ]
  ;;

  let effects role user =
    let open Guard in
    let target_id = user.Pool_user.id |> Guard.Uuid.target_of Pool_user.Id.value in
    ValidationSet.one_of_tuple (Permission.Update, role, Some target_id)
  ;;
end

module VerifyEmail : sig
  include Common.CommandSig with type t = Email.unverified Email.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?signup_code:Signup_code.Code.t
    -> user
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Role.Target.t -> Pool_user.t -> Guard.ValidationSet.t
end = struct
  type t = Email.unverified Email.t

  let handle ?(tags = Logs.Tag.empty) ?signup_code contact command =
    Logs.info ~src (fun m -> m "Handle command VerifyEmail" ~tags);
    let signup_code_event =
      signup_code
      |> CCOption.map_or ~default:[] (fun code ->
        Signup_code.Verified code |> Pool_event.signupcode |> CCList.return)
    in
    match contact with
    | Contact contact ->
      Ok
        ([ Contact.EmailVerified contact |> Pool_event.contact
         ; Email.EmailVerified command |> Pool_event.email_verification
         ]
         @ signup_code_event)
    | Admin admin ->
      Ok
        [ Admin.EmailVerified admin |> Pool_event.admin
        ; Email.EmailVerified command |> Pool_event.email_verification
        ]
  ;;

  let effects role user =
    let open Guard in
    let target_id = user.Pool_user.id |> Guard.Uuid.target_of Pool_user.Id.value in
    ValidationSet.one_of_tuple (Permission.Update, role, Some target_id)
  ;;
end

module UpdatePassword : sig
  type t =
    { current_password : Pool_user.Password.Plain.t
    ; new_password : Pool_user.Password.Plain.t
    ; password_confirmation : Pool_user.Password.Confirmation.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?notification:Email.dispatch
    -> Pool_user.Id.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
end = struct
  type t =
    { current_password : Pool_user.Password.Plain.t [@opaque]
    ; new_password : Pool_user.Password.Plain.t [@opaque]
    ; password_confirmation : Pool_user.Password.Confirmation.t [@opaque]
    }

  let command current_password new_password password_confirmation =
    { current_password; new_password; password_confirmation }
  ;;

  let schema =
    let open Pool_message.Field in
    Pool_conformist.(
      make
        Field.
          [ Pool_user.Password.Plain.(
              schema ~field:CurrentPassword ~validation:CCResult.return ())
          ; Pool_user.Password.Plain.(schema ~field:NewPassword ())
          ; Pool_user.Password.Confirmation.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) ?notification user_id command =
    Logs.info ~src (fun m -> m "Handle command UpdatePassword" ~tags);
    (* NOTE use 'Pool_user.validate_current_password' in handler before this
       command. *)
    let open CCResult in
    let* () =
      Pool_user.Password.validate_confirmation
        command.new_password
        command.password_confirmation
    in
    Ok
      ((Pool_user.PasswordUpdated
          ( user_id
          , command.current_password
          , command.new_password
          , command.password_confirmation )
        |> Pool_event.user)
       :: CCOption.map_or
            ~default:[]
            (Email.sent %> Pool_event.email %> CCList.return)
            notification)
  ;;

  let decode data =
    Pool_conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;
end
