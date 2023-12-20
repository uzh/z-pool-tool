let src = Logs.Src.create "user.cqrs"

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
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Role.Target.t -> Sihl_user.t -> Guard.ValidationSet.t
end = struct
  type t = Email.unverified Email.t

  let handle ?(tags = Logs.Tag.empty) ?allowed_email_suffixes contact email =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command UpdateEmail" ~tags);
    let* () =
      Pool_user.EmailAddress.validate
        allowed_email_suffixes
        (Email.address email)
    in
    match contact with
    | Contact contact ->
      Ok
        [ Contact.EmailUpdated (contact, Email.address email)
          |> Pool_event.contact
        ; Email.EmailVerified email |> Pool_event.email_verification
        ]
    | Admin _ ->
      Ok [ Email.EmailVerified email |> Pool_event.email_verification ]
  ;;

  let effects role user =
    let open Guard in
    let target_id = user.Sihl_user.id |> Guard.Uuid.Target.of_string_exn in
    ValidationSet.one_of_tuple (Permission.Update, role, Some target_id)
  ;;
end

module VerifyEmail : sig
  include Common.CommandSig with type t = Email.unverified Email.t

  val handle
    :  ?tags:Logs.Tag.set
    -> user
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Role.Target.t -> Sihl_user.t -> Guard.ValidationSet.t
end = struct
  type t = Email.unverified Email.t

  let handle ?(tags = Logs.Tag.empty) contact command =
    Logs.info ~src (fun m -> m "Handle command VerifyEmail" ~tags);
    match contact with
    | Contact contact ->
      Ok
        [ Contact.EmailVerified contact |> Pool_event.contact
        ; Email.EmailVerified command |> Pool_event.email_verification
        ]
    | Admin admin ->
      Ok
        [ Admin.EmailVerified admin |> Pool_event.admin
        ; Email.EmailVerified command |> Pool_event.email_verification
        ]
  ;;

  let effects role user =
    let open Guard in
    let target_id = user.Sihl_user.id |> Guard.Uuid.Target.of_string_exn in
    ValidationSet.one_of_tuple (Permission.Update, role, Some target_id)
  ;;
end
