module PoolError = Pool_common.Message
module Common = Common_user

type email_unverified =
  { address : Common.Email.Address.t
  ; token : Common.Email.Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

type email_verified =
  { address : Common.Email.Address.t
  ; verified_at : Common.Email.VerifiedAt.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

(* TODO hide private constructors if possible *)
(* Don't use these private constructors *)
(* They are needed so the typechecker understands they are disjoint *)
type unverified = private XUnverifiedP
type verified = private XVerifiedP

type _ t =
  | Unverified : email_unverified -> unverified t
  | Verified : email_verified -> verified t

(* Carries type information, is a type "witness" *)
type _ carrier =
  | UnverifiedC : unverified carrier
  | VerifiedC : verified carrier

let equal : type state. state t -> state t -> bool =
 fun m k ->
  match m, k with
  | Unverified one, Unverified two -> equal_email_unverified one two
  | Verified one, Verified two -> equal_email_verified one two
;;

let pp : type state. Format.formatter -> state t -> unit =
 fun formatter email ->
  match email with
  | Unverified m -> pp_email_unverified formatter m
  | Verified m -> pp_email_verified formatter m
;;

let show : type state. state t -> string = function
  | Unverified { address; _ } | Verified { address; _ } ->
    Common.Email.Address.show address
;;

let address : type state. state t -> Common.Email.Address.t = function
  | Unverified { address; _ } | Verified { address; _ } -> address
;;

let token (Unverified email) = Common.Email.Token.value email.token

let create address token =
  Unverified
    { address
    ; token
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
;;

let verify (Unverified email) =
  Verified
    { address = email.address
    ; verified_at = Common.Email.VerifiedAt.create_now ()
    ; created_at = email.created_at
    ; updated_at = Ptime_clock.now ()
    }
;;

let create_public_url pool_url path =
  path
  |> Sihl.Web.externalize_path
  |> Format.asprintf "%s%s" (Pool_common.Url.value pool_url)
;;

let prepend_root_directory pool url =
  let open Pool_common.Database in
  match Label.equal pool root with
  | true -> Format.asprintf "/root%s" url
  | false -> url
;;

let prepare_email pool template_label subject email params =
  let%lwt template =
    Service.EmailTemplate.get_by_label
      ~ctx:(Pool_common.Utils.pool_to_ctx pool)
      template_label
  in
  match template, Sihl.Configuration.read_string "SMTP_SENDER" with
  | _, None -> failwith "SMTP_SENDER not found in configuration"
  | None, _ -> failwith "Email template not found!"
  | Some template, Some sender ->
    let mail =
      Sihl_email.
        { sender
        ; recipient = email
        ; subject
        ; text = ""
        ; html = None
        ; cc = []
        ; bcc = []
        }
    in
    Sihl_email.Template.email_of_template ~template mail params
;;

module PasswordReset = struct
  let create pool ~user =
    let email = user.Sihl_user.email in
    let%lwt url = Pool_common.Repo.Url.of_pool pool in
    let%lwt reset_token =
      Service.PasswordReset.create_reset_token
        ~ctx:(Pool_common.Utils.pool_to_ctx pool)
        email
    in
    match reset_token with
    | None -> Lwt.return_error Pool_common.Message.PasswordResetFailMessage
    | Some token ->
      let subject = "Password reset" in
      let reset_url =
        Format.asprintf "/reset-password/?token=%s" token
        |> prepend_root_directory pool
        |> Sihl.Web.externalize_path
        |> create_public_url url
      in
      let given_name =
        user.Sihl_user.given_name |> CCOption.value ~default:""
      in
      let name = user.Sihl_user.name |> CCOption.value ~default:"" in
      prepare_email
        pool
        "password_reset"
        subject
        email
        [ "resetUrl", reset_url
        ; "name", Format.asprintf "%s %s" given_name name
        ]
      |> Lwt_result.ok
  ;;
end

module PasswordChange = struct
  let create db_pool email firstname lastname =
    let name =
      Format.asprintf
        "%s %s"
        (Common.Firstname.value firstname)
        (Common.Lastname.value lastname)
    in
    let subject = "Password has been changed" in
    prepare_email
      db_pool
      "password_change"
      subject
      (address email |> Common_user.Email.Address.value)
      [ "name", name ]
  ;;
end

module SignUp = struct
  let create db_pool email firstname lastname =
    let%lwt url = Pool_common.Repo.Url.of_pool db_pool in
    let name =
      Format.asprintf
        "%s %s"
        (Common.Firstname.value firstname)
        (Common.Lastname.value lastname)
    in
    let subject = "Email verification" in
    let validation_url =
      Format.asprintf "/email-verified?token=%s" (token email)
      |> create_public_url url
    in
    prepare_email
      db_pool
      "signup_verification"
      subject
      (address email |> Common_user.Email.Address.value)
      [ "verificationUrl", validation_url; "name", name ]
  ;;
end

module ConfirmationEmail = struct
  let create pool email firstname lastname =
    let%lwt url = Pool_common.Repo.Url.of_pool pool in
    let name =
      Format.asprintf
        "%s %s"
        (Common.Firstname.value firstname)
        (Common.Lastname.value lastname)
    in
    let subject = "Email verification" in
    let validation_url =
      Format.asprintf "/email-verified?token=%s" (token email)
      |> Sihl.Web.externalize_path
      |> create_public_url url
    in
    prepare_email
      pool
      "email_verification"
      subject
      (address email |> Common_user.Email.Address.value)
      [ "verificationUrl", validation_url; "name", name ]
  ;;
end
