module Common = Common_user

module Sign_up : sig
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Pool_event.t list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t
end = struct
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  let command email password firstname lastname recruitment_channel =
    { email; password; firstname; lastname; recruitment_channel }
  ;;

  let schema =
    Conformist.(
      make
        [ string "email"
        ; string "password"
        ; string "firstname"
        ; string "lastname"
        ; custom
            (fun l -> l |> List.hd |> Participant.RecruitmentChannel.of_string)
            (fun l -> [ Participant.RecruitmentChannel.to_string l ])
            "recruitment_channel"
            ~meta:()
        ]
        command)
  ;;

  let handle ?allowed_email_suffixes ?password_policy command =
    let ( let* ) = Result.bind in
    let* password =
      Common.Password.create ?password_policy command.password ()
    in
    let* email =
      Participant.Email.Address.validate allowed_email_suffixes command.email
    in
    let* firstname = Common.Firstname.create command.firstname in
    let* lastname = Common.Lastname.create command.lastname in
    let terms_accepted_at = Common.TermsAccepted.create_now in
    let participant =
      Participant.
        { email
        ; password
        ; firstname
        ; lastname
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at
        }
    in
    Ok
      [ Participant.Created participant |> Pool_event.participant
      ; Common.Event.Email.Created email |> Pool_event.email_address
      ]
  ;;

  let decode data = Conformist.decode_and_validate schema data
end

module UpdateDetails : sig
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; paused : string
    }

  val handle
    :  Participant.t
    -> email:string
    -> password:string
    -> (Pool_event.t list, string) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; paused : string
    }

  let handle _ ~email:_ ~password:_ = Utils.todo ()

  let can user command =
    let open Lwt.Syntax in
    let* participant = Participant.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Participant, Some command.id)
        ; Permission.Update (Permission.Tenant, Some tenant.id)
        ]
  ;;
end

module UpdatePassword : sig
  type t =
    { id : string
    ; current_password : string
    ; new_password : string
    }

  val handle : t -> Participant.t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; current_password : string
    ; new_password : string
    }

  let handle _ = Utils.todo

  let can user command =
    let open Lwt.Syntax in
    let* participant = Participant.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      participant.Participant.user
      ~any_of:
        [ Permission.Update (Permission.Participant, Some command.id)
        ; Permission.Update (Permission.Tenant, Some tenant.id)
        ]
  ;;
end

module UpdateEmail : sig
  type t =
    { id : string
    ; email : string
    }

  val handle : t -> Participant.t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; email : string
    }

  let handle _ = Utils.todo

  let can user command =
    let open Lwt.Syntax in
    let* participant = Participant.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      participant.Participant.user
      ~any_of:
        [ Permission.Update (Permission.Participant, Some command.id)
        ; Permission.Update (Permission.Tenant, Some tenant.id)
        ]
  ;;
end
