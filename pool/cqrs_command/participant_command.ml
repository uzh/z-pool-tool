module User = Common_user
module Id = Common.Id

module Sign_up : sig
  type t =
    { email : User.Email.Address.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
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
    { email : User.Email.Address.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  let command email password firstname lastname recruitment_channel =
    { email; password; firstname; lastname; recruitment_channel }
  ;;

  let schema =
    Conformist.(
      make
        [ custom
            (fun l -> l |> List.hd |> User.Email.Address.create)
            (fun l -> [ User.Email.Address.show l ])
            "email"
        ; custom
            (fun l -> l |> List.hd |> User.Password.create)
            (fun l -> [ User.Password.to_sihl l ])
            "password"
        ; custom
            (fun l -> l |> List.hd |> User.Firstname.create)
            (fun l -> [ User.Firstname.show l ])
            "firstname"
        ; custom
            (fun l -> l |> List.hd |> User.Lastname.create)
            (fun l -> [ User.Lastname.show l ])
            "lastname"
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
    let* () = User.Password.validate ?password_policy command.password in
    let* () =
      Participant.Email.Address.validate allowed_email_suffixes command.email
    in
    let participant =
      Participant.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at = User.TermsAccepted.create_now
        }
    in
    Ok
      [ Participant.Created participant |> Pool_event.participant
      ; User.Event.Email.Created command.email |> Pool_event.email_address
      ]
  ;;

  let decode data = Conformist.decode_and_validate schema data
end

module UpdateDetails : sig
  type t =
    { id : Id.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; paused : User.Paused.t
    }

  val handle
    :  Participant.t
    -> email:User.Email.Address.t
    -> password:User.Password.t
    -> (Pool_event.t list, string) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : Id.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; paused : User.Paused.t
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
    { id : Id.t
    ; current_password : User.Password.t
    ; new_password : User.Password.t
    }

  val handle : t -> Participant.t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : Id.t
    ; current_password : User.Password.t
    ; new_password : User.Password.t
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
    { id : Id.t
    ; email : User.Email.Address.t
    }

  val handle : t -> Participant.t -> (Pool_event.t list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : Id.t
    ; email : User.Email.Address.t
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
