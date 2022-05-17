module EditLocation : sig
  type t =
    { location_id : Pool_common.Id.t
    ; room : Pool_location.MailingAddress.Room.t
    ; building : Pool_location.MailingAddress.Building.t
    ; street : Pool_location.MailingAddress.Street.t
    ; zip : Pool_location.MailingAddress.Zip.t
    ; city : Pool_location.MailingAddress.City.t
    }

  val handle
    :  t
    -> Pool_location.MailingAddress.t
    -> (Pool_event.t list, string) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { location_id : Pool_common.Id.t
    ; room : Pool_location.MailingAddress.Room.t
    ; building : Pool_location.MailingAddress.Building.t
    ; street : Pool_location.MailingAddress.Street.t
    ; zip : Pool_location.MailingAddress.Zip.t
    ; city : Pool_location.MailingAddress.City.t
    }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Location, Some command.location_id) ]
  ;;
end
