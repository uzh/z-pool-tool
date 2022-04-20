module EditLocation : sig
  type t =
    { location_id : Pool_common.Id.t
    ; room : Pool_location.Room.t
    ; building : Pool_location.Building.t
    ; street : Pool_location.Street.t
    ; zip : Pool_location.Zip.t
    ; city : Pool_location.City.t
    }

  val handle : t -> Pool_location.t -> (Pool_event.t list, string) result
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { location_id : Pool_common.Id.t
    ; room : Pool_location.Room.t
    ; building : Pool_location.Building.t
    ; street : Pool_location.Street.t
    ; zip : Pool_location.Zip.t
    ; city : Pool_location.City.t
    }

  let handle = Utils.todo

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Location, Some command.location_id) ]
  ;;
end
