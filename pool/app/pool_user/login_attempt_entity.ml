module Id = Pool_common.Id

module Counter = struct
  type t = int [@@deriving eq, show]

  let value m = m
  let create = CCInt.max 0
  let init = 0
  let increment m = m + 1
end

module BlockedUntil = struct
  type t = Ptime.t [@@deriving eq, show]

  let value m = m

  let create m =
    if Ptime.is_earlier ~than:(Ptime_clock.now ()) m
    then Error Pool_common.Message.TimeInPast
    else Ok m
  ;;
end

type t =
  { id : Id.t
  ; email : Entity.EmailAddress.t
  ; counter : Counter.t
  ; blocked_until : BlockedUntil.t option
  }

let create ?(id = Id.create ()) email counter blocked_until =
  { id; email; counter; blocked_until }
;;
