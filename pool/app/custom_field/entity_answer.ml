module Id = struct
  include Pool_common.Id
end

type 'a t =
  { id : Id.t
  ; value : 'a
  ; version : Pool_common.Version.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) ?(version = Pool_common.Version.create ()) value
  =
  { id; version; value }
;;

let id { id; _ } = id
let version { version; _ } = version

let increment_version m =
  { m with version = Pool_common.Version.increment (version m) }
;;
