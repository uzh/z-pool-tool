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
let set_version version m = { m with version }

let answer_meta answer =
  answer
  |> CCOption.map_or ~default:(None, None) (fun { id; version; _ } ->
       Some id, Some version)
;;
