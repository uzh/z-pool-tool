open Ppx_yojson_conv_lib.Yojson_conv

type t =
  { entity_uuid : Pool_common.Id.t
  ; job : Sihl_queue.instance
  ; message_template : string option
  (* Should I pass a UUID? entity_specific templates can be deleted *)
  }
[@@deriving show]

type create =
  { entity_uuids : Pool_common.Id.t list
  ; message_template : string option
  }
[@@deriving show, eq, yojson]

let create ?message_template ~entity_uuid job =
  { entity_uuid; job; message_template }
;;
