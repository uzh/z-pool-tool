module Id = struct
  include Pool_common.Id
end

module ApiKey = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.GtxApiKey
  let schema () = schema field ()
end

module Sender = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.GtxSender

  (* The GTX Rest API does not allow more than 11 characters *)
  let validation str =
    if CCString.length str > 11 then Error (Pool_message.Error.MaxLength 11) else Ok str
  ;;

  let schema () = schema ~validation field ()
end

(* Improve naming *)
type t =
  { id : Id.t
  ; api_key : ApiKey.t
  ; sender : Sender.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let create ?(id = Id.create ()) api_key sender =
  let open Pool_common in
  { id
  ; api_key
  ; sender
  ; created_at = CreatedAt.create_now ()
  ; updated_at = UpdatedAt.create_now ()
  }
;;
