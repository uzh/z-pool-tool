module Id = struct
  include Pool_common.Id

  let schema () = schema ~field:Pool_message.Field.Id ()
end

module Channel = struct
  module Core = struct
    let field = Pool_message.Field.Channel

    type t = Email [@name "email"] [@printer Utils.ppx_printer "email"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core
end

module Token = struct
  include Pool_model.Base.String

  let length = 8
  let charset = "0123456789"

  let generate () =
    let charset_length = CCString.length charset in
    let random_string = Bytes.create length in
    for i = 0 to length - 1 do
      let index = Random.int charset_length in
      Bytes.set random_string i (String.get charset index)
    done;
    Bytes.to_string random_string
  ;;

  let value m = m
  let schema () = schema Pool_message.Field.Token ()
end

type t =
  { id : Id.t
  ; user_uuid : Pool_user.Id.t
  ; channel : Channel.t
  ; token : Token.t
  }
[@@deriving eq, show { with_path = false }]
