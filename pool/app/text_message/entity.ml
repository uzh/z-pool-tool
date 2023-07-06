open Ppx_yojson_conv_lib.Yojson_conv

module Content = struct
  type t = string [@@deriving eq, show, yojson]

  let render text params =
    Sihl.Contract.Email_template.render params text None |> fst
  ;;

  let value m = m
end

type t =
  { recipient : Pool_user.CellPhone.t
  ; sender : Pool_tenant.Title.t
  ; text : Content.t
  }
[@@deriving eq, show, yojson]

let create recipient sender text = { recipient; sender; text }

let render_and_create recipient sender (text, params) =
  Content.render text params |> create recipient sender
;;
