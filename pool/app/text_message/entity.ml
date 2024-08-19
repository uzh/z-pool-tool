open Ppx_yojson_conv_lib.Yojson_conv

module Content = struct
  type t = string [@@deriving eq, show, yojson]

  let render text params =
    Sihl.Contract.Email_template.render params text None |> fst
  ;;

  let of_string m = m
  let value m = m
end

type t =
  { recipient : Pool_user.CellPhone.t
  ; sender : Pool_tenant.GtxSender.t
  ; text : Content.t
  }
[@@deriving eq, show, yojson] [@@yojson.allow_extra_fields]

let update ?new_recipient message =
  { message with
    recipient = CCOption.get_or ~default:message.recipient new_recipient
  }
;;

let create recipient sender text = { recipient; sender; text }

let render_and_create recipient sender (text, params) =
  Content.render text params |> create recipient sender
;;

module DlrMask = struct
  let field = Pool_message.Field.TextMessageDlrStatus

  type t =
    | Delivered [@name "delivered"] [@printer Utils.ppx_printer "delivered"]
    | NonDelivered [@name "non-delivered"]
    [@printer Utils.ppx_printer "non-delivered"]
    | Expired [@name "expired"] [@printer Utils.ppx_printer "expired"]
    | Unknown [@name "unknown"] [@printer Utils.ppx_printer "unknown"]
  [@@deriving eq, show { with_path = false }, yojson]

  let of_int = function
    | 1 -> Delivered
    | 2 -> NonDelivered
    | 34 -> Expired
    | _ -> Unknown
  ;;

  let to_human m =
    m
    |> show
    |> CCString.replace ~which:`All ~sub:"-" ~by:""
    |> CCString.capitalize_ascii
  ;;
end

type delivery_report =
  { job_id : Pool_queue.Id.t
  ; raw : string
  ; from : string
  ; to_ : string
  ; message_id : string
  ; dlr_mask : int
  ; error_code : int
  ; error_message : string
  ; submit_date : Pool_model.Base.Ptime.t
  ; done_date : Pool_model.Base.Ptime.t
  ; plmn : string
  ; country : string
  ; sms_cost : float
  }
[@@deriving eq, show, yojson]
