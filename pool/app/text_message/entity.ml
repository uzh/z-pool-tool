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

type job =
  { message : t
  ; message_history : Queue.History.create option [@yojson.option]
  ; resent : Pool_common.Id.t option
  }
[@@deriving eq, show, yojson]

let parse_job_json str =
  try Ok (str |> Yojson.Safe.from_string |> job_of_yojson) with
  | _ -> Error Pool_common.Message.(Invalid Field.Input)
;;

let job_message_history { message_history; _ } = message_history

let create_job ?message_history message =
  { message; message_history; resent = None }
;;

let create recipient sender text = { recipient; sender; text }

let render_and_create recipient sender (text, params) =
  Content.render text params |> create recipient sender
;;
