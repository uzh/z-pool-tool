let url_key = Pool_message.Field.SignUpCode
let payload_url_key = Pool_message.Field.Payload

module Field = Pool_message.Field

module Id = struct
  include Pool_model.Base.Id
end

module Code = struct
  include Pool_model.Base.String

  let field = Field.Code
  let max_length = 128

  let create code =
    if CCString.length code > max_length
    then Error (Pool_message.Error.MaxLength max_length)
    else create code
  ;;

  let schema = schema field ~validation:create
end

module Count = struct
  type t = int [@@deriving eq, show]

  let value m = m
end

module WebhookUrl = struct
  include Pool_model.Base.String

  let pp formatter _ = Format.pp_print_string formatter "<opaque>"
  let show _ = "<opaque>"
  let max_length = 2048

  let create url =
    let open Pool_message in
    let url = CCString.trim url in
    let uri = Uri.of_string url in
    match Uri.scheme uri, Uri.host uri with
    | _ when CCString.length url > max_length -> Error (Error.MaxLength max_length)
    | Some ("http" | "https"), Some host when not (CCString.is_empty host) -> Ok url
    | _ -> Error (Error.Invalid Field.Url)
  ;;

  let schema field = schema field ~validation:create
end

module Payload = struct
  include Pool_model.Base.String

  let field = Field.Payload
  let schema () = schema field ()
end

type t =
  { id : Id.t
  ; code : Code.t
  ; signup_count : Count.t
  ; verification_count : Count.t
  ; signed_up_webhook_url : WebhookUrl.t option
  ; email_verified_webhook_url : WebhookUrl.t option
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) ?signed_up_webhook_url ?email_verified_webhook_url code =
  { id
  ; code
  ; signup_count = 0
  ; verification_count = 0
  ; signed_up_webhook_url
  ; email_verified_webhook_url
  }
;;

module Webhook = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  module Trigger = struct
    type t =
      | SignedUp [@name "signed_up"] [@printer Utils.ppx_printer "signed_up"]
      | EmailVerified [@name "email_verified"]
      [@printer Utils.ppx_printer "email_verified"]
    [@@deriving eq, show { with_path = false }, yojson]
  end

  type t =
    { url : WebhookUrl.t
    ; trigger : Trigger.t
    ; code : Code.t
    ; payload : Payload.t option
    }
  [@@deriving eq, show, yojson]

  let code_query_key = Field.show url_key
  let payload_query_key = Field.show payload_url_key

  let request_uri { url; code; payload; _ } =
    [ Some (code_query_key, Code.value code)
    ; CCOption.map (fun m -> payload_query_key, Payload.value m) payload
    ]
    |> CCList.keep_some
    |> Uri.add_query_params' (url |> WebhookUrl.value |> Uri.of_string)
  ;;
end

let webhook
      trigger
      ?payload
      { code; signed_up_webhook_url; email_verified_webhook_url; _ }
  =
  let open Webhook in
  (match trigger with
   | Trigger.SignedUp -> signed_up_webhook_url
   | Trigger.EmailVerified -> email_verified_webhook_url)
  |> CCOption.map (fun url -> { url; trigger; code; payload })
;;

let column_code = (Code.field, "pool_signup_codes.code") |> Query.Column.create

let column_signup_count =
  (Field.SignUpCount, "pool_signup_codes.signup_count") |> Query.Column.create
;;

let column_verification_count =
  (Field.VerificationCount, "pool_signup_codes.verification_count") |> Query.Column.create
;;

let filterable_by = None
let searchable_by = [ column_code ]
let sortable_by = [ column_code; column_signup_count; column_verification_count ]
let default_sort = Query.Sort.{ column = column_code; order = SortOrder.Descending }
let default_query = Query.create ~sort:default_sort ()
