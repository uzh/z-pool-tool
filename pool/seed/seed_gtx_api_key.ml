open Gtx_config

let create pool =
  let api_key =
    let open Pool_common.Id in
    create () |> value |> ApiKey.of_string
  in
  let sender = "Pool" |> Sender.of_string in
  let config = create api_key sender in
  Created config |> handle_event pool
;;
