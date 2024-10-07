open Pool_message
module Model = Test_utils.Model
module Command = Cqrs_command.Api_key_command

let get_exn = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label

module Data = struct
  let name = "API Key Name"
  let urlencoded = [ Field.(show Name), [ name ] ]
end

let create () =
  let open Api_key in
  let id = Id.create () in
  let token = Token.generate () in
  let events =
    let open CCResult in
    let open Command.Create in
    Data.urlencoded |> decode >>= handle ~id ~token
  in
  let expected =
    let api_key = Api_key.create ~id ~token (Name.of_string Data.name) in
    Ok [ Created api_key |> Pool_event.api_key ]
  in
  Test_utils.check_result ~msg:"Create API Key" events expected
;;

let update () =
  let open Api_key in
  let api_key = Model.create_api_key () in
  let name = "Updated API Key Name" in
  let events =
    let open CCResult in
    let open Command.Update in
    [ Field.(show Name), [ name ] ] |> decode >>= handle api_key
  in
  let expected =
    Ok
      [ Updated (api_key, { api_key with name = Name.of_string name })
        |> Pool_event.api_key
      ]
  in
  Test_utils.check_result ~msg:"Update API Key" events expected
;;
