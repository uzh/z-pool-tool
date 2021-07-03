type sign_up_command =
  { email : string
  ; password : string
  ; firstname : string
  ; lastname : string
  ; recruitment_channel : string
  ; terms_accepted_at : Sihl.timestamp
  }

type handle_sign_up_command =
  ?allowed_email_suffixes:string list
  -> ?password_policy:(string -> (unit, string) Result.t)
  -> sign_up_command
  -> (Event.t list, string) Result.t

let handle_sign_up_command : handle_sign_up_command =
 fun ?(allowed_email_suffixes = []) ?password_policy _ ->
  allowed_email_suffixes |> ignore;
  password_policy |> ignore;
  failwith "todo"
;;
