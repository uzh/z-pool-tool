type event = Sent of Sihl__Contract_email.t

let handle_event pool : event -> unit Lwt.t = function
  | Sent email ->
    Service.Email.send
      ~ctx:[ "pool", Pool_common.Database.Label.value pool ]
      email
;;
