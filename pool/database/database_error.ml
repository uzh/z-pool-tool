type failure =
  { label : Entity.Label.t
  ; error : Caqti_error.t
  }

exception Failed of failure

let () =
  Printexc.register_printer
  @@ function
  | Failed { label; error } ->
    Some (Format.asprintf "Database %s: %a" label Caqti_error.pp error)
  | _ -> None
;;

let create label error = { label; error }

let cause : Caqti_error.t -> Caqti_error.cause option = function
  | `Request_failed e -> Some (Caqti_error.cause (`Request_failed e))
  | `Response_failed e -> Some (Caqti_error.cause (`Response_failed e))
  | `Load_failed _
  | `Load_rejected _
  | `Connect_failed _
  | `Connect_rejected _
  | `Post_connect _
  | `Encode_rejected _
  | `Encode_failed _
  | `Decode_rejected _
  | `Response_rejected _ -> None
;;

let cause_of_exn = function
  | Failed { error; _ } | Caqti_error.Exn error -> cause error
  | _ -> None
;;
