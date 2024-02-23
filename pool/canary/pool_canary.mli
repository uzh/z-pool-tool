type t

val lifecycle : Sihl.Container.lifecycle
val register : unit -> Sihl.Container.Service.t

val notify
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?labels:string list
  -> ?additional:string
  -> exn
  -> string
  -> (unit, string) Lwt_result.t
