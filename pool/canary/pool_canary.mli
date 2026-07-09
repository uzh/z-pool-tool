type t

val lifecycle : Pool_core.Container.lifecycle
val register : unit -> Pool_core.Container.Service.t

val notify
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?labels:string list
  -> ?additional:string
  -> exn
  -> string
  -> (unit, string) Lwt_result.t
