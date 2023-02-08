val hide : 'a Sihl.Contract.Queue.job -> Sihl.Contract.Queue.job'
val lifecycle : Sihl.Container.lifecycle

val register
  :  ?jobs:Sihl.Contract.Queue.job' list
  -> unit
  -> Sihl.Container.Service.t
