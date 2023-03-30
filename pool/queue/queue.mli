val hide : 'a Sihl.Contract.Queue.job -> Sihl.Contract.Queue.job'
val lifecycle : Sihl.Container.lifecycle

val register
  :  ?jobs:Sihl.Contract.Queue.job' list
  -> unit
  -> Sihl.Container.Service.t

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (Sihl_queue.instance, Pool_common.Message.error) Lwt_result.t

module Guard : sig
  module Access : sig
    val index : Guard.ValidationSet.t
    val read : Guard.ValidationSet.t
  end
end
