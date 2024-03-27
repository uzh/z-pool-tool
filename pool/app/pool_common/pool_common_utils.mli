val src : Logs.src
val info : Logs.level
val warning : Logs.level
val error : Logs.level
val debug : Logs.level

val with_log_info
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Info.t
  -> Pool_message.Info.t

val with_log_success
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Success.t
  -> Pool_message.Success.t

val with_log_warning
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Warning.t
  -> Pool_message.Warning.t

val with_log_error
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ?level:Logs.level
  -> Pool_message.Error.t
  -> Pool_message.Error.t

val with_log_result_error
  :  ?src:Logs.src
  -> ?tags:Logs.Tag.set
  -> ('a -> Pool_message.Error.t)
  -> ('b, 'a) result
  -> ('b, 'a) result
