include Conformist__Core.CONFORMIST with type error_msg = Pool_message.Error.t

val pp_schema
  :  Format.formatter
  -> ('a, 'b, 'c) t * (string * string list) list
  -> unit

val decode_and_validate
  :  ?tags:Logs.Tag.set
  -> ('a, 'b, 'c) t
  -> input
  -> ('c, error list) result

val decoder
  :  ?tags:Logs.Tag.set
  -> ('a -> ('b, Pool_message.Error.t) result)
  -> Pool_message.Field.t
  -> 'a list
  -> ('b, Pool_message.Error.t) result

val schema_decoder
  :  ?tags:Logs.Tag.set
  -> ?default:'a
  -> (string -> ('a, error_msg) result)
  -> ('a -> string)
  -> Pool_message.Field.t
  -> ('b, 'a) Field.t

val schema_list_decoder
  :  'a decoder
  -> 'a encoder
  -> Pool_message.Field.t
  -> ('b, 'a) Field.t
