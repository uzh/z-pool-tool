module String : sig
  val encrypt_to_string : string -> string
  val decrypt_from_string : string -> (string, [> `Msg of string ]) result
end
