exception Exception of string

val raise_caqti_error
  :  ( 'a
       , [< `Connect_failed of Caqti_error.connection_error
         | `Connect_rejected of Caqti_error.connection_error
         | `Decode_rejected of Caqti_error.coding_error
         | `Encode_failed of Caqti_error.coding_error
         | `Encode_rejected of Caqti_error.coding_error
         | `Load_failed of Caqti_error.load_error
         | `Load_rejected of Caqti_error.load_error
         | `Post_connect of Caqti_error.call_or_retrieve
         | `Request_failed of Caqti_error.query_error
         | `Response_failed of Caqti_error.query_error
         | `Response_rejected of Caqti_error.query_error
         | `Unsupported
         ] )
       result
  -> 'a

module Make : functor (_ : Pools_sig.ConfigSig) -> Pools_sig.Sig
