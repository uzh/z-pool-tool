let () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)
let block_size = 16

module Secret : sig
  type t

  val find_exn : unit -> t
  val value : t -> Cstruct.t
end = struct
  type t = Cstruct.t

  let find_exn () =
    Sihl.Configuration.read_secret ()
    |> Cstruct.of_string
    |> Mirage_crypto.Hash.SHA256.digest
  ;;

  let value = CCFun.id
end

module String = struct
  open Cstruct

  type t = Cstruct.t

  let key =
    let open Mirage_crypto.Cipher_block.AES.CTR in
    Secret.(find_exn () |> value |> of_secret)
  ;;

  let ctr =
    let open Mirage_crypto_pk.Z_extra in
    let open Z in
    to_cstruct_be
      ~size:block_size
      (Sihl.Configuration.read_secret () |> CCString.length |> of_int)
    |> Mirage_crypto.Cipher_block.AES.CTR.ctr_of_cstruct
  ;;

  let encrypt_to_string value =
    let open Mirage_crypto.Cipher_block.AES.CTR in
    encrypt ~key ~ctr (of_string value) |> to_string |> Base64.encode_string
  ;;

  let decrypt_from_string value =
    let open Mirage_crypto.Cipher_block.AES.CTR in
    Base64.decode value
    |> CCResult.map (fun value ->
      decrypt ~key ~ctr (of_string value) |> to_string)
  ;;
end
