let () = Mirage_crypto_rng_unix.use_default ()
let block_size = 16

module Secret : sig
  type t

  val find_exn : unit -> t
  val value : t -> string
end = struct
  type t = string

  let find_exn () =
    Sihl.Configuration.read_secret ()
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_raw_string
  ;;

  let value = CCFun.id
end

module String = struct
  let key =
    let open Mirage_crypto.AES.CTR in
    Secret.(find_exn () |> value |> of_secret)
  ;;

  let ctr =
    let open Mirage_crypto_pk.Z_extra in
    let open Z in
    to_octets_be
      ~size:block_size
      (Sihl.Configuration.read_secret () |> CCString.length |> of_int)
    |> Mirage_crypto.AES.CTR.ctr_of_octets
  ;;

  let encrypt_to_string value =
    let open Mirage_crypto.AES.CTR in
    encrypt ~key ~ctr value |> Base64.encode_string
  ;;

  let decrypt_from_string value =
    let open Mirage_crypto.AES.CTR in
    Base64.decode value |> CCResult.map (decrypt ~key ~ctr)
  ;;
end
