open Caqti_request.Infix
open CCFun.Infix

module Email = struct
  let t =
    let open Utils.Crypto.String in
    Pool_common.Repo.make_caqti_type
      Caqti_type.string
      (decrypt_from_string
       %> CCResult.map_err (fun _ -> Pool_message.(Error.Decode Field.Email)))
      encrypt_to_string
  ;;
end

module Reason = Pool_common.Repo.Model.SelectorType (Entity.Reason)

let insert_request =
  {sql|
    INSERT INTO pool_archived_email_addresses (
      uuid,
      email, 
      reason
    ) VALUES (
      UNHEX(REPLACE(UUID(), '-', '')),
      $1,
      $2
    )
  |sql}
  |> Caqti_type.(t2 Email.t Reason.t ->. unit)
;;
