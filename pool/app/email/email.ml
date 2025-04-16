include Entity
include Event

module SmtpAuth = struct
  include Entity.SmtpAuth
  include Repo.Smtp

  let defalut_is_set pool =
    let open Utils.Lwt_result.Infix in
    Email_service.Cache.find_default pool
    |> function
    | Some _ -> Lwt.return_true
    | None ->
      find_full_default pool
      ||> (function
       | Error _ -> false
       | Ok smtp ->
         let () = Email_service.Cache.add pool smtp in
         true)
  ;;
end

let find_unverified_by_user pool = Repo.find_by_user pool UnverifiedC
let find_verified_by_user pool = Repo.find_by_user pool VerifiedC
let find_unverified_by_address pool = Repo.find_by_address pool UnverifiedC
let delete_unverified_by_user = Repo.delete_unverified_by_user

let create_token pool address =
  let open Utils.Lwt_result.Infix in
  Pool_token.create pool [ "email", Pool_user.EmailAddress.value address ]
  ||> Token.create
;;

module Service = Email_service
module Guard = Entity_guard
