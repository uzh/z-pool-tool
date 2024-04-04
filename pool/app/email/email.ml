include Entity
include Event

module SmtpAuth = struct
  include Entity.SmtpAuth
  include Repo.Smtp
end

let find_unverified_by_user pool = Repo.find_by_user pool UnverifiedC
let find_verified_by_user pool = Repo.find_by_user pool VerifiedC
let find_unverified_by_address pool = Repo.find_by_address pool UnverifiedC
let delete_unverified_by_user = Repo.delete_unverified_by_user

let create_token pool address =
  let open Utils.Lwt_result.Infix in
  Service.Token.create
    ~ctx:(Database.to_ctx pool)
    [ "email", Pool_user.EmailAddress.value address ]
  ||> Token.create
;;

module Service = Email_service
module Guard = Entity_guard
