include Entity
include Event

let find_unverified_by_user pool = Repo.find_by_user pool UnverifiedC
let find_verified_by_user pool = Repo.find_by_user pool VerifiedC
let find_unverified_by_address pool = Repo.find_by_address pool UnverifiedC
let delete_unverified_by_user = Repo.delete_unverified_by_user

let create_token pool address =
  let open Utils.Lwt_result.Infix in
  Service.Token.create
    ~ctx:(Pool_tenant.to_ctx pool)
    [ "email", User.EmailAddress.value address ]
  ||> Token.create
;;
