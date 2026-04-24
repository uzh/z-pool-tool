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

  let check_can_delete pool =
    let open Utils.Lwt_result.Infix in
    find_all pool
    ||> fun all ->
    if CCList.length all <= 1
    then Error Pool_message.Error.SmtpCannotDeleteLast
    else Ok ()
  ;;
end

let find_unverified_by_user pool = Repo.find_by_user pool UnverifiedC
let find_verified_by_user pool = Repo.find_by_user pool VerifiedC
let find_unverified_by_address pool = Repo.find_by_address pool UnverifiedC
let delete_unverified_by_user = Repo.delete_unverified_by_user
let token_data address = [ "email", Pool_user.EmailAddress.value address ]
let create_token pool address = Pool_token.create pool (token_data address)

let find_active_token pool address =
  token_data address |> Pool_token.find_active_by_data pool
;;

let renew_token pool address =
  let%lwt () = token_data address |> Pool_token.deactivate_all_by_data pool in
  create_token pool address
;;

module Service = Email_service
module Guard = Entity_guard

module Contact = struct
  let increment_smtp_bounce = Repo_sql.Contact.increment_smtp_bounce
  let reset_smtp_bounce = Repo_sql.Contact.reset_smtp_bounce
end
