module Root = struct
  let create () =
    let%lwt () = Seed_tenant.create () in
    let%lwt () = Seed_emails.root () in
    let%lwt () = Seed_guard.create Pool_database.root in
    Lwt.return_unit
  ;;
end

module Tenant = struct
  let create db_pools () =
    Lwt_list.iter_s
      (fun pool ->
        let%lwt () = Seed_settings.create pool in
        let%lwt () = Seed_i18n.i18n pool in
        let%lwt () = Seed_emails.tenant pool in
        let%lwt () = Seed_experiment.experiments pool in
        let%lwt () = Seed_custom_fields.create pool in
        let%lwt () = Seed_user.admins pool in
        let%lwt () = Seed_user.contacts pool in
        let%lwt () = Seed_i18n.i18n pool in
        let%lwt () = Seed_location.create pool in
        let%lwt () = Seed_session.create pool in
        let%lwt () = Seed_invitation.invitations pool in
        let%lwt () = Seed_waiting_list.waiting_list pool in
        let%lwt () = Seed_assignment.assignment pool in
        let%lwt () = Seed_mailings.create pool in
        let%lwt () = Seed_filter.filter pool in
        let%lwt () = Seed_guard.create pool in
        Lwt.return_unit)
      db_pools
  ;;
end
