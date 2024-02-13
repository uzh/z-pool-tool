module Assets = Seed_assets

module Root = struct
  let create () =
    let%lwt () = Seed_tenant.create () in
    let%lwt () = Seed_guard.create Pool_database.root in
    let%lwt () = Seed_smtp.create Pool_database.root in
    Lwt.return_unit
  ;;
end

module Tenant = struct
  let create ?(is_test = false) db_pools () =
    Lwt_list.iter_s
      (fun pool ->
        let seeds =
          [ Seed_experiment.experiments
          ; Seed_custom_fields.create
          ; Seed_user.admins
          ; Seed_location.create
          ; Seed_session.create
          ; Seed_invitation.invitations
          ; Seed_waiting_list.waiting_list
          ; Seed_assignment.assignment
          ; Seed_assignment.assignment
          ; Seed_mailings.create
          ; Seed_filter.filter
          ; Seed_smtp.create
          ; Seed_organisational_units.create
          ; Seed_guard.create
          ]
          @
          if is_test
          then []
          else [ Seed_user.contacts; Seed_guard.create_role_assignments ]
        in
        seeds |> Lwt_list.iter_s (fun fnc -> fnc pool))
      db_pools
  ;;

  let create_contacts db_label () = Seed_user.contacts db_label
end
