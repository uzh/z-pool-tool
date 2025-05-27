val create_and_read_token : Lwt_switch.t -> unit -> unit Lwt.t
val deactivate_and_reactivate_token : Lwt_switch.t -> unit -> unit Lwt.t
val forge_token : Lwt_switch.t -> unit -> unit Lwt.t
val suite : (string * unit Alcotest_lwt.test_case list) list
