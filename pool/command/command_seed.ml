module Seed = Database.Seed

let seed_data =
  Sihl.Command.make ~name:"seed" ~description:"Seed development data" (fun _ ->
      let%lwt () = Seed.Tenants.create () in
      Lwt.return_some ())
;;
