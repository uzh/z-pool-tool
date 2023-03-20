include Entity
module Repo = Repo

module Logger = struct
  module Tags = struct
    let add (database_label : Label.t) : Logs.Tag.set -> Logs.Tag.set =
      Logs.Tag.add Logger.tag_database (Label.value database_label)
    ;;

    let create (database_label : Label.t) : Logs.Tag.set =
      add database_label Logs.Tag.empty
    ;;
  end
end

let test_and_create url label =
  let%lwt connection =
    url
    |> Uri.of_string
    |> CCFun.flip Caqti_lwt.with_connection (fun _ -> Lwt_result.return ())
  in
  match connection with
  | Ok _ -> create label url |> Lwt_result.lift
  | Error _ -> Lwt_result.fail Pool_common.Message.(Invalid Field.DatabaseUrl)
;;
