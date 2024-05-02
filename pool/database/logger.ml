open CCFun.Infix
open Entity

module Tags = struct
  let add_label : string Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
  ;;

  let add = Label.value %> Logs.Tag.add add_label
  let create database = Logs.Tag.empty |> add database
  let extend label = CCOption.map_or ~default:(create label) (add label)
end
