open CCFun.Infix
open Entity

module Tags : sig
  val add_label : string Logs.Tag.def
  val add : Label.t -> Logs.Tag.set -> Logs.Tag.set
  val create : Label.t -> Logs.Tag.set
end = struct
  let add_label : string Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
  ;;

  let add = Label.value %> Logs.Tag.add add_label
  let create database = Logs.Tag.empty |> add database
end
