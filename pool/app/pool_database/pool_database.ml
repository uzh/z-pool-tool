include Entity
module Repo = Repo

module Logs = struct
  let add (database_label : Label.t) : Logs.Tag.set -> Logs.Tag.set =
    Logs.Tag.add Logger.tag_database (Label.value database_label)
  ;;

  let create (database_label : Label.t) : Logs.Tag.set =
    add database_label Logs.Tag.empty
  ;;
end
