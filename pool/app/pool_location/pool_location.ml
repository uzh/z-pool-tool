include Entity
include Event
include Default
module Repo = Repo
module Guard = Entity_guard
module VersionHistory = Version_history

let find = Repo.find
let find_all = Repo.find_all
let find_location_file = Repo_file_mapping.find
let search = Repo.search
let search_multiple_by_id = Repo.search_multiple_by_id
let find_targets_grantable_by_target = Repo.find_targets_grantable_by_target

module Statistics = struct
  include Entity_statistics

  let current_year () = Ptime_clock.now () |> Ptime.to_year
  let create ?(year = current_year ()) = Repo_statistics.statistics year

  let year_select database_label =
    let%lwt fist_year = Repo_statistics.find_statistics_starting_year database_label in
    let current_year = current_year () in
    CCList.range current_year fist_year |> Lwt.return
  ;;
end
