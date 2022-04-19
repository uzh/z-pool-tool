include Entity
include Event
include Default
module Helper = Helper

let find_unverified_by_user pool = Repo.find_by_user pool UnverifiedC
let find_verified_by_user pool = Repo.find_by_user pool VerifiedC
let find_unverified_by_address pool = Repo.find_by_address pool UnverifiedC
let delete_unverified_by_user = Repo.delete_unverified_by_user
