include Entity
include Event
module Helper = Helper

let find_unverified pool = Repo.find pool UnverifiedC
let find_verified pool = Repo.find pool VerifiedC
