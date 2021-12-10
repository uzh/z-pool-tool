include Entity
module Event = Event
module Repo = Repo

module Email = struct
  include Email
  include Helper.Email

  let find_unverified pool = Repo.Email.find pool UnverifiedC
  let find_verified pool = Repo.Email.find pool VerifiedC
end
