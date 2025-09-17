include Entity
include Event
include Repo
include Pool_user_service

module Password = struct
  include Password

  let update = Event.update_password
end
