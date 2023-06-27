include Entity
module Repo = Repo

module FailedLoginAttempt = struct
  include Login_attempt_entity
  module Repo = Repo_login_attempt
end
