module Paused = struct
  include Repo_model.Paused
end

module Disabled = struct
  include Repo_model.Disabled
end

module TermsAccepted = struct
  include Repo_model.TermsAccepted
end

module Verified = struct
  include Repo_model.Verified
end

module EmailAddress = struct
  include Repo_model.EmailAddress
end

let user_caqti = Repo_model.User.user_caqti
