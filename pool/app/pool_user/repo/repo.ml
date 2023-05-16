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

module PhoneNumber = struct
  include Repo_model.PhoneNumber
end

module UnverifiedPhoneNumber = struct
  include Repo_model.UnverifiedPhoneNumber
end

module EmailAddress = struct
  include Repo_model.EmailAddress
end

module EmailVerified = struct
  include Repo_model.EmailVerified
end

let user_caqti = Repo_model.User.user_caqti
