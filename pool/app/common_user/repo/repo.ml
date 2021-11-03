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

module Email = struct
  include Repo_model.Email

  let find = Repo_sql.Email.find
  let insert = Repo_sql.Email.insert
  let update = Repo_sql.Email.update
  let update_email = Repo_sql.Email.update_email
end

let user_caqti = Repo_model.user_caqti
