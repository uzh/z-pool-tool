open Entity

module Paused = struct
  include Paused

  let t = Caqti_type.bool
end

module Disabled = struct
  include Disabled

  let t = Caqti_type.bool
end

module TermsAccepted = struct
  include TermsAccepted

  let t = Caqti_type.ptime
end

module Verified = struct
  include Verified

  let t = Caqti_type.ptime
end
