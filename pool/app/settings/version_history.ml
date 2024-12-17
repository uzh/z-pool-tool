open Entity

module Record = struct
  include Changelog.DefaultSettings
  include Value

  let model = Pool_message.Field.Setting
end

include Changelog.T (Record)
