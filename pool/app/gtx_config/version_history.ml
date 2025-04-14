module Record = struct
  include Changelog.DefaultSettings
  include Entity

  let model = Pool_message.Field.GtxConfig
  let hide_list = [ "api_key" ]
end

include Changelog.T (Record)
