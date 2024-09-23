module Record = struct
  let model = Pool_message.Field.Filter
end

include Changelog.T (Entity)
