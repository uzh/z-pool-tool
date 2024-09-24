module Record = struct
  include Entity

  let model = Pool_message.Field.CustomField
end

include Changelog.T (Record)
module OptionVersionHistory = Changelog.T (Entity.SelectOption)
module GroupVersionHistory = Changelog.T (Entity.Group)
