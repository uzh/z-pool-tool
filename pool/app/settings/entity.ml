type language =
  | En
  | De

type setting_value =
  | Languages of language list
  | Email_suffixes of string list

type setting =
  { setting : setting
  ; created_at : Sihl.timestamp
  ; updated_at : Sihl.timestamp
  }
