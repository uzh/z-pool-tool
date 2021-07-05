type language =
  | En
  | De

type setting =
  | Languages of language list
  | Email_suffixes of string list
