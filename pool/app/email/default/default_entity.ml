type email_template =
  { label : Entity.TemplateLabel.t
  ; language : Pool_common.Language.t
  ; text : string
  ; html : string
  }
[@@deriving eq, show]

type default = email_template list [@@deriving eq, show]
