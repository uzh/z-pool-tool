type tenant =
  { id : string
  ; title : string
  ; description : string
  ; url : string
  ; database : string
  ; styles : string
  ; icon : string
  ; logos : string
  ; partner_logos : string
  ; disabled : bool
  ; created_at : bool
  ; updated_at : bool
  }
[@@deriving eq, show]
