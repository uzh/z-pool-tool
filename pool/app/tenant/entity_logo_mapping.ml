module Id = Pool_common.Id
module File = Pool_common.File

let stringify_type = function
  | `PartnerLogo -> "partner_logo"
  | `TenantLogo -> "tenant_logo"
;;

let type_of_string = function
  | "partner_logo" -> Ok `PartnerLogo
  | "tenant_logo" -> Ok `TenantLogo
  | _ -> Error "Unknown logo type"
;;

type t =
  { id : Id.t
  ; logo_type : [ `PartnerLogo | `TenantLogo ]
  ; tenant_id : Id.t
  ; file : File.t
  }
[@@deriving eq, show]

module Write = struct
  type t =
    { id : Id.t
    ; tenant_id : Id.t
    ; asset_id : Id.t
    ; logo_type : [ `PartnerLogo | `TenantLogo ]
    }
  [@@deriving eq, show]
end
