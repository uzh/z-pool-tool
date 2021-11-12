module Id = Pool_common.Id
module File = Pool_common.File

module LogoType = struct
  type t =
    | PartnerLogo
    | TenantLogo
  [@@deriving eq, show]

  let of_string = function
    | "partner_logo" -> Ok PartnerLogo
    | "tenant_logo" -> Ok TenantLogo
    | _ -> Error Pool_common.Message.(Invalid LogoType)
  ;;

  let to_string = function
    | PartnerLogo -> "partner_logo"
    | TenantLogo -> "tenant_logo"
  ;;

  let all () = [ PartnerLogo; TenantLogo ] |> CCList.map to_string
end

type mapping =
  { id : Id.t
  ; logo_type : LogoType.t
  ; tenant_id : Id.t
  ; file : File.t
  }
[@@deriving eq, show]

module Write = struct
  type t =
    { id : Id.t
    ; tenant_id : Id.t
    ; asset_id : Id.t
    ; logo_type : LogoType.t
    }
  [@@deriving eq, show]
end
