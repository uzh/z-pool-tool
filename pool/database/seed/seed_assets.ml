type file =
  { id : string
  ; filename : string
  ; filesize : int
  ; mime : string
  ; body : string
  }

let dummy_css () =
  { id = Uuidm.create `V4 |> Uuidm.to_string
  ; filename = "dummy.css"
  ; filesize = 20
  ; mime = "text/css"
  ; body = "html {font-family: sans-serif;}"
  }
;;

let dummy_icon () =
  { id = Uuidm.create `V4 |> Uuidm.to_string
  ; filename = "icon.svg"
  ; filesize = 60
  ; mime = "image/svg+xml"
  ; body =
      "<?xml version='1.0' encoding='UTF-8'?><svg \
       xmlns='http://www.w3.org/2000/svg' class='ionicon' viewBox='0 0 512 \
       512'><path d='M461.81 53.81a4.4 4.4 0 00-3.3-3.39c-54.38-13.3-180 \
       34.09-248.13 102.17a294.9 294.9 0 00-33.09 39.08c-21-1.9-42-.3-59.88 \
       7.5-50.49 22.2-65.18 80.18-69.28 105.07a9 9 0 009.8 \
       10.4l81.07-8.9a180.29 180.29 0 001.1 18.3 18.15 18.15 0 005.3 \
       11.09l31.39 31.39a18.15 18.15 0 0011.1 5.3 179.91 179.91 0 0018.19 \
       1.1l-8.89 81a9 9 0 0010.39 9.79c24.9-4 83-18.69 105.07-69.17 7.8-17.9 \
       9.4-38.79 7.6-59.69a293.91 293.91 0 0039.19-33.09c68.38-68 \
       115.47-190.86 102.37-247.95zM298.66 213.67a42.7 42.7 0 1160.38 0 42.65 \
       42.65 0 01-60.38 0z' fill='none' stroke='currentColor' \
       stroke-linecap='round' stroke-linejoin='round' stroke-width='32' \
       /><path d='M109.64 352a45.06 45.06 0 00-26.35 12.84C65.67 382.52 64 448 \
       64 448s65.52-1.67 83.15-19.31A44.73 44.73 0 00160 402.32' fill='none' \
       stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' \
       stroke-width='32' /></svg>"
  }
;;

let dummy_tenant_logo () =
  { id = Uuidm.create `V4 |> Uuidm.to_string
  ; filename = "tenant_logo.svg"
  ; filesize = 60
  ; mime = "image/svg+xml"
  ; body =
      "<svg xmlns='http://www.w3.org/2000/svg' class='ionicon' viewBox='0 0 \
       512 512'><title>Headset</title><path d='M83 \
       384c-13-33-35-93.37-35-128C48 141.12 149.33 48 256 48s208 93.12 208 \
       208c0 34.63-23 97-35 128' fill='none' stroke='currentColor' \
       stroke-linecap='round' stroke-linejoin='round' stroke-width='32'/><path \
       d='M108.39 270.13l-13.69 8h0c-30.23 17.7-31.7 72.41-3.38 122.2s75.87 \
       75.81 106.1 58.12h0l13.69-8a16.16 16.16 0 005.78-21.87L130 276a15.74 \
       15.74 0 00-21.61-5.87zM403.61 270.13l13.69 8h0c30.23 17.69 31.74 72.4 \
       3.38 122.19s-75.87 75.81-106.1 58.12h0l-13.69-8a16.16 16.16 0 \
       01-5.78-21.87L382 276a15.74 15.74 0 0121.61-5.87z' fill='none' \
       stroke='currentColor' stroke-miterlimit='10' stroke-width='32'/></svg>"
  }
;;

let dummy_partner_logo () =
  { id = Uuidm.create `V4 |> Uuidm.to_string
  ; filename = "tenant_logo.svg"
  ; filesize = 60
  ; mime = "image/svg+xml"
  ; body =
      "<svg xmlns='http://www.w3.org/2000/svg' class='ionicon' viewBox='0 0 \
       512 512'><title>Pizza</title><path d='M404.76 123.08C358.37 104.18 \
       309.69 96 256 96s-106.1 9-148.9 26.68c-8.08 3.3-15.26 9-10.07 \
       19.5C101.24 150.71 203 375 241.66 455a15.94 15.94 0 0028.72 \
       0l144.05-312.22c3.19-6.9.9-15.4-9.67-19.7z' fill='none' \
       stroke='currentColor' stroke-miterlimit='10' stroke-width='32'/><path \
       d='M436.38 82.68C384.31 62.08 320.17 48 256 48S128.65 60.78 75.48 \
       82.08C70.79 84 62 88.43 64.41 95.88L74.09 120c4 8.2 8.67 8.2 15.06 8.2 \
       1.79 0 4.29-1 7.28-2.18A442.46 442.46 0 01256 96c56.76 0 114.91 12 \
       159.6 30 3.59 1.4 5.59 2.18 7.28 2.18 6.58 0 10.38 2.19 15-8.1L447.65 \
       96c2.01-6-4.99-10.82-11.27-13.32z' fill='none' stroke='currentColor' \
       stroke-miterlimit='10' stroke-width='32'/><circle cx='192' cy='192' \
       r='32'/><circle cx='320' cy='208' r='32'/><circle cx='256' cy='320' \
       r='32'/></svg>"
  }
;;
