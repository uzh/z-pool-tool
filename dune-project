(lang dune 3.7)

(generate_opam_files true)

(name pool)

(version 0.13.5)

(authors "Department of Economics, University of Zurich")

(source
 (github uzh/z-pool-tool))

(license GPL-2.0-or-later)

(maintainers "Econ IT <engineering@uzh.econ.ch>")

(homepage "https://github.com/uzh/z-pool-tool")

(bug_reports "https://github.com/uzh/z-pool-tool")

(documentation "https://uzh.github.io/z-pool-tool-documentation/")

(package
 (name pool)
 (synopsis "The Z-Pool tool")
 (description "The central platform for subject management.")
 (depends
  (alcotest-lwt :with-test)
  (astring
   (>= 0.8.5))
  (base64
   (>= 3.4.0))
  canary
  (caqti
   (and
    (>= 2.3.0)
    (< 3.0.0)))
  (caqti-driver-mariadb
   (and
    (>= 2.3.0)
    (< 3.0.0)))
  (caqti-lwt
   (and
    (>= 2.3.0)
    (< 3.0.0)))
  (conformist
   (>= 0.8.1))
  (containers
   (>= 3.14))
  (containers-data
   (>= 3.14))
  (cstruct
   (>= 6.0.1))
  digestif
  (dune-build-info
   (>= 2.8.4))
  (fmt
   (>= 0.8.8))
  (guardian
   (= 0.4.0))
  (letters
   (>= 0.4.0))
  (logs
   (>= 0.7.0))
  (lwt
   (>= 5.7.0))
  (lwt_ppx
   (>= 2.0.1))
  (mariadb
   (and
    (>= 1.2.0)
    (< 2.0.0)))
  (mirage-crypto
   (>= 1.1.0))
  (mirage-crypto-pk
   (>= 1.1.0))
  (mirage-crypto-rng
   (>= 1.2.0))
  (mtime
   (>= 2.0.0))
  (ocaml
   (>= 4.14.0))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    (>= 0.18.0)
    (or :with-dev-setup :with-test)))
  (opium
   (>= 0.20.0))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_deriving_yojson
   (>= 3.5.2))
  (ppx_fields_conv
   (>= v0.13.0))
  (ppx_inline_test :with-test)
  (ppx_sexp_conv
   (>= v0.13.0))
  (ppx_string
   (= v0.17.0))
  (ppx_variants_conv
   (= v0.17.0))
  (ppx_yojson_conv
   (= v0.17.0))
  (ptime
   (>= 0.8.1))
  (rock
   (>= 0.20.0))
  (safepass
   (>= 3.0))
  (sexplib
   (>= v0.13.0))
  sexplib0
  (stringext
   (>= 1.6.0))
  (tsort
   (>= 2.0.0))
  (tyxml-ppx
   (>= 4.6.0))
  (uri
   (>= 4.2.0))
  (uuidm
   (>= 0.9.9))
  (yojson
   (>= 2.2.2))))
