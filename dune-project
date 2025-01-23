(lang dune 3.7)

(generate_opam_files true)

(name pool)

(version 0.9.1)

(authors "Department of Economics, University of Zurich")

(source
 (uri git+https://github.com/uzh/z-pool-tool))

(license GPL-2.0-or-later)

(maintainers "engineering@uzh.econ.ch")

(homepage "https://github.com/uzh/z-pool-tool")

(bug_reports "https://github.com/uzh/z-pool-tool")

(documentation "https://uzh.github.io/z-pool-tool-documentation/")

(package
 (name pool)
 (synopsis "The Z-Pool tool")
 (description "\nTODO\n")
 (depends
  dune
  (ocaml
   (>= 4.14.0))
  (conformist
   (>= 0.8.1))
  (sihl
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-email
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-storage
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (lwt
   (>= 5.6.1))
  (logs
   (>= 0.7.0))
  (containers
   (>= 3.14))
  (containers-data
   (>= 3.14))
  (mariadb
   (and
    (>= 1.2.0)
    (< 2.0.0)))
  (caqti-driver-mariadb
   (>= 2.0.1))
  (caqti-lwt
   (>= 2.0.1))
  (caqti
   (>= 2.0.1))
  (ocamlformat
   (>= 0.18.0))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_string
   (>= 0.16.0))
  (ppx_yojson_conv
   (>= 0.14.0))
  (ppx_variants_conv
   (= v0.17.0))
  (tyxml-ppx
   (>= 4.6.0))
  digestif
  canary
  (guardian
   (= 0.3.2))
  ;; Test/Dev dependencies
  (alcotest-lwt :with-test)))
