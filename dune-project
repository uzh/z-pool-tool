(lang dune 3.7)

(generate_opam_files true)

(name pool)

(version 0.12.5)

(authors "Department of Economics, University of Zurich")

(source
 (uri git+<https://github.com/uzh/z-pool-tool>))

(license GPL-2.0-or-later)

(maintainers "<engineering@uzh.econ.ch>")

(homepage "<https://github.com/uzh/z-pool-tool>")

(bug_reports "<https://github.com/uzh/z-pool-tool>")

(documentation "<https://uzh.github.io/z-pool-tool-documentation/>")

(package
 (name pool)
 (synopsis "The Z-Pool tool")
 (description "\nTODO\n")
 (depends
  (alcotest-lwt :with-test)
  canary
  (caqti
   (>= 2.0.1))
  (caqti-driver-mariadb
   (>= 2.0.1))
  (caqti-lwt
   (>= 2.0.1))
  (conformist
   (>= 0.8.1))
  (containers
   (>= 3.14))
  (containers-data
   (>= 3.14))
  digestif
  (guardian
   (= 0.4.0))
  (logs
   (>= 0.7.0))
  (lwt
   (>= 5.7.0))
  (mariadb
   (and
    (>= 1.2.0)
    (< 2.0.0)))
  (ocaml
   (>= 4.14.0))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    (>= 0.18.0)
    (or :with-dev-setup :with-test)))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_inline_test :with-test)
  (ppx_string
   (= v0.17.0))
  (ppx_variants_conv
   (= v0.17.0))
  (ppx_yojson_conv
   (= v0.17.0))
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
  (tyxml-ppx
   (>= 4.6.0))
  (yojson
   (>= 2.2.2))))
