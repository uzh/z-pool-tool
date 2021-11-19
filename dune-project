(lang dune 2.8)

(generate_opam_files true)

(name pool)

(version 0.0.1)

(authors "Department of Economics, University of Zurich")

(source
 (uri git+https://gitlab.uzh.ch/econ/study-coordination/pool))

(license PRIVATE)

(maintainers "engineering@uzh.econ.ch")

(homepage "https://gitlab.uzh.ch/econ/study-coordination/pool")

(bug_reports "https://gitlab.uzh.ch/econ/study-coordination/pool")

(documentation "https://gitlab.uzh.ch/econ/study-coordination/pool/doc")

(package
 (name pool)
 (synopsis "The Z-Pool tool")
 (description "\nTODO\n")
 (depends
  dune
  (ocaml
   (>= 4.12.0))
  (conformist
   (>= 0.6.0))
  (sihl
   (and
    (>= 2.0.1)
    (< 3.0.0)))
  (sihl-user
   (and
    (>= 2.0.1)
    (< 3.0.0)))
  (sihl-token
   (and
    (>= 2.0.1)
    (< 3.0.0)))
  (sihl-email
   (and
    (>= 2.0.1)
    (< 3.0.0)))
  (sihl-queue
   (and
    (>= 2.0.1)
    (< 3.0.0)))
  (sihl-storage
   (and
    (>= 2.0.1)
    (< 3.0.0)))
  (lwt
   (>= 5.4.1))
  (logs
   (>= 0.7.0))
  (containers
   (>= 3.6))
  (caqti-driver-mariadb
   (>= 1.6.0))
  (ocamlformat
   (>= 0.18.0))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_yojson_conv
   (>= 0.14.0))
  (ppx_variants_conv
   (>= 0.14.1))
  ;; Test/Dev dependencies
  (alcotest-lwt :with-test)
  (ppx_inline_test :with-test)))
