(lang dune 2.8)

(generate_opam_files true)

(name pool)

(version 0.6.1)

(authors "Department of Economics, University of Zurich")

(source
 (uri git+https://gitlab.uzh.ch/econ/study-coordination/pool))

(license GPL-2.0-or-later)

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
   (>= 4.14.0))
  (conformist
   (>= 0.8.1))
  (sihl
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-cache
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-email
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-queue
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-storage
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-token
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (sihl-user
   (and
    (>= 4.0.0)
    (< 5.0.0)))
  (lwt
   (>= 5.6.1))
  (logs
   (>= 0.7.0))
  (containers
   (>= 3.6))
  (containers-data
   (>= 3.6))
  (mariadb
   (and
    (>= 1.1.6)
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
   (= v0.16.0))
  canary
  (guardian
   (= 0.3.1))
  ;; Test/Dev dependencies
  (alcotest-lwt :with-test)
  (ppx_inline_test :with-test)))
