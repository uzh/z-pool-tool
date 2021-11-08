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
   (>= 2.0.1))
  (sihl-user
   (>= 2.0.1))
  (sihl-token
   (>= 2.0.1))
  (sihl-email
   (>= 2.0.1))
  (sihl-queue
   (>= 2.0.1))
  (sihl-storage
   (>= 2.0.1))
  (lwt
   (>= 5.4.1))
  (containers
   (>= 3.6))
  (caqti-driver-mariadb
   (>= 1.6.0))
  (ocamlformat
   (>= 0.18.0))
  (alcotest-lwt
   (>= 1.4.0))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_yojson_conv
   (>= 0.14.0))
  (ppx_variants_conv
   (>= 0.14.1))))
