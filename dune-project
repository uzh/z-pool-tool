(lang dune 2.8)
(generate_opam_files true)

(name pool)
(version 0.0.1)

(authors
  "Department of Economics, University of Zurich")

(source (uri https://gitlab.uzh.ch/econ/study-coordination/pool))
(license PRIVATE)
(maintainers "engineering@uzh.econ.ch")
(homepage "https://gitlab.uzh.ch/econ/study-coordination/pool")
(bug_reports "https://gitlab.uzh.ch/econ/study-coordination/pool")
(documentation "https://gitlab.uzh.ch/econ/study-coordination/pool/doc")

(package
 (name pool)
 (synopsis "The Z-Pool tool")
 (description "
TODO
")
 (depends
  dune
  (ocaml (>= 4.12.0))
  (conformist (>= 0.6.0))
  (lwt (>= 5.4.1))
  (containers (>= 3.4))
  (alcotest-lwt (>= 1.4.0))
  (ppx_deriving (>= 5.2.1))))
