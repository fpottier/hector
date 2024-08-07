(lang dune 3.11)
(name hector)
(generate_opam_files true)
(package
  (name hector)
  (authors
    "François Pottier"
  )
  (maintainers
    "François Pottier"
  )
  (source (uri git+https://github.com/fpottier/hector.git))
  (homepage https://github.com/fpottier/hector/)
  (bug_reports https://github.com/fpottier/hector/issues)
  (synopsis "Vectors (dynamic arrays)")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.14))
    cppo
    (monolith :with-test)
    (ocamlfind :with-test)
  )
)
