(lang dune 3.3)

(name problems)

(generate_opam_files true)

(source
 (github rajgodse/ocaml-exercises))

(authors "Rajeev Godse")

(license MIT)

(package
 (name problems)
 (synopsis "Solutions and tests to the OCaml language website exercises")
 (description "The OCaml language website has included exercises to learn the language (https://ocaml.org/problems). These are my personal solutions, including interfaces for the values to be implemented as well as test suites for the implemented functions.")
 (depends ocaml dune ounit2)
 (tags
  (educational)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
