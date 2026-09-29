(lang dune 3.24)

(name euler)

(generate_opam_files true)

(source
 (github 0xmukesh/euler.ml))

(authors "Mukesh <mukesh@0xc84.fyi>")

(maintainers "Mukesh <mukesh@0xc84.fyi>")

(license "MIT")

(package
 (name euler)
 (synopsis "Project Euler solutions in OCaml")
 (description "Project Euler solutions in OCaml")
 (depends
  ocaml
  ocaml-lsp-server
  ocamlformat
  utop)
 (tags
  ("project-euler")))
