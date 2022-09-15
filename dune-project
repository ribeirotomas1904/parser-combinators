(lang dune 3.4)

(name parser-combinators)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github ribeirotomas1904/parser-combinators))

(authors "Nathanael Ribeiro")

(maintainers "Nathanael Ribeiro <ribeirotomas1904@gmail.com>")

(license MIT)

(package
 (name parser-combinators)
 (synopsis "A simple parser combinator library in ocaml")
 (depends
  ocaml
  dune
  ocaml-lsp-server
  ocamlformat
  ocamlformat-rpc
  utop))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
