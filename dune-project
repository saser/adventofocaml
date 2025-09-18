(lang dune 3.20)

(name adventofocaml)

(generate_opam_files true)

(source
 (github saser/adventofocaml))

(authors "Christian Persson <582273+saser@users.noreply.github.com>")

(maintainers "Christian Persson <582273+saser@users.noreply.github.com>")

(license LICENSE) ; TODO: decide on a license.

(documentation https://github.com/saser/adventofocaml)

(implicit_transitive_deps false)

(package
 (name adventofocaml)
 (synopsis "Advent of Code solutions in OCaml")
 (description "A set of Advent of Code solutions, created as a way to learn OCaml and some of its tooling.")
 (depends ocaml)
 (tags
  (adventofcode)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
