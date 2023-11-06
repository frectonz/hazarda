(lang dune 3.4)

(name hazarda)

(generate_opam_files true)

(source
 (github frectonz/hazarda))

(authors "Fraol Lemecha")

(maintainers "Fraol Lemecha")

(license MIT)

(package
 (name hazarda)
 (synopsis "A random number game")
 (description "A random number game")
 (depends ocaml dune core ANSITerminal)
 (tags
  (random number game)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
