(lang dune 2.9)
(generate_opam_files true)

(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(homepage "https://alan-j-hu.github.io/ocaml-sorted-abt")
(source (github alan-j-hu/ocaml-sorted-abt))

(package
 (name sorted-abt)
 (synopsis "Strongly typed many-sorted abstract binding trees (ABTs)")
 (description "Strongly typed many-sorted abstract binding trees (ABTs).

This library provides a type-safe API for manipulating abstract binding trees
of many sorts. Trees carry their sorts as a phantom type parameter.")
 (depends
  (ocaml (>= 4.12))))
