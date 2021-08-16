(lang dune 2.8)
(name tyabt)
(generate_opam_files true)

(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(homepage "https://alan-j-hu.github.io/tyabt")
(source (github alan-j-hu/tyabt))

(package
 (name tyabt)
 (synopsis "Strongly typed many-sorted abstract binding trees (ABTs)")
 (description "\
Tyabt is an implementation of many-sorted abstract binding trees. Abstract
binding trees (ABTs) are similar to abstract syntax trees, but also keep track
of variable scopes. Many-sorted ABTs support multiple syntactic classes, known
as sorts. This library uses GADTs and phantom types to statically ensure that
only syntactically valid ABTs are representable.")
 (depends
  (ocaml (>= 4.12))))
