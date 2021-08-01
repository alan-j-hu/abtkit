(lang dune 2.9)
(generate_opam_files true)

(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(homepage "https://alan-j-hu.github.io/ocaml-sorted-abt")
(source (github alan-j-hu/ocaml-sorted-abt))

(package
 (name sorted-abt)
 (synopsis "Strongly-typed multi-sorted abstract binding trees (ABTs)")
 (description "Strongly-typed abstract binding trees ABTs.")
 (depends
  (ocaml (>= 4.12))))

(package
 (name sorted-abt-ppx)
 (synopsis "Syntactic sugar for sorted-abt")
 (depends
  (sorted-abt (= version))
  (ppxlib (>= 0.22))))
