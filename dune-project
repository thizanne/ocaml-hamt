(lang dune 2.9)
(name hamt)
(generate_opam_files true)
(maintainers "thi.suzanne@gmail.com" "rudi.grinberg@gmail.com" "marek@xivilization.net")
(authors
  "Thibault Suzanne"
  "Rudi Grinberg")
(source (github thizanne/ocaml-hamt))
(bug_reports "https://github.com/thizanne/ocaml-hamt/issues")
(documentation "https://thizanne.github.io/ocaml-hamt/")
(license MIT)

(package
  (name hamt)
  (depends
    stdlib-shims
    (ocaml (>= 4.05.0))
    (core (and :with-test (>= v0.11)))
    (core_bench (and :with-test (>= v0.11))))
  (synopsis "Hash Array Mapped Tries")
  (description "HAMT data structure implemented in OCaml

http://gallium.inria.fr/blog/implementing-hamt-for-ocaml/"))
