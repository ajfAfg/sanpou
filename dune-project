(lang dune 3.22)

(name sanpou)

(cram enable)

(generate_opam_files true)

(source
 (github ajfAfg/sanpou))

(authors "Shoichi Sasaki <56056962+ajfAfg@users.noreply.github.com>")

(maintainers "Shoichi Sasaki <56056962+ajfAfg@users.noreply.github.com>")

(license LICENSE)

(package
 (name sanpou)
 (synopsis "Compiler from sanpou to TLA+")
 (description
  "A compiler that translates sanpou source code into TLA+. It can also annotate TLC model checker output traces with sanpou source line numbers.")
 (depends
  (ocaml
   (= 5.2.0))
  (dune
   (= 3.22.0))
  (menhir
   (= 20260209))
  (alcotest
   (and
    (= 1.9.1)
    :with-test))
  (ocamlformat
   (= 0.29.0))
  (ppx_deriving
   (>= 6.0.3)))
 (tags
  (tla tlaplus model-checking formal-methods compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(using menhir 2.1)
