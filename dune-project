(lang dune 3.22)

(name sanpou)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name sanpou)
 (synopsis "A short synopsis")
 (description "A longer description")
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
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(using menhir 2.1)
