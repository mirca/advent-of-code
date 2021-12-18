(lang dune 2.9)

(name advent_of_code)

(generate_opam_files true)

(license MIT)

(authors "Ze Vinicius <jvmirca@gmail.com>")

(authors "Ze Vinicius <jvmirca@gmail.com>")

(source
 (github mirca/advent-of-code))

(bug_reports "https://github.com/mirca/advent-of-code/issues")

(homepage "https://github.com/mirca/advent-of-code/")

(documentation "https://github.com/mirca/advent-of-code")

(package
 (name advent_of_code)
 (synopsis "my solutions to advent of code in ocaml")
 (description "my solutions to advent of code in ocaml")
 (depends
  (alcotest :with-test)
  (ocaml
   (>= 4.02.3))
  dune))
