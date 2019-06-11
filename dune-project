(lang dune 1.10)
(name coq)
(using coq 0.1)

; (generate_opam_files true)

(license LGPL-2.1)
(maintainers "The Coq Development Team")
(authors "The Coq Development Team")
(source (github coq/coq))
(documentation "https://coq.github.io/doc/")

(package
 (name coq)
 (depends
  (ocaml (>= 4.05.0))
  (dune  (and :build (>= 1.10.0)))
  (ocamlfind :build)
   num)
 (synopsis "The Coq Proof Assistant")
 (description "Coq is a formal proof management system. It provides
a formal language to write mathematical definitions, executable
algorithms and theorems together with an environment for
semi-interactive development of machine-checked proofs. Typical
applications include the certification of properties of programming
languages (e.g. the CompCert compiler certification project, or the
Bedrock verified low-level programming library), the formalization of
mathematics (e.g. the full formalization of the Feit-Thompson theorem
or homotopy type theory) and teaching.
"))

(package
 (name coqide-server)
 (depends
  (dune  (and :build (>= 1.10.0)))
   coq))

(package
 (name coqide)
 (depends
  (dune  (and :build (>= 1.10.0)))
   coq))

(package
 (name coq-refman)
 (depends
  (dune  (and :build (>= 1.10.0)))
   coq))
