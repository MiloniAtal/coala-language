(lang dune 3.4)

(name microc)

(generate_opam_files false)

(authors "Stephen A. Edwards")

(maintainers "Weicheng Zhao")

(license MIT)

(package
 (name microc)
 (synopsis "MicroC Complier")
 (description "A MicroC Complier project originally by Stephen A. Edwards. Introduced dune to manage the project")
 (depends ocaml dune llvm llvm.analysis)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
