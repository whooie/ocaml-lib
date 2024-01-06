(lang dune 3.12)
(generate_opam_files true)
(name whooie)
(authors "whooie <wchooie@gmail.com>")

; (maintainers "whooie <wchooie@gmail.com>")
; (source (github whooie/CHANGEME))
; (license CHANGEME)
; (documentation https://CHANGEME.com)

(package
 (name whooie)
 (synopsis "Personal library.")
 (description "Collection of definitions for my own personal use.")
 (depends
  ocaml
  dune
  ; owl
 )
 ; (tags (topics ...))
)
