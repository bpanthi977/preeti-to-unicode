;;;; preeti->unicode.asd

(asdf:defsystem #:preeti->unicode
  :description "Converts nepali text written in preeti font to Unicode Devanagari"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :serial t
  :depends-on (:bp-utils)
  :components ((:file "package")
               (:file "preeti->unicode")))

