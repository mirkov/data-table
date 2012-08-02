;;;; numeric-table.asd

(asdf:defsystem #:numeric-table
  :serial t
  :description "Storage and queries of tabular data"
  :author "Mirko Vukovic <mirko.vukovic@us.tel.com>"
  :version "0.1"
  :license "Unspecified"
  :depends-on (#:lisp-unit
	       #:alexandria
	       #:anaphora)
  :components ((:file "numeric-table-package-def")
               (:file "generic-table")
	       (:file "column-major-table")
	       (:file "2d-table")))

