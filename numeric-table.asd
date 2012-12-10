;;;; numeric-table.asd

(asdf:defsystem #:numeric-table
  :serial t
  :description "Storage, queries, filtering, and numerical analysis of
  tabular data"
  :author "Mirko Vukovic <mirko.vukovic@us.tel.com>"
  :version "0.1"
  :license "Unspecified"
  :depends-on ("lisp-unit"
	       "alexandria"
	       "anaphora"
	       "antik"
	       "gsll")
  :components
  ((:module "init"
	    :serial t
	    :components
	    ((:file "numeric-table-package-def")
	     (:file "utilities")
	     (:file "generic-numeric-table")))
   (:module "column-major-table"
	    :serial t
	    :components
	    ((:file "test-data")
	     (:file "column-major-table-classes")
	     (:file "table-building")
	     (:file "table-queries")
	     (:file "matching-functions")
	     (:file "data-access")
	     (:file "foreign-column")
	     (:file "interpolated-column")
	     (:file "non-lin-least-sq-fit-column"))
	    )
   #+2d-table-implemented
   (:module "2d-table"
	    :components
	    ((:file "2d-table")))))

