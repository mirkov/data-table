(asdf:defsystem #:numeric-table
  :serial t
  :description "Storage, queries, filtering, and numerical analysis of
  tabular data"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :license "MIT"
  :depends-on ("lisp-unit"
	       "alexandria"
	       "anaphora"
	       "antik"
	       "gsll"
	       "cl-csv"
	       "nested-vectors")
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
	     (:file "cmt-io")
	     (:file "cmt-row")
	     (:file "foreign-column")
	     (:file "fitting+interpolation")
	     (:file "interpolated-column")
	     (:file "column-fitting")
	     (:file "linear-least-squares-fit-column")
	     (:file "non-lin-least-sq-fit-column")
	     (:file "fft"))
	    )
   #+2d-table-implemented
   (:module "2d-table"
	    :components
	    ((:file "2d-table")))))

