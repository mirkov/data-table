;;;; numeric-table.lisp

(defpackage #:numeric-table
  (:use #:cl #:lisp-unit)
  (:import-from :alexandria
		#:with-gensyms)
  (:import-from #:anaphora
		:awhen
		:it)
  #+export-from-package-def
  (:export :row-count
	   :column-count
	   :data-source
	   :data-author
	   :column-major-table
	   :make-table
	   :make-schema
	   :insert-row
	   :table-size
	   :nth-row
	   :nth-column
	   :table-column
	   :column-names
	   :column-documentation
	   :number
	   :symbol
	   :custom
	   :do-rows    ;; loop over rows
	   :do-columns ;; loop over columns
	   :table      ;; return whole table
	   :value      ;; return table element
	   :matching-rows
	   :interpolate ;; return numerically interpolated value
	   )
  (:documentation "Package for 2-dimensional column tables

The tables can be of two types: 
- A collection of columns with common row specifiers, really a 
  collection of f(x;y) where y is a parameter
- 2 dimensional tables of values, such as f(x,y)

The table type depends primarily on the primarily mode of access
Also the internal data storage is determined by table type"))

(antik:make-user-package "numeric-table")
