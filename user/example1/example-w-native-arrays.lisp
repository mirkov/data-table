(in-package :ntu)

(defparameter *file-pathname*
  (let* ((rel-file-name "data-files/us-population-historical-data-by-decade.dat")
	 (pathname
	   (probe-file
	    (merge-pathnames rel-file-name
			     (asdf:system-source-directory "numeric-table-user")))))
    (assert pathname ()
	    "Datafile: ~a not found" rel-file-name)
    pathname)
  "Pathname")


(defparameter *table-schema*
  (make-table-schema 'numeric-table:column-major-table
		     (list '(decade number :documentation "Decade end")
			   '(population number :documentation "Population"
			     :empty-value ?)))
  "Schema for table using native storage")

(defparameter *table*
  (let ((table
	 (make-table 'column-major-table *table-schema*)))
    (with-input-from-file (stream *file-pathname*)
      (dotimes (i 8)
	(read-line stream))
      (read-table stream table))
    table)
  "Table of US population data by decade")

(defparameter *table-1*
  (select *table*
	  :where (lambda (i)
		   (< (numeric-table::vvref
		       (numeric-table::table-data *table*) i 0) 1900)))
  "Subset of *table* with data before year 1900.  This implementation
  is problematic since I use internal's of numeric-table")

(defparameter *table-2*
  (select *table*
	  :where (matching-rows *table*
				(list 'decade 1800 #'<)))
  "A preferable way of selecting the data based on a range.  The value 1800 is normalized prior to comparing with table data")


(defparameter *table-2a*
  (select *table*
	  :where (matching-rows *table*
				(list 'decade 1800 #'<=)
				(list 'decade 1700 #'>=)))
  "Selecting all data from 1700 to 1800")

(defparameter *table-3*
  (select *table*
	  :where (matching-rows *table* '(decade 1900)))
  "Data for year 1900")

(defparameter *table-4a*
  (select *table*
	  :where (matching-rows *table*
				`(population not-empty-p equal)))
  "We remove all empty values from *table*

We use `not-empty-p' to trigger the match for non-empty values.  But
we have to over-ride the schema equality predicate.  It is defined `='
because we are storing numeric values.  To test for empty
values (labeled with `?' we have to use `equal')
")

(defparameter *table-4b*
  (select *table*
	  :where (matching-rows *table*
				`(population empty-p
					     ,(lambda (arg1 arg2)
						      (not (equal arg1 arg2))))))
  "We remove all empty values from *table* using `empty-p' instead of `not-empty-p'

Unlike for `*table4a*'.  Just as for case 4a, we have to specify the
equality predicate.  But now we explicitly define it to return true
for values that do not match the empty symbo `?'
")

