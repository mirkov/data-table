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


(defparameter *table-schema-fa*
  (make-table-schema 'numeric-table:column-major-table
		     (list `(decade foreign-double :documentation "Decade end"
			     :value-normalizer ,(lambda (value column-schema)
							(declare (ignore column-schema))
							(float value 1d0)))
			   `(population foreign-double :documentation "Population"
			     :empty-value -1d0
			     :value-normalizer ,(lambda (value column-schema)
							(when (and (symbolp value)
							      (equal value '?))
						     (setf value (empty-value column-schema)))
						   (float value 1d0)))))
  "Schema for table using foreign storage.  Since we are using foreign
  arrays, I use a negative value of -1d0 to signify empty values.  I
  will have to normalize the data when I read it in")

(defparameter *table-fa*
  (let ((table
	 (make-table 'column-major-table *table-schema-fa*)))
    (with-input-from-file (stream *file-pathname*)
      (dotimes (i 8)
	(read-line stream))
      (read-table stream table))
    (setf (slot-value table 'numeric-table::build-method)
	  'numeric-table::set-column
	  (nth-column 1 table :overwrite t)
	  (nth-column 1 table)
	  (nth-column 0 table :overwrite t)
	  (nth-column 0 table))
    table)
  "Table of US population data by decade")

(defparameter *table-1-fa*
  (select *table-fa*
	  :where (lambda (i)
		   (< (numeric-table::vvref
		       (numeric-table::table-data *table-fa*) i 0) 1900)))
  "Subset of *table-fa* with data before year 1900.  This implementation
  is problematic since I use internal's of numeric-table")

(defparameter *table-2-fa*
  (select *table-fa*
	  :where (matching-rows *table-fa*
				(list 'decade 1800 #'<)))
  "A preferable way of selecting the data based on a range.  The value 1800 is normalized prior to comparing with table data")


(defparameter *table-2a-fa*
  (select *table-fa*
	  :where (matching-rows *table-fa*
				(list 'decade 1800 #'<=)
				(list 'decade 1700 #'>=)))
  "Selecting all data from 1700 to 1800")

(defparameter *table-3-fa*
  (select *table-fa*
	  :where (matching-rows *table-fa* '(decade 1900)))
  "Data for year 1900")

(defparameter *table-4a-fa*
  (select *table-fa*
	  :where (matching-rows *table-fa*
				`(population not-empty-p)))
  "We remove all empty values from *table-fa*

We use `not-empty-p' to trigger the match for non-empty values.  But
we have to over-ride the schema equality predicate.  It is defined `='
because we are storing numeric values.  To test for empty
values (labeled with `?' we have to use `equal')
")

(defparameter *table-4b-fa*
  (select *table-fa*
	  :where (matching-rows *table-fa*
				`(population empty-p
					     ,(lambda (arg1 arg2)
						      (not (= arg1 arg2))))))
  "We remove all empty values from *table-fa* using `empty-p' instead of `not-empty-p'

Unlike for `*table4a*'.  Just as for case 4a, we have to specify the
equality predicate.  But now we explicitly define it to return true
for values that do not match the empty symbo `?'
")

(defparameter *table-5-fa*
  ())
