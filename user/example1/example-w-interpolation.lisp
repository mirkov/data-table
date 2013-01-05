(in-package :ntu)

"Now for some real work.

I will use a table with empty rows removed and interpolate for the
missing data.  I will then overwrite the empty data in a raw array.

The interpolation will be done on a log 10 scale.  This is achieved by
normalizing the population by log 10
"

(defparameter *log-interpolation-schema*
  (make-table-schema 'numeric-table:column-major-table
		     (list `(decade foreign-double :documentation "Decade end")
			   `(population linear-interpolation-column :documentation "Population"
			     :empty-value 1d0
			     :value-normalizer ,(lambda (value column-schema)
							(when (and (symbolp value)
							      (equal value '?))
							  (setf value (empty-value column-schema)))
						    (log (float value 1d0) 10)))))
  "Schema for interpolation of log of values.

For the `decade' column, I use the default normalizer.  For the
population column, I do a couple of things:
- Convert `?' to 1d0
- Coerce to double-float
- Take log 10
")

(defparameter *log-table*
  (let ((table
	 (make-table 'column-major-table *table-schema-log-interpolation*)))
    (setf (table-column 'population table)
	  (table-column 'population *table-non-empty-rows*)
	  (table-column 'decade table)
	  (table-column 'decade *table-non-empty-rows*))
    table)
  "Table with non-empty data, and the population data on a log 10 scale

I could have read the table from the file.  But I can also import data
from an existing table.  Here I test that as I import a native array,
it is stored as a foreign array.
")

(define-test coerce-vector-type
  "I test that the data in the new table is of foreign type"
  (assert-true (equal 'grid:vector-double-float
		      (type-of (table-column 'population *log-table*))))
  (assert-true (equal 'grid:vector-double-float
		      (type-of (table-column 'decade *log-table*)))))




(progn
 "We iterate over the empty rows.  We use their `decade' data to
interpolate the missing data.  We use the `value' function to fill the
missing values.

A couple of notes:
- Convert `decade' to double float before passing it to `interp-column'
- Manually de-normalize the interpolated values before storing them
"
(iter:iter
  (iter:for decade :vector-element (table-column 'decade
						 *table-empty-rows*))
  (setf (value *fa-table* 
	       :where (matching-rows *fa-table*
				     `(decade ,decade))
	       :column-name 'population)
	(expt 10
	      (interp-column 'population (float decade 1d0)
			     *log-table*)))))


(defparameter *fa-table-interp-values-only*
  (select *fa-table*
	  :where (in 'decade *table-empty-rows* *fa-table*))
  "This table consists only of interpolated values.

We use the `in' function to select the rows from *fa-table* that match
those of *table-empty-rows*.  We compare the values in the `decade'
columns

This example highlights a usage issue.  I have to be careful to
specify the same table to `select' and `in'.  Otherwise, the results
are inpredicatable.
")





