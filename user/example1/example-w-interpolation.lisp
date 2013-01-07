(in-package :ntu)

"Now for some real work.

To tasks in this file.

Task 1:

I will use a table with empty rows removed and interpolate for the
missing data.  I will then overwrite the empty data in a raw array.

The interpolation will be done on a log 10 scale.  This is achieved by
normalizing the population by log 10

Task 2:

Interpolate the raw data using polynomial, spline, and akima methods.
Put a comparison plot for all three methods
"

;;; Part 1

(defparameter *log-interpolation-schema*
  (make-table-schema 'numeric-table:column-major-table
		     (list `(year foreign-double :documentation "Year")
			   `(population linear-interpolation-column :documentation "Population"
			     :empty-value 1d0
			     :value-normalizer ,(lambda (value column-schema)
							(when (and (symbolp value)
							      (equal value '?))
							  (setf value (empty-value column-schema)))
						    (log (float value 1d0) 10)))))
  "Schema for interpolation of log of values.

For the `year' column, I use the default normalizer.  For the
population column, I do a couple of things:
- Convert `?' to 1d0
- Coerce to double-float
- Take log 10
")

(defparameter *log-table*
  (let ((table
	 (make-table 'column-major-table *log-interpolation-schema*)))
    (setf (table-column 'population table)
	  (table-column 'population *table-non-empty-rows*)
	  (table-column 'year table)
	  (table-column 'year *table-non-empty-rows*))
    (setf (independent-var table 'population) 'year)
    (init-column-interp table 'population)
    table)
  "Table with non-empty data, and the population data on a log 10 scale

I could have read the table from the file.  But I can also import data
from an existing table.  While I am importing an array in cl 'array format, it is automatically converted to grid's foreign-array
")

(define-test coerce-vector-type
  "I test that the data in the new table is of foreign type"
  (assert-true (equal 'grid:vector-double-float
		      (type-of (table-column 'population *log-table*))))
  (assert-true (equal 'grid:vector-double-float
		      (type-of (table-column 'year *log-table*)))))




(progn
 "We iterate over the empty rows to fill the data of the *fa-table*.
We use the `value' function to set the table values.

A couple of notes:
- Convert `year' to double float before passing it to `evaluate'
- Manually de-normalize the interpolated values before storing them

`evaluate' is a generic function that is used to evaluate fits and
interpolation
"
(iter:iter
  (iter:for year :vector-element (table-column 'year
						 *table-empty-rows*))
  (setf (value *fa-table* 
	       :where (matching-rows *fa-table*
				     `(year ,year))
	       :column-name 'population)
	(expt 10
	      (evaluate *log-table* 'population (float year 1d0))))))


(defparameter *fa-table-interp-values-only*
  (select *fa-table*
	  :where (in 'year *table-empty-rows* *fa-table*))
  "This table consists only of interpolated values.

We use the `in' function to select the rows from *fa-table* that match
those of *table-empty-rows*.  We compare the values in the `year'
columns

This example highlights a usage issue.  I have to be careful to
specify the same table to `select' and `in'.  Otherwise, the results
are inpredicatable.
")


;;; Part 2


;; we define the interpolation schema's.  I will read the data from
;; table with the missing values already removed.  Thus I don't need
;; to specify handlng of missing values.  This could set-up a trap for
;; an unsuspecting user.  He could use the schema unaware of this
;; assumption.  May be best to declare a schema lexically just before
;; it is used to define a table.

(defparameter *interpolation-schema*
  (labels ((log-normalizer (value column-schema)
	     (declare (ignore column-schema))
	     (float (log value 10) 1d0)))
    (make-table-schema 'numeric-table:column-major-table
		       (list `(year foreign-double :documentation "Year")
			     `(population-p polynomial-interpolation-column
					    :documentation "Population"
					    :value-normalizer ,#'log-normalizer)
			     `(population-s cubic-spline-interpolation-column
					    :documentation "Population"
					    :value-normalizer ,#'log-normalizer)
			     `(population-a akima-interpolation-column
					    :documentation "Population"
					    :value-normalizer ,#'log-normalizer))))
  "Schema for the table that will test the three interpolations")

(defparameter *interpolation-table*
  (let ((table
	 (make-table 'column-major-table *interpolation-schema*)))
    (setf (table-column 'year table)
	  (table-column 'year *table-non-empty-rows*)
	  (table-column 'population-p table)
	  (table-column 'population *table-non-empty-rows*)
	  (table-column 'population-s table)
	  (table-column 'population *table-non-empty-rows*)
	  (table-column 'population-a table)
	  (table-column 'population *table-non-empty-rows*))
    (mapc (lambda (y-col)
	    (setf (independent-var table y-col) 'year)
	    (init-column-interp table y-col)) '(population-p
	  population-s population-a)) table)
  
"Table that is used to test interpolation.  All population columns
have identical data.  The only difference is in their schema, each for
a different interpolation method.

I could have read the table from the file.  But I can also import data
from an existing table.  Here I test that as I import a native array,
it is stored as a foreign array.
")

(defparameter *years*
  (let* ((table-years (table-column 'year *table-non-empty-rows*))
	 (start-year (aref table-years 0))
	 (end-year (aref table-years (1- (row-count *table-non-empty-rows*)))))
  (coerce
   (loop for year from start-year to end-year
	collect year)
   'array))
  "Vector of all years covered by the table")


;; a big plot comparing the three interpolation methods.  This code
;; does the interpolations, and plots them.
;;
;; The plot shows the wildly oscillating nature of the polynomial
;; interpolation.  Note that we are interpolating logs of the data,
;; and that is what I am plotting.  
;;
;; It is not possible to denormalize this data in a simple manner,
;; because the polynomial interpolation values exceed 1e8 (the
;; non-normalized values are 10^1e8)

#|
(gnuplot:with-png-output ("interpolation-comparisons.png" *png-dir*)
    (destructuring-bind (p-interpol s-interpol a-interpol)
    ;; interpolation part
    (mapcar
     (lambda (y-col)
       (coerce
	(loop for year across *years*
	   collect (evaluate *interpolation-table* y-col (float year 1d0)))
	'array))
     '(population-p population-s population-a))
  ;; plot part
  (gnuplot:set-to ((yrange '(3 10))
		   (xlabel "Year")
		   (ylabel "Log_10 Population")
		   (title "Comparison of polynomial, spline, and Akima"))
    (gnuplot:plot-xys *years*
		      `((,p-interpol :title "Polynomial")
			(,s-interpol :title "Spline")
			(,a-interpol :title "Akima"))))))

|#

