(in-package :ntu)

"In this file I show how to create tables using foreign-arrays.

I also demonstrate that the value extraction and table selections
still work with foreign arrays.

I define unit-tests comparing results with foreign and raw arrays
"


(defparameter *fa-table-schema*
  (make-table-schema 'numeric-table:column-major-table
		     (list `(year foreign-double :documentation "Year")
			   `(population foreign-double :documentation "Population"
			     :empty-value 1d0
			     :value-normalizer ,(lambda (value column-schema)
							(when (and (symbolp value)
							      (equal value '?))
						     (setf value (empty-value column-schema)))
						   (float value 1d0)))))
  "Schema for table using foreign storage.  `foreign-double' by default normalizes all values to double-float.

Since we are using foreign arrays, instead of `?', I use a value of
1d0 to signify empty values.")

(defparameter *fa-table*
  (let ((table
	 (make-table 'column-major-table *fa-table-schema*)))
    (with-input-from-file (stream *file-pathname*)
      (dotimes (i 8)
	(read-line stream))
      (read-table stream table))
    table)
  "Table of US population data by year.  The missing values in this
table will eventually be replaced by interpolated data.")

(define-test fa-columns
  (assert-numerical-equal (table-column 'year *raw-table*)
			  (grid:copy-to (table-column 'year *fa-table*)
					'array)))


(define-test fa-values
  (assert-number-equal
   (value *raw-table* :column-name 'population
	  :where (matching-rows *raw-table*
				'(year 1900)))
   (value *fa-table* :column-name 'population
	  :where (matching-rows *fa-table*
				'(year 1900)))))


;; Selecting portions of the table using `select'
(defparameter *fa-table-1900*
  (select *fa-table*
	  :where (matching-rows *fa-table* '(year 1900)))
  "Data for year 1900

The matching functions uses the equality predicate by default.  If the
table contained multiple 1900 entries, all of them would be returned
")

(defparameter *fa-table-before-1900/a*
  (select *fa-table*
	  :where (matching-rows *fa-table*
				(list 'year 1900 #'<)))
  "Preferable way of selecting the data based on a range.  We override the predicate

In general, the value 1900 is normalized prior to comparing with table
data.")


(defparameter *fa-table-before-1900/b*
  (select *fa-table*
	  :where (lambda (i)
		   (< (numeric-table::vvref
		       (numeric-table::table-data *fa-table*) i 0) 1900)))
  "Subset of *fa-table* with data before year 1900, but explicitly
specifying the selection function.  This implementation is problematic
since I use the internals of numeric-table: vvref and column indices")

(defparameter *fa-table-1700->1800*
  (select *fa-table*
	  :where (matching-rows *fa-table*
				(list 'year 1800 #'<=)
				(list 'year 1700 #'>=)))
  "We can supply multiple matching criteria.  Here we select data
between 1700 and 1800")

(defparameter *fa-table-1800->1e8*
  (select *fa-table*
	  :where (matching-rows *fa-table*
				(list 'year 1800 #'>=)
				(list 'population 100000000 #'<=)))
  "We can supply multiple matching criteria, on different columns.  Here we extract data between year 1800 and until the population reaches 100,000,000")

(define-test fa-selections
  (assert-numerical-equal (table-column 'year *table-1800->1e8*)
			  (grid:copy-to (table-column 'year *fa-table-1800->1e8*)
					'array)))


(defparameter *fa-table-non-empty-rows*
  (select *fa-table*
	  :where (matching-rows *fa-table*
				(list 'population 'not-empty-p #'equal)))
  "We remove all empty values from *fa-table*.  This table can be used to
interpolate over the missing data.  This will be done in the file with
interpolation examples.

We use `not-empty-p' to trigger the match for non-empty values.  But
we have to over-ride the schema equality predicate.  It is defined `='
by the `number' schema.  To test for empty values (labeled with `?' we
have to use `equal')
")


(defparameter *fa-table-empty-rows*
  (select *fa-table*
	  :where (matching-rows *fa-table*
				`(population empty-p equal)))
  "We collect all empty rows from *fa-table* using `empty-p' instead
of `not-empty-p'.  We will use that to identify years for which we
interpolate the missing data.
")

(define-test empty/non-empty-selections
  (assert-numerical-equal (table-column 'year *table-empty-rows*)
			  (grid:copy-to (table-column 'year *fa-table-empty-rows*)
					'array))
  (assert-numerical-equal (table-column 'year *table-non-empty-rows*)
			  (grid:copy-to (table-column 'year *fa-table-non-empty-rows*)
					'array)))
#|
(gnuplot:set-to ((xlabel "Year")
		 (ylabel "Population")
		 (logscale :y)
		 (yrange '(1000 5e8)))
  (gnuplot:plot-xy   (table-column 'year *fa-table-non-empty-rows*)
		     (table-column 'population *table-non-empty-rows*)))
|#
