(in-package :ntu)



"In this file I demonstrate how to read a file, extract columns or
values, and derive tables based on selection criteria

All the results are stored in special variables.

Some of the results are used in following example files.


"



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


(defparameter *raw-table-schema*
  (make-table-schema 'numeric-table:column-major-table
		     (list '(decade number :documentation "Decade end")
			   '(population number :documentation "Population"
			     :empty-value ?)))
  "Schema for table using native storage.  I have defined both columns
as `number'.  This predefines the equality and comparator operators as
+ and <.

The data-file uses `?' to mark missing data, and that is what I am
using here.  But this will cause a problem when testing for missing
values, since the test will use the equality predicate `='.  I will
have to override the predicate, as shown below.
")

(defparameter *raw-table*
  (let ((table
	 (make-table 'column-major-table *table-schema*)))
    (with-input-from-file (stream *file-pathname*)
      ;; skip comment lines
      (dotimes (i 8)
	(read-line stream))
      ;; read table
      (read-table stream table))
    table)
  "Table of US population data by decade")

;; extract data


(progn
  "extract table columns by name"
  (table-column 'decade *raw-table*)
  (table-column 'population *raw-table*))


(progn
  "extract population for year 1900"
  (value *raw-table* :column-name 'population
	 :where (matching-rows *raw-table*
			       '(decade 1900))))

;; Selecting portions of the table using `select'
(defparameter *table-1900*
  (select *raw-table*
	  :where (matching-rows *raw-table* '(decade 1900)))
  "Data for year 1900

The matching functions uses the equality predicate by default.  If the
table contained multiple 1900 entries, all of them would be returned
")

(defparameter *table-before-1900/a*
  (select *raw-table*
	  :where (matching-rows *raw-table*
				(list 'decade 1900 #'<)))
  "Preferable way of selecting the data based on a range.  We override the predicate

In general, the value 1900 is normalized prior to comparing with table
data.")


(defparameter *table-before-1900/b*
  (select *raw-table*
	  :where (lambda (i)
		   (< (numeric-table::vvref
		       (numeric-table::table-data *raw-table*) i 0) 1900)))
  "Subset of *raw-table* with data before year 1900, but explicitly
specifying the selection function.  This implementation is problematic
since I use the internals of numeric-table: vvref and column indices")

(defparameter *table-1700->1800*
  (select *raw-table*
	  :where (matching-rows *raw-table*
				(list 'decade 1800 #'<=)
				(list 'decade 1700 #'>=)))
  "We can supply multiple matching criteria.  Here we select data
between 1700 and 1800")

(defparameter *table-1800->1e8*
  (select *raw-table*
	  :where (matching-rows *raw-table*
				(list 'decade 1800 #'>=)
				(list 'population 100000000 #'<=)))
  "We can supply multiple matching criteria, on different columns.  Here we extract data between year 1800 and until the population reaches 100,000,000")



(defparameter *table-non-empty-rows*
  (select *raw-table*
	  :where (matching-rows *raw-table*
				(list 'population 'not-empty-p #'equal)))
  "We remove all empty values from *raw-table*.  This table can be used to
interpolate over the missing data.  This will be done in the file with
interpolation examples.

We use `not-empty-p' to trigger the match for non-empty values.  But
we have to over-ride the schema equality predicate.  It is defined `='
by the `number' schema.  To test for empty values (labeled with `?' we
have to use `equal')
")

(defparameter *table-empty-rows*
  (select *raw-table*
	  :where (matching-rows *raw-table*
				`(population empty-p equal)))
  "We collect all empty rows from *raw-table* using `empty-p' instead
of `not-empty-p'.  We will use that to identify years for which we
interpolate the missing data.
")

