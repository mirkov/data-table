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
			     :empty-value ?))))

(defparameter *table*
  (let ((table
	 (make-table 'column-major-table *table-schema*)))
    (with-input-from-file (stream *file-pathname*)
      (dotimes (i 8)
	(read-line stream))
      (read-table stream table))
    table)
  "Table of US population data by decade")

(defparameter *table1*
  (select *table*
	  :where (lambda (i)
		   (< (numeric-table::vvref
		       (numeric-table::table-data *table*) i 0) 1900)))
  "Subset of *table* with data before year 1900.  This implementation
  is problematic since I use internal's of numeric-table")

(defparameter *table2*
  (select *table*
	  :where (matching-rows *table*
				(list 'decade 1800 #'<)))
  "A preferable way of selecting the data based on a range.  Note that
  we have to execute the test on normalized data")

(defparameter *table2a*
  (select *table*
	  :where (matching-rows *table*
				(list 'decade 1800 #'<=)
				(list 'decade 1700 #'>=)))
  "Selecting all decades from 1700 to 1800")

(defparameter *table3*
  (select *table*
	  :where (matching-rows *table* '(decade 1900))))

(defparameter *table4*
  (select *table*
	  :where (matching-rows *table*
				`(population empty-p
					     ,(lambda (arg1 arg2)
						      (not (equal arg1 arg2))))))
  "We remove all empty values from *table*")

