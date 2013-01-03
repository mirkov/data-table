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


(defparameter *table-schema-linear-fit*
  (make-table-schema 'numeric-table:column-major-table
		     (list `(decade foreign-double :documentation "Decade end"
			     :value-normalizer ,(lambda (value column-schema)
							(declare (ignore column-schema))
							(float value 1d0)))
			   `(population ax+b-ls-fit :documentation "Population"
			     :empty-value -1d0
			     :value-normalizer ,(lambda (value column-schema)
							(when (and (symbolp value)
							      (equal value '?))
						     (setf value (empty-value column-schema)))
						   (float value 1d0)))))
  "")

(defparameter *table-linear-fit*
  (let ((table
	 (make-table 'column-major-table *table-schema-linear-fit*)))
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
    (setf (independent-var table 'population) 'decade)
    table)
  "Table of US population data by decade")

(fit-column *table-linear-fit* 'population)
(fit-coeffs (find-column-schema 'population *table-schema-linear-fit*))


(defparameter *table-schema-linear-log-fit*
  (make-table-schema
   'numeric-table:column-major-table
   (list `(decade foreign-double :documentation "Decade end"
		  :value-normalizer ,(lambda (value column-schema)
					     (declare (ignore column-schema))
					     (float value 1d0)))
	 `(population ax+b-ls-fit :documentation "Population"
		      :empty-value 2d0
		      :value-normalizer
		      ,(lambda (value column-schema)
			       (when (and (symbolp value)
					  (equal value '?))
				 (setf value (empty-value column-schema)))
			       (log (float value 1d0))))))
		     "")

(defparameter *table-linear-log-fit*
  (let ((table
	 (make-table 'column-major-table *table-schema-linear-log-fit*)))
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
    (setf (independent-var table 'population) 'decade)
    table)
  "Table of US population data by decade")

(fit-column *table-linear-log-fit* 'population)
(fit-coeffs (find-column-schema 'population *table-schema-linear-log-fit*))
