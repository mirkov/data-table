(in-package :numeric-table)

(export '(row-by-row add-row coerce-vectors-grid-type vector-length))

(defmethod add-row ((row list) (table column-major-table))
  "Insert a new table row

If the table is bare, initialize it and specify the build method."
  (with-slots (build-method table-data column-count row-count table-schema) table
    (assert table-schema ()
	    "The table must have a schema")
    (assert (= (length table-schema)
	       (length row)) ()
	       "Row length, ~a, does not match table schema length, ~a"
	       (length row) (length table-schema))
    (cond 
      ((null build-method)
       (setf build-method 'row-by-row
	     table-data
	     (make-nested-vector `(0 ,(column-count table))
				 :adjustable-row-count t
				 :adjustable-column-count nil)))
      ((eql build-method 'row-by-row))
      (t 
       (error "Table build-method:~a is not defined as row-by-row"
	      build-method)))
    (let ((normalized-row
	   (mapcar (lambda (value column-schema)
		     (funcall (value-normalizer column-schema)
			      value column-schema))
		   row table-schema)))
      (nested-vectors:add-row table-data normalized-row))
    (incf row-count)))

(define-test add-row
  "Test dimensions of table that was build row-by-row"
  (let ((table (test-column-table)))
    (dotimes (i-row 3)
      (add-row
       (loop for i-column below 5
	  collect (aref *flower-data* i-row i-column))
       table))
  (assert-equal 3 (row-count table))
  (assert-equal 5 (column-count table))
  (assert-number-equal 4.9 (vvref (table-data table) 0 0))
  (assert-number-equal 3.2 (vvref (table-data table) 1 1))
  (assert-number-equal 3.1 (vvref (table-data table) 2 1))
  (assert-number-equal 1.3 (vvref (table-data table) 1 2))))




(defun loaded-test-table ()
  (let ((table (test-column-table)))
    (dotimes (i-column 5)
      (setf (nth-column
	    i-column table)
       (make-array
	10
	:initial-contents (loop :for i-row below 10
			     :collect (aref *flower-data* i-row i-column)))))
    table))


(defgeneric vector-length (vector)
  (:documentation "Return vector length

VECTOR is either a CL vector or a GRID vector")
  (:method ((vector vector))
    (length vector)))

(defmethod make-vector (length (column-schema number-column-schema))
  (make-array length :element-type 'number))

(defmethod make-vector (length (column-schema string-column-schema))
  (make-array length :element-type 'string))

(defmethod make-vector (length (column-schema symbol-column-schema))
  (make-array length :element-type 'symbol))


(defmethod (setf nth-column) :before
    (column-vector
     (column-index integer)
     (column-table column-major-table)
     &key #+skip(overwrite nil))
  (with-slots (build-method table-data column-count row-count table-schema)
      column-table
    (cond
      ((null build-method)
       (setf build-method 'set-column
	     row-count (sequence-length column-vector)
	     table-data
	     (make-nested-vector `(,row-count ,column-count))))
      ((equal build-method 'set-column)
       (assert (< column-index column-count) ()
	       "Column index ~a is greater than number of columns ~a"
	       column-index column-count)
       #+skip(when
		 (and (not (null (aref table-data column-index) ))
		      (not overwrite))
	       (error "Attempting to overwrite column ~a" column-index))
       (assert (= (sequence-length column-vector) row-count) ()
	       "The new column length ~a does not match table row count ~a"
	       (sequence-length column-vector) row-count))
      (t (error "Table build method, ~a, must be either NIL or SET-COLUMN"
		build-method)))))

(defgeneric normalize-vector (vector column-schema)
  (:documentation
   "Return a vector of properly normalized values

Normalization is done using the value-normalizer specified in COLUMN-SCHEMA

The vector storage method (native, or foreign) is determined by the
type of COLUMN-SCHEMA")
  (:method (vector (column-schema column-schema))
    (let ((value-normalizer (slot-value column-schema 'value-normalizer)))
      (grid:map-grid :source vector
		     :destination-specification
		     (list (list grid:*default-grid-type* (sequence-length vector))
			   (default-type column-schema))
		     :element-function (lambda (value)
					 (funcall value-normalizer
						  value column-schema))))))


(defmethod (setf nth-column) (vector
			      (column-index integer)
			      (table column-major-table)
			      &key #+skip(overwrite nil))
  (declare (ignore overwrite))
  (with-slots (build-method table-data column-count row-count table-schema)
      table
    (setf (nested-vectors:nth-column table-data column-index)
	  (normalize-vector vector (nth-column-schema column-index
						      table)))))

(define-test set-nth-column
  "Test dimesions of table that was build column-by-column"
  (let ((table (test-column-table)))
    (dotimes (i-column 5)
      (setf (nth-column i-column table)
	    (make-array
	     3
	     :initial-contents (loop :for i-row below 3
				  :collect (aref *flower-data* i-row i-column)))))
    (assert-equal 5 (column-count table))
    (assert-equal 3 (row-count table))
    (assert-number-equal 4.9 (vvref (table-data table) 0 0))
    (assert-number-equal 3.2 (vvref (table-data table) 1 1))
    (assert-number-equal 3.1 (vvref (table-data table) 2 1))
    (assert-number-equal 1.3 (vvref (table-data table) 1 2))))


(defmethod (setf table-column) (vector
				(column-name symbol)
				(table column-major-table)
			     &key #+skip(overwrite nil))
  (let ((column-index
	 (position column-name (column-names table))))
    (setf (nth-column column-index table #+skip :overwrite #+skip overwrite) vector)))

(defmethod (setf table-column) (vector
				(column-schema column-schema)
				(table column-major-table)
				&key #+skip(overwrite nil))
  (let ((column-index
	 (position column-schema (table-schema table))))
    (setf (nth-column column-index table #+skip :overwrite #+skip overwrite) vector)))


(define-test setf-table-column
  "Test dimesions of table that was build column-by-column"
  (let* ((table (test-column-table))
	 (column-names (column-names table)))
    (dolist (column-name column-names)
      (setf (table-column
	     column-name table)
       (make-array
	3
	:initial-contents (loop :for i-row below 3
			     :collect (aref *flower-data* i-row
					    (position column-name column-names))))))
    (assert-equal 5 (column-count table))
    (assert-equal 3 (row-count table))
    (assert-number-equal 4.9 (vvref (table-data table) 0 0))
    (assert-number-equal 3.2 (vvref (table-data table) 1 1))
    (assert-number-equal 3.1 (vvref (table-data table) 2 1))
    (assert-number-equal 1.3 (vvref (table-data table) 1 2))))


(defgeneric coerce-vector-grid-type (vector column-schema)
  (:documentation 
"Return vector as either a native or foreign array, depending on
column-schema

This routine operates on a small subset of column schemas, mainly
those dealing with numbers stored as double float.  For all the
others, it returns the original vector, assumed to be in native
format.
")
  (:method (vector (column-schema t))
    (declare (ignore column-schema))
    vector))



(defmethod coerce-vectors-grid-type ((table column-major-table))
  (let ((table-schema (table-schema table))
	(table-data (table-data table)))
    (loop :for i :upfrom 0
       :for column-schema :in table-schema
       :do (setf (nested-vectors:nth-column table-data i)
		 (coerce-vector-grid-type
		  (nested-vectors:nth-column table-data i)
				 	  column-schema))))
  table)


(defmethod adjust-vectors-length ((table column-major-table))
  (let ((table-data (table-data table))
	(row-count (row-count table)))
    (iter:iter
      (iter:for column :in-nv-column table-data)
      (when (subtypep (type-of column) 'array)
	(setf column (adjust-array column row-count))))))

(defmethod init-storage ((table column-major-table))
  "Initialize storage of a column-major-table"
  (let ((table-data (table-data table))
	(table-schema (table-schema table))
	(table-length (row-count table)))
    (loop for j from 0 below (column-count table)
	 for column-schema in table-schema
	 do (setf (aref table-data j)
		  (make-vector table-length column-schema)))))
