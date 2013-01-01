(in-package :numeric-table)

(export '(row-by-row insert-row))

(defmethod insert-row ((row list) (table column-major-table))
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
       (setf build-method 'row-by-row)
       (dotimes (i column-count)
	 (setf (aref table-data i) (make-array 0 :adjustable t :fill-pointer 0))))
      ((eql build-method 'row-by-row))
      (t 
       (error "Table build-method:~a is not defined as row-by-row"
	      build-method)))
    (loop
       :for value in row
       :for table-column across table-data
       :for column-schema in table-schema
       :for column-index from 0
       :do (vector-push-extend (normalize-value value column-schema)
			     table-column))
    (incf row-count)
    (dotimes (i column-count)
      (adjust-array (aref table-data i) row-count))))

(define-test insert-row
  "Test dimensions of table that was build row-by-row"
  (let ((table (test-column-table)))
    (dotimes (i-row 3)
      (insert-row
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
      (set-nth-column
       i-column table
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

(defmethod set-nth-column :before
    ((column-index integer)
     (column-table column-major-table)
     column-vector
     &key (overwrite nil))
  (with-slots (build-method table-data column-count row-count table-schema)
      column-table
    (assert (or (null build-method)
		(eq 'set-column build-method)) ()
		"Table build method, ~a, must be either NULL or SET-COLUMN"
		build-method)
    (unless build-method
      (setf build-method 'set-column
	    row-count (vector-length column-vector)))
    (assert (< column-index column-count) ()
	    "Column index ~a is greater than number of columns ~a"
	    column-index column-count)
    (when
	(and (not (null (aref table-data column-index) ))
	     (not overwrite))
      (error "Attempting to overwrite column ~a" column-index))
    (assert (= (vector-length column-vector) row-count) ()
	    "The new column length ~a does not match table row count ~a"
	    (vector-length column-vector) row-count)))

    

(defmethod set-nth-column ((column-index integer)
			   (table column-major-table)
			   vector
			   &key (overwrite nil))
  (declare (ignore overwrite))
  (with-slots (build-method table-data column-count row-count table-schema)
      table
    (setf (aref table-data column-index)
	  (let* ((column-schema (nth-column-schema column-index
						   table))
		 (value-normalizer (slot-value column-schema 'value-normalizer)))
	    (grid:map-grid :source vector
			   :element-function (lambda (value)
					       (funcall value-normalizer
							value column-schema)))))))

(define-test set-nth-column
  "Test dimesions of table that was build column-by-column"
  (let ((table (test-column-table)))
    (dotimes (i-column 5)
      (set-nth-column
       i-column table
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


(defmethod set-table-column ((column-name symbol)
			     (table column-major-table)
			     vector
			     &key (overwrite nil))
  (let ((column-index
	 (position column-name (column-names table))))
    (set-nth-column column-index table vector :overwrite overwrite)))

(defmethod set-table-column ((column-schema column-schema)
			     (table column-major-table)
			     vector
			     &key (overwrite nil))
  (let ((column-index
	 (position column-schema (table-schema table))))
    (set-nth-column column-index table vector :overwrite overwrite)))


(define-test set-table-column
  "Test dimesions of table that was build column-by-column"
  (let* ((table (test-column-table))
	 (column-names (column-names table)))
    (dolist (column-name column-names)
      (set-table-column
       column-name table
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
