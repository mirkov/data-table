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
	 (print i)
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
    (incf row-count)))

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

(defmethod set-table-column ((column-table column-major-table)
			     column-identifier
			     (column-vector array)
			     &key (overwrite nil))
  (let ((column-index
	 (typecase column-identifier
	   (symbol (position column-identifier
			      (column-names column-table)))
	   (integer column-identifier)
	   (t (error "column-identifier type, ~a~, can be either an integer
or a symbol" (type-of column-identifier))))))
    (with-slots (build-method table column-count row-count table-schema) column-table
      (assert (or (null build-method)
		  (eq 'set-column build-method)) ()
		  "Table build method, ~a, must be either NULL or SET-COLUMN"
		  build-method)
      (unless build-method
	(setf build-method 'set-column
	      row-count (length column-vector)))
      (assert (< column-index column-count) ()
	      "Column index ~a is greater than number of columns ~a"
	      column-index column-count)
      (when
	  (and (not (null (aref table column-index) ))
	       (not overwrite))
	(error "Attempting to overwrite column ~a" column-index))
      (assert (= (length column-vector) row-count) ()
	      "The new column length ~a does not match table row count ~a"
	      (length column-vector) row-count)
      (setf (aref table column-index)
	    (let* ((column-schema (nth-column-schema column-index
						     column-table))
		   (value-normalizer (slot-value column-schema 'value-normalizer)))
	      (grid:map-grid :source column-vector
			     :element-function (lambda (value)
						 (funcall value-normalizer
							  value column-schema))))))))


(define-test set-table-column
  "Test dimesions of table that was build column-by-column"
  (let ((table (test-column-table)))
    (dotimes (i-column 5)
      (set-table-column
       table i-column
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


(defun loaded-test-table ()
  (let ((table (test-column-table)))
    (dotimes (i-column 5)
      (set-table-column
       table i-column
       (make-array
	10
	:initial-contents (loop :for i-row below 10
			     :collect (aref *flower-data* i-row i-column)))))
    table))
