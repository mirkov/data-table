(in-package :numeric-table)

(export '(foreign-double-float))

(defclass foreign-column-schema (column-schema)
  ()
  (:documentation "Schema for data stored in as foreign vectors
appropriate for GSLL and other C and Fortran libraries"))

(defmethod make-column-schema  (name (type (eql 'foreign-double-float))
			&key documentation
			  value-normalizer &allow-other-keys)
    (let ((schema (make-instance 'foreign-column-schema
				 :name name
				 :comparator #'<
				 :equality-predicate #'=
				 :default-type 'number
				 :documentation documentation)))
      (awhen value-normalizer
	(setf (slot-value schema 'value-normalizer) it))
      schema))

(defmethod initialize-instance :after ((self (eql 'foreign-column-schema)) &key)
  (with-slots (value-normalizer default-type) self
  (setf value-normalizer
	#'(lambda (value column-schema)
	    (declare (ignore column-schema))
	    (assert (typep value default-type))
	    (float value 1d0)))))


(defmethod set-table-column ((column-table column-major-table)
			     column-identifier
			     (column-vector grid:vector-double-float)
			     &key (overwrite nil))
  (let ((column-index
	 (typecase column-identifier
	   (symbol (position column-identifier
			      (column-names column-table)))
	   (integer column-identifier)
	   (t (error "column-identifier type, ~a~, can be either an integer
or a symbol" (type-of column-identifier))))))
    (with-slots (build-method table column-count row-count table-schema) column-table
      (assert (typep (nth column-index table-schema)
			  'foreign-column-schema)
	      () "Column schema must be a foreign-column-schema")
      (assert (or (null build-method)
		  (eq 'set-column build-method)) ()
		  "Table build method, ~a, must be either NULL or SET-COLUMN"
		  build-method)
      (unless build-method
	(setf build-method 'set-column
	      row-count (grid:dim0 column-vector)))
      (assert (< column-index column-count) ()
	      "Column index ~a is greater than number of columns ~a"
	      column-index column-count)
      (when
	  (and (not (null (aref table column-index) ))
	       (not overwrite))
	(error "Attempting to overwrite column ~a" column-index))
      (assert (= (grid:dim0 column-vector) row-count) ()
	      "The new column length ~a does not match table row count ~a"
	      (grid:dim0 column-vector) row-count)
      (setf (aref table column-index) column-vector))))
