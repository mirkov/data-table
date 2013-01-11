(in-package :numeric-table)

(export '(foreign-double))

(defclass foreign-column-schema (number-column-schema)
  ()
  (:documentation "Schema for data stored in as foreign vectors
appropriate for GSLL and other C and Fortran libraries"))

(add-column-schema-short+long-names 'foreign-column 'foreign-column-schema)

(defclass foreign-double-schema (foreign-column-schema)
  ((default-type :initform 'double-float)
   (value-normalizer :initform (lambda (value column-schema)
				 (declare (ignore column-schema))
				 (float value 1d0))))
  (:documentation "Stores a vector of double float values as a foreign-array"))
(add-column-schema-short+long-names 'foreign-double 'foreign-double-schema)

(defun foreign-column-suptypep (schema)
  "Return true if SCHEMA type is a subtype of FOREIGN-COLUMN-SCHEMA"
  (subtypep (type-of schema)
	    'foreign-column-schema))


(defmethod make-vector (length (column-schema foreign-double-schema))
  (grid:make-foreign-array 'double-float :dimensions  `(,length)))

(defmethod normalize-vector :around (vector (column-schema foreign-column-schema))
  (let ((grid:*default-grid-type* 'grid:foreign-array))
    (call-next-method)))

(defmethod (setf nth-column) :before ((column-vector grid:vector-double-float)
				      (column-index integer)
				      (table column-major-table)
				   &key (overwrite nil))
  (assert (foreign-column-suptypep (nth column-index
					(table-schema table)))
	  () "Column schema: ~a, must be a foreign-column-schema"))

(defmethod vector-length ((vector grid:vector-double-float))
  (grid:dim0 vector))

(define-test set-table-foreign-column
  "Test dimesions of table that was build column-by-column"
  (let ((table (make-table 'column-major-table
			   (make-table-schema 'column-major-table
					      '((x number) (y foreign-double))))))
    (dotimes (i-column 2)
      (setf (nth-column i-column
		       table)
       (make-array
	3
	:initial-contents (loop :for i-row below 3
			     :collect (aref *flower-data* i-row i-column)))))
    (assert-equal 2 (column-count table))
    (assert-equal 3 (row-count table))
    (assert-number-equal 4.9 (vvref (table-data table) 0 0))
    (assert-number-equal 3.2 (vvref (table-data table) 1 1))
    (nth-column 1 table)))





;; Vector storage coercion.  If I start havving issues with copy-to, I
;; can revert to map-grid (which copy-to calls)
(defmethod coerce-vector-grid-type ((vector array)
				    (column-schema foreign-double-schema))
  (declare (ignore column-schema))
  (grid:copy-to vector 'grid:foreign-array 'double-float))


(defmethod coerce-vector-grid-type ((vector grid:vector-double-float)
				    (column-schema foreign-double-schema))
  (declare (ignore column-schema))
  vector)

(defmethod coerce-vector-grid-type ((vector array)
				    (column-schema number))
  (declare (ignore column-schema))
  vector)

(defmethod coerce-vector-grid-type ((vector grid:vector-double-float)
				    (column-schema number))
  (declare (ignore column-schema))
  (grid:copy-to vector 'array))
