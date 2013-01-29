(in-package :numeric-table)


(export '(column-major-table))


;;; Classes and methods for instantiating column-major tables
(defclass column-major-table (numeric-table)
  ()
  (:documentation "Table that contains multiple columns

The table can be expanded by adding both rows and columns"))

(define-test instantiate-column-major-table
  (assert-true (make-instance 'column-major-table)))


(defmethod initialize-instance :after ((self column-major-table)
				       &key
					 table-data 
					 &allow-other-keys)
  (setf (column-count self) (length (table-schema self))
	(table-data self)
	(when table-data
	    (progn
	      (assert (typep table-data 'nested-vector))
	      (assert (= (nested-vectors:column-count table-data)
			 (column-count self)))
	      table-data)
	    )))

(defmethod make-table ((type (eql 'column-major-table)) schema
		       &key build-method data-source data-author
			 table-data)
  (let* ((table-data-1
	  (or table-data
	      (make-nested-vector (list 0 (length schema)))))
	 (table (make-instance type
			       :table-data table-data-1
			       :table-schema schema
			       :build-method build-method
			       :data-source data-source
			       :data-author data-author)))
    table))



(defmethod make-table-schema ((type (eql 'column-major-table)) spec)
  "Create a table schema by looping over list SPEC

Each SPEC list element is a two element list consisting of the column
name and column type"
  (declare (ignore type))
  (loop :for i from 0
     :for column-spec in spec
     :collect (let ((column-schema (apply #'make-column-schema column-spec)))
		(setf (slot-value column-schema 'i-column) i)
		column-schema)))

(defun test-table-schema ()
  "Build a schema for the test table"
  (make-table-schema
	  'column-major-table
	  (loop
	     :for name in *column-names*
	     :for type in *column-types*
	     :for index from 0
	     collect (list name type
			   :documentation (format nil "column ~a" index)))))

(define-test make-table-schema
  "Test the structure of table schema"
  (let ((schema (test-table-schema)))
    (assert-number-equal 0 (slot-value (first schema) 'i-column))
    (assert-number-equal 1 (slot-value (second schema) 'i-column))
    (assert-number-equal 5 (length schema))))


(defmethod column-documentation ((column-name symbol)
				  (table column-major-table))
  (column-documentation column-name (table-schema table)))

(define-test column-schema
  "Lightly test the contents of column schema"
  (let ((schema (test-table-schema)))
    (assert-equal 'sepal-length (column-name (first schema)) "1")
    (assert-equal 'number (default-type (first schema)) "2")
    (assert-equal "column 2" (column-doc (find-column-schema 'petal-length schema)) "3")))




(defun test-column-table ()
  (make-table 'column-major-table (test-table-schema)))

(define-test make-table
  "Lightly test that we can create a table object and load a table
schema"
  (assert-equal 'column-major-table (type-of (test-column-table))))




