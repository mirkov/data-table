(in-package :numeric-table)


(export '(column-major-table))


;;; Classes and methods for instantiating column-major tables
(defclass column-major-table (numeric-table)
  ((row-count :initform 0))
  (:documentation "Table that contains multiple columns

The table can be expanded by adding both rows and columns"))

(define-test instantiate-column-major-table
  (assert-true (make-instance 'column-major-table)))


(defmethod initialize-instance :after ((self column-major-table)
				       &key
					 &allow-other-keys)
  #+skip(print 'hello)
  (with-slots (row-count column-count table-data table-schema) self
    (setf column-count (length table-schema))
    (setf table-data
	  (make-nested-vector (list row-count column-count))))
    #+skip(print 'good-bye))

(defmethod make-table ((type (eql 'column-major-table)) schema
		       &key build-method data-source data-author
			 #+skip table-data row-count)
  (let (kwds)
    (when row-count (setf (getf kwds :row-count) row-count))
    (let ((table (apply #'make-instance type
			      :table-schema schema
			      :build-method build-method
			      :data-source data-source
			      :data-author data-author
			      kwds)))
      table)))



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




(defun test-column-table (&optional rows)
  (if rows
      (make-table 'column-major-table (test-table-schema) :row-count rows)
      (make-table 'column-major-table (test-table-schema))))

(define-test make-table
  "Lightly test that we can create a table object and load a table
schema"
  (assert-equal 'column-major-table (type-of (test-column-table))))




