(in-package :numeric-table)

(defgeneric extractor (table-schema)
  (:documentation "Return a function of a row accessor

This function will return the list of values of TABLE's row in
columns identified by column-name of the new TABLE-SCHEMA

TABLE-SCHEMA is supposed to be a sub-set of TABLE's own table-schema

EXTRACTOR is used when we want to return a sub-set of the table's columns")
;;; Modeled after PCL's routine on p. 393
  (:method ((table-schema cons))
    (let ((column-indices (mapcar #'i-column table-schema)))
      #'(lambda (row-accessor)
	  (mapcar (lambda (column-index)
		    (vrref row-accessor column-index))
		  column-indices)))))

(define-test extractor
  (let* ((table (loaded-test-table))
	 (old-schema (table-schema table))
	 (new-schema (list (second old-schema)
			   (fourth old-schema)))
	 (extractor (extractor new-schema)))
    (assert-numerical-equal '(3.6 0.2) (funcall extractor
						(nested-vectors:nth-row
						 (table-data table)
						 3)))))

(defmethod project-columns ((old-schema list) (old-data nested-vector)
			    (new-schema list))
  "Return a new data-table that contains columns specified by SCHEMA"
  (let ((new-data (make-nested-vector (list 0 (length new-schema)))))
    (loop
       :for new-col-schema in new-schema
       :for new-col-schema-index from 0
       :for col-name = (column-name new-col-schema)
       :for old-col-schema-index = (position col-name old-schema :key #'column-name)
       :when old-col-schema-index
       :do (setf (nested-vectors:nth-column new-data new-col-schema-index)
		 (nested-vectors:nth-column old-data old-col-schema-index)))
    new-data))

(define-test project-columns
  "The new table consists of columns 0 and 2 of the old table"
  (let* ((cm-table (loaded-test-table))
	 (table (table-data cm-table))
	 (schema (table-schema cm-table))
	 (new-schema (list (first schema) (third schema)))
	 (new-table (project-columns schema table new-schema)))
    (assert-numerical-equal (nested-vectors:nth-column table 0)
			    (nested-vectors:nth-column new-table 0))
    (assert-numerical-equal (nested-vectors:nth-column table 2)
			    (nested-vectors:nth-column new-table 1))))


(defgeneric extract-schema (column-names schema)
  (:documentation
"Return a new schema, containing columns specified by column names")
  (:method (column-names (schema list))
    (loop :for c in column-names
     :collect (find-column-schema c schema))))


#+skip(defun pick-rows (rows table-schema)
  "Create a new column-major-table data structure.  Fill it with rows
specified by row-index"
  (let* ((row-count (length row-index))
	 (col-count (length old-data))
	 (new-data (make-nested-vector `(0 ,col-count))))
    (dotimes (j col-count)
      (setf (aref new-data j)
	    (make-vector row-count (nth j table-schema)))
      (loop :for old-index :in row-index
	 :for new-index :upfrom 0
	 :do (setf (vvref new-data new-index j)
		   (vvref old-data old-index j))))
    new-data))

(defmethod restrict-rows ((data nested-vector) (where function))
  "Return a new data table whose rows satisfy the WHERE function

WHERE is a function of one argument: the table row accessor"
  (let* ((column-count (nested-vectors:column-count data))
	 (new-data
	  (make-nested-vector (list 0 column-count)
			      :adjustable-row-count t)))
    (iter:iter
      (iter:for row :in-nv-row data)
      (when (funcall where row)
	(nested-vectors:add-row new-data
	      (nested-vectors:row-contents row))))
    new-data))
    

(define-test restrict-rows-1
  "Test row restriction by selecting every even row"
  (let* ((table (loaded-test-table))
	 (table-data (table-data table)))
    (let ((col-count (column-count table))
	  (row-count (row-count table))
	  (new-table-data
	   (restrict-rows table-data
			  (lambda (row)
			    (evenp (nested-vectors:row-index row)))))
	  (target-rows '(0 2 4 6 8)))
      (assert-equal col-count (nested-vectors:column-count new-table-data)
		    "column count")
      (assert-equal (/ row-count 2)
		    (nested-vectors:row-count new-table-data)
		    "row count")
      (assert-true
       (compare-nested-vectors table-data new-table-data
			       :target-rows target-rows)
       "table content"))))

(define-test restrict-rows-2
  "Test row restriction by selecting every row whose third column contains 1.4"
  (let* ((table (loaded-test-table))
	 (table-data (table-data table))
	 (target-rows '(0 3 5 7)))
    (let ((col-count (column-count table))
	  (row-count (row-count table))
	  (new-table-data
	   (restrict-rows table-data (lambda (row)
				       (equal 1.4 (vrref row 2))))))
      (assert-equal col-count (nested-vectors:column-count new-table-data))
      (assert-equal 4 (nested-vectors:row-count new-table-data))
      (assert-true
       (compare-nested-vectors table-data new-table-data :target-rows target-rows)))))



(defun row-equality-tester (column-names table-schema)
  "Return a function of two arguments: two row indices, i1 and i2

This function returns T if the contents of rows I1 and I2 of DATA are
equal in all the columns specified by COLUMN-NAMES

TABLE-SCHEMA contains the column schema that is used to identify the
data columns for sorting, and also the equality testers.

TABLE-SCHEMA and DATA have to be compatible.  That should be ensured
by the caller routine."
  (multiple-value-bind (col-indices tests)
      (loop :for column-name in column-names
	 :collect (position column-name table-schema
			    :key #'column-name) :into indices
	 :collect (slot-value (find column-name table-schema
					    :key #'column-name)
			      'equality-predicate) :into tests
	 :finally (return (values indices tests)))
    (lambda (row1 row2)
      (loop for j in col-indices and test in tests
	   always (funcall test (vrref row1 j) (vrref row2 j))))))

(define-test row-equality-tester
  "PETAL-LENGTH column has several values 1.4 an PETAL-WIDTH has
several values 0.2 We test duplication against those two columns

Thus rows 0 and 3 are duplicates
0 and 1 are not because neither columns match
0 and 5 are not because PETAL-WIDTH does not match"
  (let* ((cm-table (loaded-test-table))
	 (data (table-data cm-table))
	 (schema (table-schema cm-table))
	 (row-equality-tester (row-equality-tester '(petal-length petal-width) schema)))
    (assert-true (not (funcall row-equality-tester
			       (nested-vectors:nth-row data 0)
			       (nested-vectors:nth-row data 1))))
    (assert-true (funcall row-equality-tester (nested-vectors:nth-row data 0)
			  (nested-vectors:nth-row data 3)))
    (assert-true (not (funcall row-equality-tester (nested-vectors:nth-row data 0)
			       (nested-vectors:nth-row data 5))))))

(defgeneric distinct-rows (data column-names table-schema)
  (:documentation
  "Remove duplicate rows from data

Remove rows from DATA that are duplicates in columns identified by
COLUMN-NAMES

TABLE-SCHEMA contains DATA's column names and equality testers.

DATA and SCHEMA must be congruent.  This has to be insured by the
caller routine")
  (:method ((data nested-vector) column-names (table-schema list))
  (let* ((rows (iter:iter
		(iter:for row :in-nv-row data)
		(iter:collect row)))
	 (distinct-rows (delete-duplicates
			 rows
			 :test (row-equality-tester column-names table-schema)))
	 (new-data (make-nested-vector (list 0 (length table-schema))
				       :adjustable-row-count t)))
    (loop for row in distinct-rows
	 do (nested-vectors:add-row new-data
				    (nested-vectors:row-contents row)))
    new-data)))



(define-test distinct-rows
  "We first test duplication removal on petal-length and petal-width separately,
and then together This rows returned with the last test are a union of
rows returned in the first two tests"
  (let* ((cm-table (loaded-test-table))
	 (data (table-data cm-table))
	 (schema (table-schema cm-table)))
    (let ((target-rows '(1 4 7 9)))
      (assert-true
       (compare-nested-vectors
	data
	(distinct-rows data '(petal-length) schema)
	:target-rows target-rows)))
    (let ((target-rows '(4 5 8 9)))
      (assert-true
       (compare-nested-vectors
	data
			  (distinct-rows data '(petal-width) schema)
			  :target-rows target-rows)))
    (let ((target-rows '(1 4 5 7 8 9)))
      (assert-true
       (compare-nested-vectors data
			  (distinct-rows data '(petal-width petal-length) schema)
			  :target-rows target-rows)))))
  

(defun row-comparator (column-names table-schema)
  "Return a function of two arguments: two row indices, i1 and i2

This function returns T if the contents of rows I1 and I2 of DATA are
equal in all the columns specified by COLUMN-NAMES

TABLE-SCHEMA contains the column schema that is used to identify the
data columns for sorting, and also the equality testers.

TABLE-SCHEMA and DATA have to be compatible.  That should be ensured
by the caller routine."
  (multiple-value-bind (col-indices tests)
      (loop :for column-name in column-names
	 :collect (position column-name table-schema
			    :key #'column-name) :into indices
	 :collect (slot-value (find column-name table-schema
				    :key #'column-name)
			      'comparator) :into tests
	 :finally (return (values indices tests)))
    (lambda (row1 row2)
      (loop :for j in col-indices :and test in tests
	 :for value1 = (vrref row1 j)
	 :for value2 = (vrref row2 j)
	 :when (funcall test value1 value2) :return t
	 :when (funcall test value2 value1) :return nil
	 :finally (return nil)))))

(define-test row-comparator
  "PETAL-LENGTH column has several values 1.4 an PETAL-WIDTH has
several values 0.2 We test comparisons against those two columns.

The first three tests compare rows in PETAL-LENGTH column
The second three in PETAL-LENGTH and PETAL-WIDTH"
  (let* ((cm-table (loaded-test-table))
	 (data (table-data cm-table))
	 (schema (table-schema cm-table)))
    (let ((row-comparator (row-comparator '(petal-length) schema)))
      (assert-true (not (funcall row-comparator
				 (nested-vectors:nth-row data 0)
				 (nested-vectors:nth-row data 1))))
      (assert-true (funcall row-comparator
			    (nested-vectors:nth-row data 0)
			    (nested-vectors:nth-row data 2)))
      (assert-true (not (funcall row-comparator
				 (nested-vectors:nth-row data 0)
				 (nested-vectors:nth-row data 3)))))
    (let ((row-comparator (row-comparator '(petal-length petal-width) schema)))
      (assert-true (funcall row-comparator
			    (nested-vectors:nth-row data 0)
			    (nested-vectors:nth-row data 2)))
      (assert-true (not (funcall row-comparator
				 (nested-vectors:nth-row data 0)
				 (nested-vectors:nth-row data 3))))
      (assert-true (funcall row-comparator
			    (nested-vectors:nth-row data 0)
			    (nested-vectors:nth-row data 5))))))


(defgeneric sorted-rows (data column-names schema)
  (:documentation
"Return the data structre sorted by rows")
  (:method ((data nested-vector) column-names (table-schema list))
  (let* ((rows (iter:iter
		(iter:for row :in-nv-row data)
		(iter:collect row)))
	 (sorted-rows (sort rows
			    (row-comparator column-names
					    table-schema)))
	 (new-data (make-nested-vector (list 0 (length table-schema))
				       :adjustable-row-count t)))
    (loop for row in sorted-rows
	 do (nested-vectors:add-row new-data
				    (nested-vectors:row-contents row)))
    new-data)))		    





(define-test sorted-rows
  "PETAL-LENGTH column has several values 1.4 an PETAL-WIDTH has
several values 0.2 We test comparisons against those two columns.

The first three tests compare rows in PETAL-LENGTH column
The second three in PETAL-LENGTH and PETAL-WIDTH
The last three use PETAL-LENGTH and PETAL-WIDTH.  PETAL-WIDTH is
the tie-breaker for rows 5 and 7.  Otherwise, the sort is the
same as for PETAL-LENGTH"
  (let* ((cm-table (loaded-test-table))
	 (data (table-data cm-table))
	 (schema (table-schema cm-table)))
    (let ((target-rows '(1 0 3 5 7 2 6 8 9 4)))
      (assert-true (compare-nested-vectors
		    data (sorted-rows data '(petal-length) schema)
		    :target-rows target-rows)))
    (let ((target-rows '(8 0 1 2 3 6 7 9 5 4)))
      (assert-true (compare-nested-vectors
		    data (sorted-rows data '(petal-width) schema)
		    :target-rows target-rows)))
    (let ((target-rows '(1 0 3 7 5 8 2 6 9 4)))
      (assert-true (compare-nested-vectors
		    data (sorted-rows data '(petal-length petal-width) schema)
		    :target-rows target-rows)))))

   
(defmethod select ((table column-major-table) &key (columns t) where
						distinct order-by)
  "Return select rows from a column-major-table"
  (let ((data (table-data table))
        (old-schema (table-schema table))
	(schema (table-schema table)))

    ;; First part of the code prunes the rows and columns via explicit tests
    (when where
      (setf data (restrict-rows data where)))

    (unless (eql columns 't)
      (setf schema (extract-schema (mklist columns) old-schema)
	    data (project-columns old-schema data schema))
      (loop :for column-schema :in schema
	 :for column-index :upfrom 0
	 :do (setf (slot-value column-schema 'i-column) column-index)))

    ;; Next, if needed, we remove distinct rows
    (when distinct
      (setf data
	    (distinct-rows data (mklist distinct) schema)))

    (when order-by
      (setf data
	    (sorted-rows data (mklist order-by) schema)))

    (iter:iter
      (iter:for column :in-nv-column data)
      (when (subtypep (type-of column) 'array)
	(setf column (adjust-array column (nested-vectors:row-count data)))))

    (setf (adjustable-row-count data) nil)
    (let ((new-table
	   (make-table 'column-major-table
		       schema
		       :table-data data)))
      (setf (column-count new-table)
	    (nested-vectors:column-count data)
	    (row-count new-table)
	    (nested-vectors:row-count data)
	    (build-method new-table) 'select)

      (coerce-vectors-grid-type new-table)
      new-table)))


(define-test select/column-major-table
  "We select columns 0 and 1 from the test table, and rows in columns
3 that contain 1.4f0

We test on the new table dimensions and contents"
  (let* ((table (loaded-test-table))
	 (data (table-data table)))
    (let ((new-table
	   (select table
		   :columns '(sepal-length petal-length)
		   :where (lambda (row)
			    (= (vrref row 2) 1.4))))
	  (target-rows '(0 3 5 7)))
      (assert-number-equal 2 (column-count new-table))
      (assert-number-equal 4 (row-count new-table))
      (assert-true
       (compare-nested-vectors data (table-data new-table)
			       :target-rows target-rows
			       :target-columns '(0 2))))
    (let ((new-table
	   (select table
		   :columns '(petal-length petal-width)
		   :distinct '(petal-length petal-width)))
	  (target-rows '(1 4 5 7 8 9)))
      (assert-number-equal 2 (column-count new-table))
      (assert-number-equal 6 (row-count new-table))
      (assert-true
       (compare-nested-vectors data (table-data new-table)
		       :target-rows target-rows
		       :target-columns '(2 3)))
      (assert-number-equal 0
			   (slot-value (find-column-schema 'petal-length new-table)
				       'i-column))
      (assert-number-equal 1
			   (slot-value (find-column-schema 'petal-width new-table)
				       'i-column)))
    (let ((new-table
	   (select table
		   :order-by 'petal-length))
	  (target-rows '(1 0 3 5 7 2 6 8 9 4)))
      (assert-true (compare-nested-vectors
		    data (table-data new-table)
		    :target-rows target-rows)))))

