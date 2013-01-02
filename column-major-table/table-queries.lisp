(in-package :numeric-table)

(defgeneric extractor (table table-schema)
  (:documentation "Return a function of a row index I

This function will return the list of values of TABLE's row I in
columns identified by column-name of the new TABLE-SCHEMA

TABLE-SCHEMA is supposed to be a sub-set of TABLE's own table-schema

EXTRACTOR is used when we want to return a sub-set of the table's columns")
;;; Modeled after PCL's routine on p. 393
  (:method ((old-table column-major-table) (table-schema cons))
    (let* ((table-schema/old (table-schema old-table))
	   (data (table-data old-table))
	   (columns (mapcar
		     (lambda (column-schema/new)
		       (position-if
			(lambda (column-schema/old)
			  (equal (slot-value column-schema/new 'name)
				 (slot-value column-schema/old 'name)))
			table-schema/old))
		     table-schema)))
      #'(lambda (i-row)
	  (mapcar (lambda (i-column)
		    (vvref data i-row i-column))
		  columns)))))

(define-test extractor
  (let* ((old-table (loaded-test-table))
	 (old-schema (table-schema old-table))
	 (new-schema (list (second old-schema)
			   (fourth old-schema)))
	 (extractor (extractor old-table new-schema)))
    (assert-numerical-equal '(3.6 0.2) (funcall extractor 3))))



(defun extract-schema (column-names schema)
  (loop :for c in column-names
     :collect (find-column-schema c schema)))


(defun pick-rows (old-data row-index table-schema)
  "Create a new column-major-table data structure.  Fill it with rows
specified by row-index"
  (let* ((row-count (length row-index))
	 (col-count (length old-data))
	 (new-data (make-array col-count)))
    (dotimes (j col-count)
      (setf (aref new-data j)
	    (make-vector row-count (nth j table-schema)))
      (loop :for old-index :in row-index
	 :for new-index :upfrom 0
	 :do (setf (vvref new-data new-index j)
		   (vvref old-data old-index j))))
    new-data))

(defmethod restrict-rows (data (where function) table-schema)
  "Return a new data table whose rows satisfy the WHERE function

WHERE is a function of one argument: the table row index"
  (let* ((row-count (vector-length (aref data 0))))
    (let ((indices 
	   (loop :for i below row-count
	      :when (funcall where i)
	      :collect i)))
      (pick-rows data indices table-schema))))

(define-test restrict-rows-1
  "Test row restriction by selecting every even row"
  (let* ((table (loaded-test-table))
	 (table-data (table-data table)))
    (let ((col-count (column-count table))
	  (row-count (row-count table))
	  (new-table-data
	   (restrict-rows table-data (lambda (i)
				  (evenp i))
			  (table-schema table)))
	  (target-rows '(0 2 4 6 8)))
      (assert-equal col-count (length new-table-data) "column count")
      (assert-equal (/ row-count 2)
		    (vector-length (aref new-table-data 0)) "row count")
      (assert-true
       (vv-table-equal table-data new-table-data :target-rows target-rows)
       "table content"))))

(define-test restrict-rows-2
  "Test row restriction by selecting every row whose third column contains 1.4"
  (let* ((table (loaded-test-table))
	 (table-data (table-data table))
	 (target-rows '(0 3 5 7)))
    (let ((col-count (column-count table))
	  (row-count (row-count table))
	  (new-table-data
	   (restrict-rows table-data (lambda (i)
				  (equal 1.4 (vvref table-data i 2)))
			  (table-schema table))))
      (assert-equal col-count (length new-table-data))
      (assert-equal 4 (vector-length (aref new-table-data 0)))
      (assert-true
       (vv-table-equal table-data new-table-data :target-rows target-rows)))))

(defmethod project-columns (table-schema old-data (schema cons))
  "Return a new data-table that contains columns specified by SCHEMA"
  (let ((new-data (init-vv-array (length schema))))
    (loop
       :for new-col across new-data
       :for new-col-schema in schema
       :for col-name = (slot-value new-col-schema 'name)
       :for old-col-index = (position col-name table-schema :key #'column-name)
       :for j upfrom 0
       :when old-col-index
       :do (setf (aref new-data j) (aref old-data old-col-index)))
    new-data))

(define-test project-columns
  "The new table consists of columns 0 and 2 of the old table"
  (let* ((cm-table (loaded-test-table))
	 (table (table-data cm-table))
	 (schema (table-schema cm-table))
	 (new-schema (list (first schema) (third schema)))
	 (new-table (project-columns schema table new-schema)))
    (assert-numerical-equal (aref table 0) (aref new-table 0))
    (assert-numerical-equal (aref table 2) (aref new-table 1))))

(defun row-equality-tester/cmt (data column-names table-schema)
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
    #'(lambda (i1 i2)
        (loop for j in col-indices and test in tests
           always (funcall test (vvref data i1 j) (vvref data i2 j))))))

(define-test row-equality-tester/cmt
  "PETAL-LENGTH column has several values 1.4 an PETAL-WIDTH has
several values 0.2 We test duplication against those two columns

Thus rows 0 and 3 are duplicates
0 and 1 are not because neither columns match
0 and 5 are not because PETAL-WIDTH does not match"
  (let* ((cm-table (loaded-test-table))
	 (data (table-data cm-table))
	 (schema (table-schema cm-table))
	 (row-equality-tester (row-equality-tester/cmt data '(petal-length petal-width) schema)))
    (assert-true (not (funcall row-equality-tester 0 1)))
    (assert-true (funcall row-equality-tester 0 3))
    (assert-true (not (funcall row-equality-tester 0 5)))))

(defun distinct-rows (data column-names table-schema)
  "Remove duplicate rows from data

Remove rows from DATA that are duplicates in columns identified by
COLUMN-NAMES

TABLE-SCHEMA contains DATA's column names and equality testers.

DATA and SCHEMA must be congruent.  This has to be insured by the
caller routine"
  (let ((row-count (vector-length (aref data 0))))
    (let ((distinct-row-indices
	   (loop :for i below row-count
	      :collect i)))
      (setf distinct-row-indices
	    (delete-duplicates distinct-row-indices
			 :test (row-equality-tester/cmt
				data column-names table-schema)))
      (pick-rows data distinct-row-indices table-schema))))



(define-test distinct-rows
  "We first test duplication removal on petal-length and petal-width separately,
and then together This rows returned with the last test are a union of
rows returned in the first two tests"
  (let* ((cm-table (loaded-test-table))
	 (data (table-data cm-table))
	 (schema (table-schema cm-table)))
    (let ((target-rows '(1 4 7 9)))
      (assert-true
       (vv-table-equal data
			  (distinct-rows data '(petal-length) schema)
			  :target-rows target-rows)))
    (let ((target-rows '(4 5 8 9)))
      (assert-true
       (vv-table-equal data
			  (distinct-rows data '(petal-width) schema)
			  :target-rows target-rows)))
    (let ((target-rows '(1 4 5 7 8 9)))
      (assert-true
       (vv-table-equal data
			  (distinct-rows data '(petal-width petal-length) schema)
			  :target-rows target-rows)))))
  

(defun row-comparator/cmt (data column-names table-schema)
  (multiple-value-bind (col-indices tests)
      (loop :for column-name in column-names
	 :collect (position column-name table-schema
			    :key #'column-name) :into indices
	 :collect (comparator (find column-name table-schema
				    :key #'column-name)) :into tests
	 :finally (return (values indices tests)))
    #'(lambda (i1 i2)
	(loop
	   :for j in col-indices
	   :for test in tests
	   :for value1 = (vvref data i1 j)
	   :for value2 = (vvref data i2 j)
	   :when (funcall test value1 value2) :return t
	   :when (funcall test value2 value1) :return nil
	   :finally (return nil)))))

(define-test row-comparator/cmt
  "PETAL-LENGTH column has several values 1.4 an PETAL-WIDTH has
several values 0.2 We test comparisons against those two columns.

The first three tests compare rows in PETAL-LENGTH column
The second three in PETAL-LENGTH and PETAL-WIDTH"
  (let* ((cm-table (loaded-test-table))
	 (data (table-data cm-table))
	 (schema (table-schema cm-table)))
    (let ((row-comparator (row-comparator/cmt data '(petal-length) schema)))
      (assert-true (not (funcall row-comparator 0 1)))
      (assert-true (funcall row-comparator 0 2))
      (assert-true (not (funcall row-comparator 0 3))))
    (let ((row-comparator (row-comparator/cmt data '(petal-length petal-width) schema)))
      (assert-true (funcall row-comparator 0 2))
      (assert-true (not (funcall row-comparator 0 3)))
      (assert-true (funcall row-comparator 0 5)))))

(defun sorted-rows (data column-names table-schema)
  (let ((row-count (vector-length (aref data 0)))
	#+skip (col-count (length data)))
    (let ((sorted-row-index
	   (loop :for i below row-count
	      :collect i))
	  #+skip (new-data (init-vv-array col-count)))
      (setf sorted-row-index
	    (sort sorted-row-index
		  (row-comparator/cmt
		   data column-names table-schema)))
      (pick-rows data sorted-row-index table-schema))))




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
      (assert-true (vv-table-equal
		    data (sorted-rows data '(petal-length) schema)
		    :target-rows target-rows)))
    (let ((target-rows '(8 0 1 2 3 6 7 9 5 4)))
      (assert-true (vv-table-equal
		    data (sorted-rows data '(petal-width) schema)
		    :target-rows target-rows)))
    (let ((target-rows '(1 0 3 7 5 8 2 6 9 4)))
      (assert-true (vv-table-equal
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
      (setf data (restrict-rows data where schema)))

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

    (let ((new-table
	   (make-instance 'column-major-table :table-data data
			  :table-schema schema)))
      (setf (slot-value new-table 'column-count)
	    (length (table-data new-table))
	    (slot-value new-table 'row-count)
	    (vector-length (aref (table-data new-table) 0))
	    (slot-value new-table 'build-method) 'select)
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
		   :where (lambda (i)
			    (= (vvref data i 2) 1.4))))
	  (target-rows '(0 3 5 7)))
      (assert-number-equal 2 (column-count new-table))
      (assert-number-equal 4 (row-count new-table))
      (assert-true
       (vv-table-equal data (table-data new-table)
		       :target-rows target-rows
		       :target-cols '(0 2))))
    (let ((new-table
	   (select table
		   :columns '(petal-length petal-width)
		   :distinct '(petal-length petal-width)))
	  (target-rows '(1 4 5 7 8 9)))
      (assert-number-equal 2 (column-count new-table))
      (assert-number-equal 6 (row-count new-table))
      (assert-true
       (vv-table-equal data (table-data new-table)
		       :target-rows target-rows
		       :target-cols '(2 3)))
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
      (assert-true (vv-table-equal
		    data (table-data new-table)
		    :target-rows target-rows)))))

