(in-package :numeric-table)

(defgeneric extractor (table table-schema)
  (:documentation "Return a function of a row index I

This function will return the list of values of TABLE's row I in
columns identified by column-name of the new TABLE-SCHEMA

TABLE-SCHEMA is supposed to be a sub-set of TABLE's own table-schema")
;;; Modeled after PCL's routine on p. 393
  (:method ((old-table column-major-table) (table-schema cons))
    (let* ((table-schema/old (table-schema old-table))
	   (data (table old-table))
	   (columns (mapcar
		     (lambda (column-schema/new)
		       (position-if
			(lambda (column-schema/old)
			  (equal (name column-schema/new)
				 (name column-schema/old)))
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

(defmethod restrict-rows (data (where function))
  "Return a new data table whose rows satisfy the WHERE function

WHERE is a function of two arguments: the data table and the row index"
  (let* ((column-count (length data))
	 (row-count (length (aref data 0)))
	 (new-data (make-vv-array column-count)))
    (dotimes (i row-count)
      (when (funcall where data i)
	(dotimes (j column-count)
	  (vector-push-extend (vvref data i j)
			      (aref new-data j)))))
    new-data))

(define-test restrict-rows-1
  "Test row restriction by selecting every even row"
  (let ((table (table (loaded-test-table))))
    (let ((col-count (length table))
	  (row-count (length (aref table 0)))
	  (new-table
	   (restrict-rows table (lambda (table i)
				  (declare (ignore table))
				  (evenp i)))))
    (assert-equal col-count (length new-table))
    (assert-equal (/ row-count 2)
		  (length (aref new-table 0)))
    (assert-true
     (notany #'null
	     (loop
		:for ocol across table
		:for ncol across new-table
		:collect (notany #'null
				 (loop
				    :for i below row-count by 2
				    :for old-val = (aref ocol i)
				    :for new-val = (aref ncol (/ i 2))
				    :collect (equal old-val new-val)))))))))

(define-test restrict-rows-2
  "Test row restriction by selecting every row whose third column contains 1.4"
  (let ((table (table (loaded-test-table)))
	(target-rows '(0 3 5 7)))
    (let ((col-count (length table))
	  (row-count (length (aref table 0)))
	  (new-table
	   (restrict-rows table (lambda (table i)
				  (equal 1.4f0 (vvref table i 2))))))
      (assert-equal col-count (length new-table))
      (assert-equal 4
		    (length (aref new-table 0)))
      (notany #'null
	      (loop
		 :for ocol across table
		 :for ncol across new-table
		 :collect (notany #'null
				  (loop
				     :for i in target-rows
				     :for old-val = (aref ocol i)
				     :for i-new below 4
				     :for new-val = (aref ncol i-new)
				     :collect (equal old-val new-val))))))))

(defmethod project-columns (table-schema old-data (schema cons))
  "Return a new data-table that contains columns specified by SCHEMA"
  (let ((new-data (make-vv-array (length schema))))
    (loop
       :for new-col across new-data
       :for new-col-schema in schema
       :for col-name = (name new-col-schema)
       :for old-col-index = (position col-name table-schema :key #'name)
       :for j upfrom 0
       :when old-col-index
       :do (setf (aref new-data j) (aref old-data old-col-index)))
    new-data))

(define-test project-columns
  "The new table consists of columns 0 and 2 of the old table"
  (let* ((cm-table (loaded-test-table))
	 (table (table cm-table))
	 (schema (table-schema cm-table))
	 (new-schema (list (first schema) (third schema)))
	 (new-table (project-columns schema table new-schema)))
    (assert-numerical-equal (aref table 0) (aref new-table 0))
    (assert-numerical-equal (aref table 2) (aref new-table 1))))


(defun select (&key from (columns t) where)
  "Return a sub-set of the table"
  (let ((data (table from))
        (old-schema (table-schema from))
	schema)
    (when where
      (setf data (restrict-rows data where)))

    (unless (eql columns 't)
      (setf schema (extract-schema (mklist columns) old-schema))
      (setf data (project-columns old-schema data schema)))

    (make-instance 'column-major-table :table data :table-schema schema)))


(define-test select
  ""
   (let* ((table (loaded-test-table))
	  (new-table
	   (select :from table
		   :columns '(sepal-length petal-length)
		   :where (lambda (data i)
			    (= (vvref data i 2) 1.4f0)))))
     (column-count new-table)))


