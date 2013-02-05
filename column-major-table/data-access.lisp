(in-package :numeric-table)

(export '(copy-columns with-columns))

(defmethod table-size ((table column-major-table))
  (list (row-count table) (column-count table)))

(defmethod nth-row ((n integer) (table column-major-table))
  (with-slots (row-count column-count table-data) table
      (assert (< n row-count) ()
	      "Row index ~a exceeds row-count ~a" n row-count)
      (nested-vectors:row-contents
       (nested-vectors:nth-row table-data n))))

(defmethod nth-column ((n integer) (table column-major-table))
  (with-slots (column-count table-data) table
  (assert (< n column-count) ()
	  "Column index ~a is greater than number of columns ~a"
	  n column-count)
  (nested-vectors:nth-column table-data n)))

(defmacro with-bare-test-table (&body body)
  `(let ((test-table
	 (make-table 'column-major-table
		     (make-table-schema 'column-major-table
				  '((delta number) (lambda-pp number))))))
     ,@body))

(define-test table-column
  (with-bare-test-table
    (add-row '(3 4) test-table)
    (add-row '(8 5) test-table)
    (assert-number-equal 2 (row-count test-table))
    (assert-numerical-equal #(3 4)
			    (nth-row 0 test-table))
    (assert-numerical-equal #(8 5)
			    (nth-row 1 test-table))
    (assert-numerical-equal #(3 8)
			    (table-column 0 test-table))))


(defmethod table-column ((column-name symbol)
			 (table column-major-table))
  "Return column contents of column matching COLUMN-NAME"
  (nth-column
   (position column-name (table-schema table)
	     :key #'column-name)
   table))

(defmethod table-column ((column-index integer)
			 (table column-major-table))
  "Return column contents of column at COLUMN-INDEX position"
  (nested-vectors:nth-column (table-data table) column-index))


(defmethod copy-columns ((target-table column-major-table)
			(source-table column-major-table)
			&rest COLUMNS)
  "Copy contents of columns from SOURCE-TABLE to TARGET-TABLE
"
  (loop for column in columns
       do (setf (table-column column target-table)
		(table-column column source-table))))

(define-test with-columns
  (with-columns (a b c) table
    (+ 2 3)))

(defmacro with-columns ((&rest column-names) table-form &body body)
  "Expand into a body where column-name refers to (table-column column-name table)"
  (let ((table-ref (gensym)))
  `(let ((,table-ref ,table-form))
     (symbol-macrolet (,@(loop :for column-name :in column-names
			      :collect `(,column-name (table-column ',column-name ,table-ref))))
       ,@body))))
