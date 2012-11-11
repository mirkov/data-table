(in-package :numeric-table)

(defun mklist (thing)
  (if (listp thing) thing (list thing)))

(defun make-vv-array (columns)
  "Return a nested vector array

The outer vector is of length COLUMNS

Each of its elements is initialized as an adjustable vector of
dimension 0 and fill-pointer set to 0"
  (let ((vv-array (make-array columns)))
    (dotimes (j columns)
      (setf (aref vv-array j) (make-array 0 :adjustable t :fill-pointer 0)))
    vv-array))


(defun vvref (nested-vector i-row i-col)
  (aref (aref nested-vector i-col) i-row))

(defun set-vvref (nested-vector i-row i-col value)
  (setf (aref (aref nested-vector i-col) i-row) value))

(defsetf vvref set-vvref)


(defun vv-table-equality (ref-table test-table
			  &key (test #'equal)
			    (target-rows (loop for i below (length (aref ref-table 0))
					      collect i))
			    (target-cols (loop for i below (length ref-table)
					      collect i)))
  "Return true if elements in REF-TABLE match those in TEST-TABLE

The two tables have the same number of columns, but REF-TABLE has
fewer rows.  TARGET-ROWS is a list of row indices of REF-TABLE.

The i-th row of TEST-TABLE is tested against the row addressed by
the i-th element of TARGET-ROWS."
  (notany #'null
	  (loop
	     :for ref-col-selector in target-cols
	     :for ref-col =  (aref ref-table ref-col-selector)
	     :for test-col across test-table
	     :collect (notany #'null
			      (loop
				 :for i-ref in target-rows
				 :for ref-val = (aref ref-col i-ref)
				 :for i-test from 0
				 :for test-val = (aref test-col i-test)
				 :collect (funcall test ref-val test-val))))))




