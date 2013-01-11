(in-package :numeric-table)

(export '(init-vv-array vvref))

(defun mklist (thing)
  "Ensure that THING is a list.  If it is not, wrap it inside a list."
  (if (listp thing) thing (list thing)))

(defun init-vv-array (columns)
  "Initialize nested vector array

The outer vector is of length COLUMNS

Each of its elements is initialized as an adjustable vector of
dimension 0 and fill-pointer set to 0"
  (let ((vv-array (make-array columns)))
    (dotimes (j columns)
      (setf (grid:aref vv-array j) (make-array 0 :adjustable t :fill-pointer 0)))
    vv-array))


(defun vvref (nested-vector i-row i-col)
  "Return element stored in I-ROW and I-COL of NESTED-VECTOR"
  (grid:aref (grid:aref nested-vector i-col) i-row))

(defun set-vvref (nested-vector i-row i-col value)
  "Set element in I-ROW and I-COL of NESTED-VECTOR to VALUE"
  (setf (grid:aref (grid:aref nested-vector i-col) i-row) value))

(defsetf vvref set-vvref)


(defun vv-table-equal (ref-table test-table
			  &key (test #'equal)
			    (target-rows (loop for i below (length (grid:aref ref-table 0))
					      collect i))
			    (target-cols (loop for i below (length ref-table)
					      collect i)))
  "Return true if elements in REF-TABLE match those in TEST-TABLE

The two tables have the same number of columns, but REF-TABLE may have
fewer rows.  TARGET-ROWS is a list of row indices of REF-TABLE.

The i-th row of TEST-TABLE is tested against the row addressed by
the i-th element of TARGET-ROWS."
  (notany #'null
	  (loop
	     :for ref-col-selector in target-cols
	     :for ref-col =  (grid:aref ref-table ref-col-selector)
	     :for test-col across test-table
	     :collect (notany #'null
			      (loop
				 :for i-ref in target-rows
				 :for ref-val = (grid:aref ref-col i-ref)
				 :for i-test from 0
				 :for test-val = (grid:aref test-col i-test)
				 :collect (funcall test ref-val test-val))))))




(defgeneric make-vector (length column-schema)
  (:documentation 
"Use LENGTH and COLUMN-SCHEMA and return a vector of correct length
and type"))

(defun add-column-schema-short+long-names (short-name long-name
					   &rest short-long-names)
  (labels ((add-short+long (short long)
	     (setf *valid-column-schema*
		   (remove short *valid-column-schema* :key #'car :test #'eq))
	     (push (cons short long) *valid-column-schema*)))
    (add-short+long short-name long-name)
    (loop
       :for sublist :on short-long-names :by #'cddr
       :do (add-short+long (car sublist) (cadr sublist)))))


